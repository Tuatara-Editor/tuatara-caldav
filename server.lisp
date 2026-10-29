;;;; server.lisp --- Calendars on a CalDAV server: found, their changes fetched, events sent and deleted.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; RFC 4791 (CalDAV), RFC 6578 (changes since a sync token), RFC 6764 (the
;;;; calendars found from the server's address). Every request goes through
;;;; curl, the password given on its standard input, never on its command
;;;; line; https, or http to this machine alone (etesync-dav). Redirects are
;;;; followed here, each address checked again. What a server answers is
;;;; untrusted: its size and the time it takes are bounded, and its XML read
;;;; as data.

(in-package #:caldav)

(defparameter *request-seconds* 60
  "The most seconds one request to a server may take.")

(defparameter *largest-answer* (* 32 1024 1024)
  "The most octets a server's answer to one request may hold.")

(defparameter *most-redirects* 5
  "The most redirects one request follows.")

(defparameter *multiget-size* 100
  "How many events one request fetches at most.")

(define-condition caldav-error (error)
  ((text :initarg :text :reader caldav-error-text))
  (:report (lambda (condition stream) (write-string (caldav-error-text condition) stream)))
  (:documentation "A server not reached, refusing, or answering what is no CalDAV: what went wrong, in words."))

(setf (documentation 'caldav-error-text 'function) "What went wrong with a server, in words.")

(defun caldav-fail (control &rest arguments)
  "Signal a CALDAV-ERROR saying CONTROL with ARGUMENTS, as FORMAT says them."
  (error 'caldav-error :text (apply #'format nil control arguments)))

;;; Addresses

(defun url-parts (url)
  "URL's scheme (http or https, in small letters), its host with its port, and
the rest (its path, at least /), as three values; NIL when it is no web address."
  (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "^(?i)(https?)://([^/?#\\s]+)([^\\s]*)$" url)
    (and match (values (string-downcase (aref groups 0)) (aref groups 1)
                       (if (plusp (length (aref groups 2))) (aref groups 2) "/")))))

(defun own-machine-p (host)
  "True when HOST, a host and maybe its port, names this machine."
  (let ((name (string-downcase (cl-ppcre:regex-replace-all "(?<=[^:\\]]):\\d+$|(?<=\\]):\\d+$" host ""))))
    (member name '("localhost" "127.0.0.1" "[::1]") :test #'string=)))

(defun url-allowed-p (url)
  "True when URL may be reached: https, or http to this machine alone."
  (multiple-value-bind (scheme host) (url-parts url)
    (and scheme (or (string= scheme "https") (own-machine-p host)))))

(defun url-joined (base href)
  "HREF, an address a server gave in answer to BASE, made whole: as it is when
it has a scheme, on BASE's host when it begins with /, else beside BASE's path."
  (cond ((url-parts href) href)
        (t (multiple-value-bind (scheme host path) (url-parts base)
             (if (uiop:string-prefix-p "/" href)
                 (format nil "~a://~a~a" scheme host href)
                 (format nil "~a://~a~a~a" scheme host (subseq path 0 (1+ (or (position #\/ path :from-end t) -1))) href))))))

(defun url-path (url)
  "URL's path, as a server names its resources."
  (nth-value 2 (url-parts url)))

;;; Requests

(defvar *transport* 'curl-request
  "The function a request to a server is made with: of a method, an address
and :HEADERS, :BODY, :USER and :PASSWORD, giving the answer's status, its
headers (an alist, names in small letters) and its body, three values.")

(defun curl-quoted (text)
  "TEXT as curl's configuration writes a quoted value: a backslash before each \\ and \"."
  (format nil "\"~a\"" (cl-ppcre:regex-replace-all "([\\\\\"])" text "\\\\\\1")))

(defun curl-headers (text)
  "The headers of the last answer in TEXT, curl's record of them: an alist,
names in small letters."
  (let* ((plain (string-right-trim '(#\Newline) (remove #\Return text)))
         (gap (search (format nil "~%~%") plain :from-end t))
         (last (if gap (subseq plain (+ gap 2)) plain)))
    (loop for line in (rest (uiop:split-string last :separator (string #\Newline)))
          for colon = (position #\: line)
          when colon collect (cons (string-downcase (string-trim " " (subseq line 0 colon)))
                                   (string-trim '(#\Space #\Tab #\Return) (subseq line (1+ colon)))))))

(defun curl-request (method url &key headers body user password)
  "Make the request METHOD to URL with curl, HEADERS (an alist) and BODY (text,
or NIL) with it, signed in as USER with PASSWORD, given on curl's standard
input: three values, the answer's status, its headers and its body."
  (let ((curl (find-program "curl")))
    (unless curl (caldav-fail "curl is not installed: syncing calendars needs it"))
    ;; a line break would end curl's line of the name and password and begin another
    (when (find-if (lambda (c) (member c '(#\Newline #\Return #\Nul))) (concatenate 'string (or user "") (or password "")))
      (caldav-fail "The name or password for ~a holds a line break: not sent" url))
    (let ((folder (new-private-folder (private-cache-directory "caldav/requests/") :hold nil)))
      (unwind-protect
           (let ((body-file (path-join folder "body")) (head-file (path-join folder "head")) (out-file (path-join folder "out")))
             (when body (replace-file body-file body :mode #o600))
             (multiple-value-bind (said ok)
                 (program-output (append (list curl "--disable" "--silent" "--show-error" "--proto" "=https,http"
                                               "--max-time" (princ-to-string *request-seconds*)
                                               "--max-filesize" (princ-to-string *largest-answer*)
                                               "--request" method "--dump-header" head-file "--output" out-file
                                               "--write-out" (format nil "%{http_code}~%%{errormsg}") "--config" "-")
                                         (loop for (name . value) in headers append (list "--header" (format nil "~a: ~a" name (one-line value))))
                                         (and body (list "--data-binary" (concatenate 'string "@" body-file)))
                                         (list "--" url))
                                 :input (if user (format nil "user = ~a~%" (curl-quoted (format nil "~a:~a" user (or password "")))) "")
                                 :seconds (+ 10 *request-seconds*))
               ;; what curl wrote out: the answer's status, and on a line of its own why it failed
               (let* ((lines (uiop:split-string (or said "") :separator (string #\Newline)))
                      (status (ignore-errors (parse-integer (string-trim " " (first lines)))))
                      (why (one-line (format nil "~{~a~^ ~}" (rest lines)))))
                 (unless (and ok status (plusp status))
                   (caldav-fail "~a not reached: ~a" url
                                (cond ((plusp (length why)) why)
                                      (said "no answer")
                                      (t (format nil "curl gave no answer in ~d seconds" (+ 10 *request-seconds*))))))
                 (values status
                         (curl-headers (or (ignore-errors (file-text head-file :limit *largest-answer*)) ""))
                         (if (path-kind out-file)
                             (handler-case (octets-string (file-octets out-file :limit *largest-answer*))
                               (file-system-error () (caldav-fail "~a answered more than ~:d octets" url *largest-answer*)))
                             "")))))
        (ignore-errors (delete-tree folder))))))

(defun request (account method url &key headers body (redirects *most-redirects*))
  "Make the request METHOD to URL for ACCOUNT, as *TRANSPORT* does, following
redirects to addresses that may be reached, and with ACCOUNT's password
only to its own server: three values, the answer's
status, its headers and its body, and the address answering as a fourth."
  (unless (url-allowed-p url)
    (caldav-fail "~a is not https, nor on this machine: not reached" url))
  ;; an account's password goes to its own server alone, whatever an answer names
  (when (and (getf account :user) (getf account :url)
             (not (equalp (nth-value 1 (url-parts url)) (nth-value 1 (url-parts (getf account :url))))))
    (caldav-fail "~a is not on the server of ~a: not reached with its password" url (getf account :name)))
  (multiple-value-bind (status answer-headers answer)
      (funcall *transport* method url :headers headers :body body
                                      :user (getf account :user) :password (getf account :password))
    (let ((location (cdr (assoc "location" answer-headers :test #'string=))))
      (if (and (member status '(301 302 303 307 308)) location)
          (if (plusp redirects)
              (request account (if (= status 303) "GET" method) (url-joined url location)
                       :headers headers :body (and (/= status 303) body) :redirects (1- redirects))
              (caldav-fail "~a redirects too many times" url))
          (values status answer-headers answer url)))))

(defun ok-status-p (status)
  "True when STATUS says a request went well."
  (and status (<= 200 status 299)))

;;; Answers

(defun element-text (element)
  "The text of the XML ELEMENT and all under it, blanks about it left out."
  (string-trim '(#\Space #\Newline #\Tab #\Return) (xml-all-text element)))

(defun multistatus-responses (text)
  "The responses of TEXT, a server's multistatus answer, each a plist: :HREF,
:STATUS (the response's own, a number, or NIL), and :PROPS, an alist of each
property found (its local name) and its element, those of a propstat whose
status is not 2xx left out; and the answer's root element as a second value."
  (let* ((tree (ignore-errors (xml-parse text)))
         (root (and tree (xml-root tree))))
    (unless (and root (equal (xml-local-name root) "multistatus"))
      (caldav-fail "the server's answer is no CalDAV multistatus"))
    (values
     (loop for response in (xml-local-children root "response")
          collect (let ((href (first (xml-local-children response "href")))
                        (status (first (xml-local-children response "status"))))
                    (list :href (and href (element-text href))
                          :status (and status (status-number (xml-all-text status)))
                          :props (loop for propstat in (xml-local-children response "propstat")
                                       for code = (let ((s (first (xml-local-children propstat "status")))) (and s (status-number (xml-all-text s))))
                                       when (ok-status-p code)
                                         nconc (loop for prop in (xml-local-children propstat "prop")
                                                     nconc (loop for child in (xml-children prop)
                                                                 when (consp child) collect (cons (xml-local-name child) child)))))))
     root)))

(defun status-number (line)
  "The number of LINE, an HTTP status line (HTTP/1.1 200 OK), or NIL."
  (let ((match (nth-value 1 (cl-ppcre:scan-to-strings "\\s(\\d{3})(\\s|$)" (concatenate 'string " " line)))))
    (and match (parse-integer (aref match 0)))))

(defun prop (response name)
  "The element of RESPONSE's property NAME (its local name), or NIL."
  (cdr (assoc name (getf response :props) :test #'string=)))

(defun prop-text (response name)
  "The text of RESPONSE's property NAME, blanks about it left out, or NIL."
  (let ((element (prop response name)))
    (and element (element-text element))))

(defun prop-href (response name)
  "The address inside RESPONSE's property NAME (current-user-principal, calendar-home-set), or NIL."
  (let* ((element (prop response name))
         (href (and element (first (xml-find-all element "href" :local t)))))
    (and href (element-text href))))

;;; Finding the calendars

(defparameter *propfind-principal*
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<d:propfind xmlns:d=\"DAV:\"><d:prop><d:current-user-principal/></d:prop></d:propfind>"
  "The request for whom the account signs in as.")

(defparameter *propfind-home*
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<d:propfind xmlns:d=\"DAV:\" xmlns:c=\"urn:ietf:params:xml:ns:caldav\"><d:prop><c:calendar-home-set/></d:prop></d:propfind>"
  "The request for where an account's calendars are.")

(defparameter *propfind-calendars*
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<d:propfind xmlns:d=\"DAV:\" xmlns:c=\"urn:ietf:params:xml:ns:caldav\" xmlns:cs=\"http://calendarserver.org/ns/\">
<d:prop><d:resourcetype/><d:displayname/><c:supported-calendar-component-set/><cs:getctag/><d:sync-token/></d:prop></d:propfind>"
  "The request for the calendars in a folder, their names and what they hold.")

(defun propfind (account url body depth)
  "The responses to a PROPFIND of BODY at URL, DEPTH deep (0 or 1), for
ACCOUNT, and the address that answered."
  (multiple-value-bind (status headers answer at)
      (request account "PROPFIND" url :headers `(("Depth" . ,(princ-to-string depth))
                                                 ("Content-Type" . "application/xml; charset=utf-8"))
                                      :body body)
    (declare (ignore headers))
    (cond ((eql status 401) (caldav-fail "~a refused the name and password" url))
          ((not (eql status 207)) (caldav-fail "~a answered ~a, not as a CalDAV server" url status)))
    (values (multistatus-responses answer) at)))

(defun account-home (account)
  "The address of ACCOUNT's calendars, as its server says (RFC 6764): its
principal from the account's address, or from /.well-known/caldav, then the
principal's calendar home."
  (let ((url (getf account :url)))
    (flet ((principal (from)
             (multiple-value-bind (responses at) (ignore-errors (propfind account from *propfind-principal* 0))
               (let ((href (and responses (prop-href (first responses) "current-user-principal"))))
                 (and href (url-joined at href))))))
      (let ((principal (or (principal url)
                           (multiple-value-bind (scheme host) (url-parts url)
                             (principal (format nil "~a://~a/.well-known/caldav" scheme host))))))
        (unless principal
          ;; to say why: the account's own address asked again, its error let through
          (propfind account url *propfind-principal* 0)
          (caldav-fail "~a says no principal for ~a" url (getf account :user)))
        (multiple-value-bind (responses at) (propfind account principal *propfind-home* 0)
          (let ((home (and responses (prop-href (first responses) "calendar-home-set"))))
            (unless home (caldav-fail "~a says no calendars for ~a" principal (getf account :user)))
            (url-joined at home)))))))

(defun account-calendars (account)
  "ACCOUNT's calendars of events or tasks, each a plist: :URL, :NAME (its
name, or the last part of its address), :CTAG and :SYNC-TOKEN as the server
gave them."
  (multiple-value-bind (responses at) (propfind account (account-home account) *propfind-calendars* 1)
    (loop for response in responses
          for type = (prop response "resourcetype")
          for components = (let ((set (prop response "supported-calendar-component-set")))
                             (and set (loop for c in (xml-find-all set "comp" :local t)
                                            collect (string-upcase (or (xml-attribute c "name") "")))))
          when (and type (xml-find-all type "calendar" :local t)
                    (or (null components) (intersection components '("VEVENT" "VTODO") :test #'string=)))
            collect (let* ((url (url-joined at (getf response :href)))
                           (path (string-right-trim "/" (url-path url))))
                      (list :url url
                            :name (or (let ((name (prop-text response "displayname"))) (and name (plusp (length name)) (one-line name)))
                                      (subseq path (1+ (or (position #\/ path :from-end t) -1))))
                            :ctag (prop-text response "getctag")
                            :sync-token (prop-text response "sync-token"))))))

;;; What changed

(defun sync-collection-body (token)
  "The request for what changed in a calendar since TOKEN (NIL for all), RFC 6578."
  (format nil "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<d:sync-collection xmlns:d=\"DAV:\"><d:sync-token>~@[~a~]</d:sync-token><d:sync-level>1</d:sync-level><d:prop><d:getetag/></d:prop></d:sync-collection>"
          (and token (xml-escape token))))

(defun calendar-changes (account calendar token)
  "What changed in CALENDAR (a plist of ACCOUNT-CALENDARS) since TOKEN, a sync
token, NIL for everything: three values, the events there now or changed,
each (ADDRESS . ETAG), the addresses of events gone, and the new token. NIL
for the first two and :NONE for the token when the server keeps no tokens or
refuses this one; NIL for the token when the server gives an empty one."
  (multiple-value-bind (status headers answer)
      (request account "REPORT" (getf calendar :url)
               :headers '(("Depth" . "1") ("Content-Type" . "application/xml; charset=utf-8"))
               :body (sync-collection-body token))
    (declare (ignore headers))
    (if (eql status 207)
        (multiple-value-bind (responses root) (multistatus-responses answer)
         (let ((new-token (let* ((element (first (xml-local-children root "sync-token")))
                                 (text (and element (string-trim '(#\Space #\Tab #\Newline #\Return) (element-text element)))))
                            (cond ((null element) :none)
                                  ((plusp (length text)) text))))
               (changed '()) (gone '()))
          (dolist (response responses)
            (let ((url (url-joined (getf calendar :url) (getf response :href))))
              (cond ((eql (getf response :status) 404) (push url gone))
                    ((prop-text response "getetag") (push (cons url (prop-text response "getetag")) changed)))))
          (values (nreverse changed) (nreverse gone) new-token)))
        (values nil nil :none))))

(defun calendar-etags (account calendar)
  "Every event of CALENDAR now, each (ADDRESS . ETAG), by a PROPFIND: for a
server that keeps no sync tokens."
  (loop for response in (propfind account (getf calendar :url)
                                  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<d:propfind xmlns:d=\"DAV:\"><d:prop><d:getetag/><d:resourcetype/></d:prop></d:propfind>" 1)
        for url = (url-joined (getf calendar :url) (getf response :href))
        for etag = (prop-text response "getetag")
        when (and etag (not (string= (string-right-trim "/" (url-path url)) (string-right-trim "/" (url-path (getf calendar :url))))))
          collect (cons url etag)))

(defun multiget-body (urls)
  "The request for the calendar data of the events at URLS."
  (format nil "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<c:calendar-multiget xmlns:d=\"DAV:\" xmlns:c=\"urn:ietf:params:xml:ns:caldav\"><d:prop><d:getetag/><c:calendar-data/></d:prop>~{<d:href>~a</d:href>~}</c:calendar-multiget>"
          (mapcar (lambda (url) (xml-escape (url-path url))) urls)))

(defun fetch-events (account calendar urls)
  "The events at URLS in CALENDAR, each (ADDRESS ETAG TEXT), TEXT the calendar
file as the server keeps it: fetched *MULTIGET-SIZE* at a time, or one by
one from a server that will not."
  (loop for rest on urls by (lambda (l) (nthcdr *multiget-size* l))
        nconc (let ((batch (subseq rest 0 (min *multiget-size* (length rest)))))
                (multiple-value-bind (status headers answer)
                    (request account "REPORT" (getf calendar :url)
                             :headers '(("Depth" . "1") ("Content-Type" . "application/xml; charset=utf-8"))
                             :body (multiget-body batch))
                  (declare (ignore headers))
                  (if (eql status 207)
                      (loop for response in (multistatus-responses answer)
                            for data = (prop-text response "calendar-data")
                            when data collect (list (url-joined (getf calendar :url) (getf response :href))
                                                    (prop-text response "getetag") data))
                      (loop for url in batch
                            nconc (multiple-value-bind (status headers answer) (request account "GET" url)
                                    (and (ok-status-p status)
                                         (list (list url (cdr (assoc "etag" headers :test #'string=)) answer))))))))))

;;; Sending

(defun put-event (account url text etag)
  "Put TEXT, a calendar file of one event, at URL for ACCOUNT: in place of the
copy ETAG names, or as a new event when ETAG is NIL. Two values: :DONE and the
new ETAG (NIL when the server says none), :CHANGED when the server's copy
changed since, or :REFUSED and the status."
  (multiple-value-bind (status headers)
      (request account "PUT" url
               :headers (append '(("Content-Type" . "text/calendar; charset=utf-8"))
                                (if etag `(("If-Match" . ,etag)) '(("If-None-Match" . "*"))))
               :body text)
    (cond ((ok-status-p status) (values :done (cdr (assoc "etag" headers :test #'string=))))
          ((eql status 412) (values :changed nil))
          (t (values :refused status)))))

(defun delete-event (account url etag)
  "Delete the event at URL for ACCOUNT, if its copy is still the one ETAG
names: :DONE (gone already too), :CHANGED, or :REFUSED and the status."
  (let ((status (request account "DELETE" url :headers (and etag `(("If-Match" . ,etag))))))
    (cond ((or (ok-status-p status) (eql status 404)) :done)
          ((eql status 412) :changed)
          (t (values :refused status)))))

;;; The password

(defun account-password (account)
  "ACCOUNT's password from the desktop's keyring (secret-tool), kept there by
the label service tuatara-caldav and user its :USER; NIL when there is none."
  (let ((tool (find-program "secret-tool")))
    (unless tool (caldav-fail "secret-tool is not installed: the password of ~a is kept by it" (getf account :name)))
    (multiple-value-bind (said ok)
        (program-output (list tool "lookup" "service" "tuatara-caldav" "user" (getf account :user)) :seconds 30)
      (and ok said (plusp (length said)) (string-right-trim '(#\Newline #\Return) said)))))
