;;;; sync.lisp --- The calendars of CalDAV accounts kept in an Org file, both ways; subscriptions read into files of their own.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Each calendar is a top heading of *CALDAV-FILE*, its address the
;;;; CALENDAR property, its events the headings under it. A sync reads that
;;;; file as it stands, sends the entries changed here, fetches what changed
;;;; on the server since the last sync (its sync token), and writes it into
;;;; the file, only the lines that differ, the lines of your own in each entry
;;;; kept. What each calendar last agreed with the server, every event's copy
;;;; and the token, is kept in the state directory, never in the file.
;;;;
;;;; Nothing is deleted unasked: an event deleted on the server leaves its
;;;; entry, tagged removed; an entry deleted here is deleted on the server
;;;; only when asked (CALDAV-DELETED). When an event changed both here
;;;; and on the server, the server's stands in the entry and yours is kept
;;;; beside it, tagged conflict.

(in-package #:caldav)

(defparameter *calendar-property* "CALENDAR"
  "The property of a calendar's heading that holds the calendar's address.")

(defparameter *removed-tag* "removed"
  "The tag of an entry whose event was deleted on the server.")

(defparameter *conflict-tag* "conflict"
  "The tag of your copy of an entry whose event changed on the server too.")

(defparameter *cancel-keywords* '("CANCELLED" "CANCELED")
  "The keywords of an entry cancelled here, as Org's cancelling writes them:
its event is deleted on the server at the next sync.")

;;; Where things are kept

(defun sync-file-path ()
  "The Org file the calendars are kept in, a native path: *CALDAV-FILE*
as written when it is a path, else that name beside the agenda's main file."
  (let ((name *caldav-file*))
    (if (or (find #\/ name) (uiop:string-prefix-p "~" name))
        (native (expand-path name))
        (path-join (parent-path (org-agenda-main-file)) name))))

(defun safe-name (text)
  "TEXT as a name of a file of its own: each character but letters, digits,
dot and hyphen an underscore, at most 100 of them, and a hash of the whole
TEXT after them, so that two texts alike there are two names."
  (let ((name (map 'string (lambda (c) (if (or (alphanumericp c) (find c ".-")) c #\_)) text)))
    (format nil "~a-~36r" (subseq name 0 (min 100 (length name))) (string-hash text))))

(defun calendar-folder (url)
  "The folder in the state directory that keeps what the calendar at URL last agreed with its server."
  (private-state-directory (format nil "calendars/~a/" (safe-name url))))

(defun subscription-path (subscription)
  "The Org file SUBSCRIPTION, a plist of *CALDAV-SUBSCRIPTIONS*, is written into, in the cache."
  (path-join (private-cache-directory "caldav/subscriptions/")
             (format nil "~a.org" (safe-name (getf subscription :name)))))

(defvar *agenda-paths* nil
  "The paths of the calendars' file and the subscriptions' files, worked out
last: (SETTINGS TIME . PATHS), for the settings they were worked out from.")

(defun sync-agenda-files ()
  "The files caldav adds to the agenda: the calendars' file and the
subscriptions' files, those there. The agenda asks often, as often as it is
drawn: where they are is worked out again only when the settings change or
a few seconds have gone."
  (let ((settings (list *caldav-file* *caldav-subscriptions*))
        (kept *agenda-paths*))
    (unless (and kept (equal (first kept) settings) (< (- (get-universal-time) (second kept)) 5))
      (setf kept (list* settings (get-universal-time)
                        (cons (sync-file-path) (mapcar #'subscription-path *caldav-subscriptions*)))
            *agenda-paths* kept))
    (remove-if-not (lambda (path) (ignore-errors (path-kind path))) (cddr kept))))

(pushnew 'sync-agenda-files *org-agenda-file-functions*)

;;; What a calendar last agreed with its server

(defstruct (agreed (:constructor make-agreed (url etag file uid)))
  "An event as its calendar last agreed with the server: its URL, its ETAG,
the FILE its copy is kept in, and its UID."
  url etag file uid)

(defun read-calendar-state (folder)
  "What the calendar kept in FOLDER last agreed with its server: its sync
token (NIL for none), and its events, a list of AGREED."
  (let ((text (ignore-errors (file-text (path-join folder "state") :limit *icalendar-largest*)))
        (token nil) (events '()))
    (when text
      (dolist (line (uiop:split-string text :separator (string #\Newline)))
        (let ((fields (uiop:split-string line :separator (string #\Tab))))
          (cond ((and (string= (first fields) "token") (second fields) (plusp (length (second fields))))
                 (setf token (second fields)))
                ((and (string= (first fields) "event") (= (length fields) 5))
                 (destructuring-bind (url etag file uid) (rest fields)
                   (push (make-agreed url (and (plusp (length etag)) etag) file uid) events)))))))
    (values token (nreverse events))))

(defun field (text)
  "TEXT as a field of the state file: on one line, no tab in it."
  (one-line (or text "")))

(defun write-calendar-state (folder token events)
  "Keep TOKEN and EVENTS, a list of AGREED, as what the calendar in FOLDER agreed with its server."
  (replace-file (path-join folder "state")
                (with-output-to-string (out)
                  (format out "token~c~a~%" #\Tab (field token))
                  (dolist (e events)
                    (format out "event~c~a~c~a~c~a~c~a~%" #\Tab (field (agreed-url e)) #\Tab (field (agreed-etag e))
                            #\Tab (field (agreed-file e)) #\Tab (field (agreed-uid e)))))
                :mode #o600))

(defun agreed-calendar (folder event)
  "The copy of EVENT (an AGREED) kept in FOLDER, read as a VCALENDAR, or NIL."
  (let ((text (ignore-errors (file-text (path-join folder (agreed-file event)) :limit *icalendar-largest*))))
    (and text (first (ignore-errors (icalendar-read text))))))

;;; The Org file read

(defun file-entries (text)
  "The entries of TEXT, the calendars' Org file, as the agenda reads them."
  (org-agenda-file-entries (sync-file-path) text))

(defun calendar-sections (text entries)
  "The calendars of TEXT, whose ENTRIES are given: each a plist, :URL, :ENTRY
\(its heading's), :END (where its headings end in TEXT), :EVENTS (the
entries of its events, the headings right under it) and :BY-UID (a table of
them by their UID)."
  (let ((tops (remove-if-not (lambda (e) (= 1 (agenda-entry-level e))) entries)))
    (loop for (top next) on tops
          for url = (entry-property top *calendar-property*)
          for end = (if next (agenda-entry-position next) (length text))
          when url
            collect (let ((events (remove-if-not (lambda (e) (and (= 2 (agenda-entry-level e))
                                                                  (< (agenda-entry-position top) (agenda-entry-position e) end)))
                                                 entries))
                          (by-uid (make-hash-table :test 'equal)))
                      (dolist (e events)
                        (let ((uid (entry-property e *uid-property*)))
                          (when (and uid (not (gethash uid by-uid))) (setf (gethash uid by-uid) e))))
                      (list :url url :entry top :end end :events events :by-uid by-uid)))))

(defun section-of (sections url)
  "The calendar of SECTIONS whose address is URL, or NIL."
  (find url sections :key (lambda (s) (getf s :url)) :test #'string=))

(defun entry-of-uid (section uid)
  "The entry of SECTION's events whose UID is UID, or NIL."
  (gethash uid (getf section :by-uid)))

;;; Your lines in an entry, kept when the server's version is written

(defun timestamp-line-p (line)
  "True when LINE holds active timestamps alone, as an entry written here holds its times."
  (cl-ppcre:scan "^[ \\t]*<(?:%%\\(.*\\)|\\d{4}-[^>]*)>(?:--<\\d{4}-[^>]*>)?[ \\t]*$" line))

(defun own-lines (own)
  "The lines of OWN, an entry's own text, that are yours rather than its
event's, as three lists: the properties, the logbook's lines other than the
notes of times cancelled and moved, and the rest of its text."
  (let ((lines (uiop:split-string (string-right-trim '(#\Newline) own) :separator (string #\Newline)))
        (props '()) (logbook '()) (body '()) (in nil) (first t))
    (dolist (line lines)
      (let ((trimmed (string-trim '(#\Space #\Tab) line)))
        (cond (first (setf first nil))
              ((eq in :description) (when (cl-ppcre:scan "(?i)^#\\+end_example" trimmed) (setf in nil)))
              ((string-equal trimmed ":PROPERTIES:") (setf in :props))
              ((string-equal trimmed ":LOGBOOK:") (setf in :logbook))
              ((and in (string-equal trimmed ":END:")) (setf in nil))
              ((eq in :props)
               (unless (cl-ppcre:scan (format nil "(?i)^:(~a|LOCATION|~a):" *uid-property* *rule-property*) trimmed)
                 (push line props)))
              ((eq in :logbook)
               (unless (cl-ppcre:scan "^- State \"(CANCELLED|MOVED)\"" trimmed) (push line logbook)))
              ((and (null body) (null props) (null logbook) (org-planning-line-p trimmed)))
              ((timestamp-line-p line))
              ((cl-ppcre:scan (format nil "(?i)^#\\+name:[ \\t]*~a[ \\t]*$" *description-name*) trimmed) (setf in :name))
              ((and (eq in :name) (cl-ppcre:scan "(?i)^#\\+begin_example" trimmed)) (setf in :description))
              (t (setf in nil) (push line body)))))
    (values (nreverse props) (nreverse logbook) (nreverse body))))

(defun with-own-lines (text props logbook body)
  "TEXT, an entry as EVENT-ENTRY writes it, with your PROPS in its property
drawer, your LOGBOOK lines in its logbook, and the rest of your BODY after
its times, before its description."
  (if (and (null props) (null logbook) (null body))
      text
      (let ((lines (uiop:split-string (string-right-trim '(#\Newline) text) :separator (string #\Newline)))
            (out '()) (in nil) (logbook-done nil) (body-done nil))
        (flet ((put (line) (push line out))
               (body-here () (unless body-done (setf body-done t) (dolist (l body) (push l out)))))
          (dolist (line lines)
            (let ((trimmed (string-trim '(#\Space #\Tab) line)))
              (cond ((string-equal trimmed ":PROPERTIES:") (setf in :props) (put line))
                    ((string-equal trimmed ":LOGBOOK:") (setf in :logbook) (put line))
                    ((and (eq in :props) (string-equal trimmed ":END:"))
                     (dolist (p props) (put p)) (put line) (setf in nil)
                     (when (and logbook (not (search (format nil "~%:LOGBOOK:") text)))
                       (put ":LOGBOOK:") (dolist (l logbook) (put l)) (put ":END:") (setf logbook-done t)))
                    ((and (eq in :logbook) (string-equal trimmed ":END:"))
                     (unless logbook-done (dolist (l logbook) (put l)) (setf logbook-done t))
                     (put line) (setf in nil))
                    ((cl-ppcre:scan (format nil "(?i)^#\\+name:[ \\t]*~a" *description-name*) trimmed)
                     (body-here) (put line))
                    (t (put line)))))
          (body-here))
        (format nil "~{~a~%~}" (nreverse out)))))

(defun heading-tagged (heading-line tag)
  "HEADING-LINE, an entry's first line, with TAG among its tags."
  (let ((line (string-right-trim '(#\Space #\Tab #\Return) heading-line)))
    (cond ((cl-ppcre:scan (format nil ":~a:(?:\\S*:)?$" (cl-ppcre:quote-meta-chars tag)) line) line)
          ((cl-ppcre:scan "\\s:[^\\s:]+(?::[^\\s:]+)*:$" line) (format nil "~a~a:" line tag))
          (t (format nil "~a :~a:" line tag)))))

(defun removed-entry (text)
  "TEXT, an entry whose event was deleted on its server, tagged removed, its
times outside the event's description made inactive ([2026-11-05 Thu]
for <2026-11-05 Thu>), so that the agenda shows it no more and your lines
in it are kept."
  (let ((lines (uiop:split-string (string-right-trim '(#\Newline) text) :separator (string #\Newline)))
        (in-description nil))
    (format nil "~{~a~%~}"
            (cons (heading-tagged (first lines) *removed-tag*)
                  (loop for line in (rest lines)
                        for trimmed = (string-trim '(#\Space #\Tab) line)
                        collect (cond (in-description
                                       (when (cl-ppcre:scan "(?i)^#\\+end_example" trimmed) (setf in-description nil))
                                       line)
                                      ((cl-ppcre:scan "(?i)^#\\+begin_example" trimmed)
                                       (setf in-description t)
                                       line)
                                      (t (cl-ppcre:regex-replace-all "<((?:\\d{4}-|%%\\()[^>]*)>" line "[\\1]"))))))))

(defun without-uid (text)
  "TEXT, an entry, without its UID property, its property drawer gone when
nothing is left in it: a copy that is no event's."
  (cl-ppcre:regex-replace-all "(?mi)^[ \\t]*:PROPERTIES:[ \\t]*\\n[ \\t]*:END:[ \\t]*\\n"
                          (cl-ppcre:regex-replace-all (format nil "(?mi)^[ \\t]*:~a:.*\\n" *uid-property*) text "")
                          ""))

(defun with-uid (text uid)
  "TEXT, an entry, with UID as its UID property, in its property drawer, made after its heading and planning when it has none."
  (if (cl-ppcre:scan "(?mi)^[ \\t]*:PROPERTIES:[ \\t]*$" text)
      (cl-ppcre:regex-replace-all "(?mi)^([ \\t]*:PROPERTIES:[ \\t]*\\n)" text (format nil "\\1:~a: ~a~%" *uid-property* (field uid)))
      (let* ((first-end (or (position #\Newline text) (length text)))
             (after (min (length text) (1+ first-end)))
             (planning-end (if (org-planning-line-p text :start after :end (or (position #\Newline text :start after) (length text)))
                               (min (length text) (1+ (or (position #\Newline text :start after) (1- (length text)))))
                               after)))
        (format nil "~a~@[~%~*~]:PROPERTIES:~%:~a: ~a~%:END:~%~a"
                (subseq text 0 planning-end) (and (= planning-end (length text)) (not (org-ends-with-newline-p text)))
                *uid-property* (field uid) (subseq text planning-end)))))

;;; Events compared as the calendar would read them

(defparameter *text-properties* '("SUMMARY" "LOCATION" "DESCRIPTION")
  "The properties of an event compared as the words they say.")

(defun said-lines (components)
  "COMPONENTS, events, as what they say, to compare: each property a line,
DTSTAMP and SEQUENCE left out, words compared as words, sorted."
  (sort (loop for c in components
              nconc (cons (format nil "~a ~a" (ical-component-name c) (or (ical-value-of c "RECURRENCE-ID") ""))
                          (loop for p in (ical-component-properties c)
                                for name = (ical-property-name p)
                                unless (member name '("DTSTAMP" "SEQUENCE" "LAST-MODIFIED" "CREATED") :test #'string-equal)
                                  collect (let ((value (ical-property-value p)))
                                            (cond ((member name *text-properties* :test #'string-equal)
                                                   (let ((words (string-trim '(#\Space #\Newline #\Return #\Tab) (ical-text value))))
                                                     (and (plusp (length words)) (format nil "~a:~a" name (if (string-equal name "DESCRIPTION") words (one-line words))))))
                                                  ((string-equal name "CATEGORIES")
                                                   (format nil "CATEGORIES:~{~a~^,~}" (sort (ical-texts value) #'string<)))
                                                  (t (property-line p)))))))
        #'string< :key (lambda (x) (or x ""))))

(defun same-events-p (a b)
  "True when A and B, lists of events, say the same."
  (equal (remove nil (said-lines a)) (remove nil (said-lines b))))

;;; A sync

(defstruct (calendar-pass (:constructor make-calendar-pass (url name folder)))
  "What one sync did with the calendar at URL, called NAME, whose state is in
FOLDER: INCOMING, the events to write into its entries, each (UID TEXT
CONFLICT), TEXT the entry as EVENT-ENTRY writes it and CONFLICT your copy of
the entry when it changed here too; GONE, the UIDs deleted on the server;
CANCELLED, the UIDs of entries cancelled here whose events were deleted there; NEW-UIDS, the entries sent as new events, each (HEADING UID); TOKEN and
EVENTS, its state once written; SENT and FETCHED, how many; MISSING, how
many events have no entry any more (deleted here); FAILED, what the server
refused, in words."
  url name folder incoming gone cancelled new-uids token events (sent 0) (fetched 0) (missing 0) failed)

(defun cancelled-entry-p (entry)
  "True when ENTRY was cancelled here: its keyword one of *CANCEL-KEYWORDS*."
  (let ((keyword (agenda-entry-todo entry)))
    (and keyword (member keyword *cancel-keywords* :test #'string=) t)))

(defun entry-text (entry)
  "ENTRY's own text."
  (subseq (agenda-entry-text entry) (agenda-entry-position entry) (agenda-entry-end entry)))

(defun entry-heading-line (entry)
  "ENTRY's first line."
  (let ((text (entry-text entry))) (subseq text 0 (or (position #\Newline text) (length text)))))

(defun event-url (calendar-url uid)
  "The address a new event of UID is put at in the calendar at CALENDAR-URL."
  (format nil "~a~:[/~;~]~a.ics" calendar-url (uiop:string-suffix-p calendar-url "/") (safe-name uid)))

(defparameter *most-fetched* 2000
  "The most events of one calendar a sync fetches; the next sync fetches the rest.")

(defun sync-calendar (account calendar section &key (zone (system-time-zone)) (now (get-universal-time)))
  "Sync CALENDAR, a plist of ACCOUNT-CALENDARS, with SECTION, its heading in the
calendars' file read when the sync began (or NIL): send what changed here,
fetch what changed there, and write each event fetched as its entry. A
CALENDAR-PASS of what to write into the file. An entry the server refuses
is noted in the pass and the others go on."
  (let* ((url (getf calendar :url))
         (folder (calendar-folder url))
         (pass (make-calendar-pass url (getf calendar :name) folder)))
    (multiple-value-bind (token agreed) (read-calendar-state folder)
      (let ((by-url (make-hash-table :test 'equal)) (by-uid (make-hash-table :test 'equal)))
        (dolist (a agreed) (setf (gethash (agreed-url a) by-url) a (gethash (agreed-uid a) by-uid) a))
        (flet ((keep (url etag text uid)
                 ;; what the server has now for URL, as agreed
                 (let* ((was (gethash url by-url))
                        (file (if was (agreed-file was) (format nil "~a.ics" (random-name)))))
                   (replace-file (path-join folder file) text :mode #o600)
                   (let ((a (make-agreed url etag file uid)))
                     (setf (gethash url by-url) a (gethash uid by-uid) a))))
               (forget (url)
                 (let ((was (gethash url by-url)))
                   (when was
                     (remhash url by-url) (remhash (agreed-uid was) by-uid)
                     (ignore-errors (remove-file (path-join folder (agreed-file was)))))))
               (refused (entry what status)
                 (push (format nil "~a refused ~a ~a (~a)" (getf calendar :name) what (agenda-entry-title entry) status)
                       (calendar-pass-failed pass))))
          ;; what changed there since the last sync
          (multiple-value-bind (changed gone new-token) (calendar-changes account calendar token)
            (when (eq new-token :none)
              (let ((now-there (calendar-etags account calendar)))
                (setf changed now-there
                      gone (loop for a in agreed unless (assoc (agreed-url a) now-there :test #'string=) collect (agreed-url a))
                      new-token nil)))
            ;; asked without a token, the answer lists every event there: those agreed and not in it are gone
            (when (and (null token) (null gone))
              (setf gone (loop for a in agreed unless (assoc (agreed-url a) changed :test #'string=) collect (agreed-url a))))
            (setf changed (remove-if (lambda (c) (let ((a (gethash (car c) by-url))) (and a (equal (agreed-etag a) (cdr c))))) changed))
            ;; a sync fetches so many at most: the token stays, and the next sync fetches the rest
            (when (> (length changed) *most-fetched*)
              (setf changed (subseq changed 0 *most-fetched*)
                    gone '()
                    new-token token))
            (job-check-stop)
            ;; what changed here, sent unless it changed there too
            (let ((changed-urls (mapcar #'car changed)) (conflicts (make-hash-table :test 'equal)))
              (dolist (entry (and section (getf section :events)))
                (let* ((uid (entry-property entry *uid-property*))
                       (local-tags (agenda-entry-local-tags entry))
                       (a (and uid (gethash uid by-uid))))
                  (unless (or (member *removed-tag* local-tags :test #'string=) (member *conflict-tag* local-tags :test #'string=)
                              ;; cancelled before it was ever sent: nothing to send
                              (and (null a) (cancelled-entry-p entry)))
                    (job-check-stop)
                    (cond
                      ((and a (cancelled-entry-p entry))
                       ;; cancelled here: its event deleted there, unless it changed there too
                       (if (member (agreed-url a) changed-urls :test #'string=)
                           (setf (gethash uid conflicts) (entry-text entry))
                           (multiple-value-bind (outcome status) (delete-event account (agreed-url a) (agreed-etag a))
                             (case outcome
                               (:done (push uid (calendar-pass-cancelled pass))
                                (forget (agreed-url a))
                                (incf (calendar-pass-sent pass)))
                               (:changed (setf (gethash uid conflicts) (entry-text entry))
                                (push (agreed-url a) changed-urls))
                               (t (refused entry "the deletion of" status))))))
                      ((null a)
                        ;; a new entry: a new event, when it says a time
                        (let* ((new (or uid (new-uid)))
                               (components (entry-components entry :uid new :zone zone :now now))
                               (target (event-url url new)))
                          (when components
                            (let ((text (icalendar-write (components-calendar components nil))))
                              (multiple-value-bind (outcome etag) (put-event account target text nil)
                                (case outcome
                                  (:done (keep target etag text new)
                                   (unless uid (push (list (entry-heading-line entry) new) (calendar-pass-new-uids pass)))
                                   (incf (calendar-pass-sent pass)))
                                  (:changed nil)
                                  (t (refused entry "the new event" etag))))))))
                      (t
                        (let* ((was (agreed-calendar folder a))
                               (now-components (entry-components entry :agreed was :zone zone :now now)))
                          ;; an entry saying no event at all sends nothing: the calendar's stands
                          (unless (or (null now-components) (same-events-p now-components (and was (calendar-events was))))
                            (if (member (agreed-url a) changed-urls :test #'string=)
                                (setf (gethash uid conflicts) (entry-text entry))
                                (let ((text (icalendar-write (components-calendar now-components was))))
                                  (multiple-value-bind (outcome etag) (put-event account (agreed-url a) text (agreed-etag a))
                                    (case outcome
                                      (:done (keep (agreed-url a) etag text uid)
                                       (incf (calendar-pass-sent pass)))
                                      (:changed (setf (gethash uid conflicts) (entry-text entry))
                                       (push (agreed-url a) changed-urls))
                                      (t (refused entry "the change of" etag)))))))))))))
              ;; what changed there, fetched and written as entries here, in the background
              (dolist (event (fetch-events account calendar (remove-duplicates changed-urls :test #'string=)))
                (job-check-stop)
                (destructuring-bind (event-url etag text) event
                  (let* ((read (ignore-errors (first (icalendar-read text))))
                         (members (and read (first (uid-groups read)))))
                    (when members
                      (let* ((uid (or (ical-value-of (first members) "UID") ""))
                             (mine (gethash uid conflicts))
                             (entry (and section (entry-of-uid section uid))))
                        ;; a conflict where both now say the same is none
                        (when (and mine entry (same-events-p (entry-components entry :agreed read :zone zone :now now)
                                                             (calendar-events read)))
                          (setf mine nil))
                        (keep event-url etag text uid)
                        (push (list uid (event-entry members read :level 2 :zone zone :now now) mine)
                              (calendar-pass-incoming pass))
                        (incf (calendar-pass-fetched pass)))))))
              ;; deleted there
              (dolist (gone-url gone)
                (let ((a (gethash gone-url by-url)))
                  (when a
                    (push (agreed-uid a) (calendar-pass-gone pass))
                    (forget gone-url))))
              (setf (calendar-pass-token pass) new-token
                    (calendar-pass-events pass) (loop for a being the hash-values of by-url collect a)
                    ;; deleted here: agreed, but no entry and nothing coming to make one
                    (calendar-pass-missing pass)
                    (count-if (lambda (a) (and section (not (entry-of-uid section (agreed-uid a)))
                                               (not (find (agreed-uid a) (calendar-pass-incoming pass) :key #'first :test #'equal))))
                              (calendar-pass-events pass))))))))
    pass))

;;; Written into the file

(defun section-heading (pass)
  "The heading of the calendar PASS synced, for a file that has none yet."
  (format nil "* ~a~%:PROPERTIES:~%:~a: ~a~%:END:~%" (heading-words (calendar-pass-name pass)) *calendar-property* (agenda-guarded (field (calendar-pass-url pass)))))

(defun on-new-line (text at)
  "A line break when the place AT of TEXT is not at a line's start, else nothing."
  (if (and (plusp at) (char/= (char text (1- at)) #\Newline)) (string #\Newline) ""))

(defun merged-text (text passes)
  "TEXT, the calendars' file as it stands, with what PASSES fetched written
in: each event's entry replaced, your own lines in it kept, or added under
its calendar; the entries of events deleted there deleted, or with lines of
yours in them kept as REMOVED-ENTRY says; the entries cancelled here whose
events were deleted there tagged removed; your copy
of an entry that changed on both sides beside it, tagged conflict; new
events' UIDs in their entries; a heading for each calendar that has none.
The entries' texts are made by the sync; here they are only put in place."
  (let* ((entries (file-entries text))
         (sections (calendar-sections text entries))
         (edits '()) (appended '()))
    (flet ((edit (start end replacement) (push (list start end replacement) edits)))
      (dolist (pass passes)
        (let ((section (section-of sections (calendar-pass-url pass)))
              (added '()))
          (dolist (in (reverse (calendar-pass-incoming pass)))
            (destructuring-bind (uid written mine) in
              (let ((entry (and section (entry-of-uid section uid))))
                (if entry
                    (multiple-value-bind (props logbook body) (own-lines (entry-text entry))
                      (edit (agenda-entry-position entry) (agenda-entry-end entry)
                            (concatenate 'string (with-own-lines written props logbook body)
                                         (if mine
                                             (let* ((copy (without-uid mine))
                                                    (line-end (or (position #\Newline copy) (length copy))))
                                               (format nil "~a~a~:[~%~;~]" (heading-tagged (subseq copy 0 line-end) *conflict-tag*)
                                                       (subseq copy line-end) (org-ends-with-newline-p copy)))
                                             ""))))
                    (push written added)))))
          (dolist (uid (calendar-pass-gone pass))
            (let ((entry (and section (entry-of-uid section uid))))
              (when entry
                (multiple-value-bind (props logbook body) (own-lines (entry-text entry))
                  (edit (agenda-entry-position entry) (agenda-entry-end entry)
                        (if (or props logbook body) (removed-entry (entry-text entry)) ""))))))
          ;; cancelled here and deleted there: tagged, its times left, so that the agenda shows it cancelled
          (dolist (uid (calendar-pass-cancelled pass))
            (let ((entry (and section (entry-of-uid section uid))))
              (when entry
                (let ((line (entry-heading-line entry)))
                  (edit (agenda-entry-position entry) (+ (agenda-entry-position entry) (length line))
                        (heading-tagged line *removed-tag*))))))
          (dolist (new (calendar-pass-new-uids pass))
            (destructuring-bind (heading uid) new
              (let ((entry (and section (find-if (lambda (e) (and (null (entry-property e *uid-property*))
                                                                   (string= (entry-heading-line e) heading)))
                                                 (getf section :events)))))
                (when entry
                  (edit (agenda-entry-position entry) (agenda-entry-end entry) (with-uid (entry-text entry) uid))))))
          (when added
            (let ((events (format nil "~{~a~}" (nreverse added))))
              (if section
                  (edit (getf section :end) (getf section :end) (concatenate 'string (on-new-line text (getf section :end)) events))
                  (push (concatenate 'string (section-heading pass) events) appended)))))))
    ;; one pass over the text, the edits in their order
    (with-output-to-string (out)
      (let ((at 0))
        (dolist (e (stable-sort (nreverse edits) #'< :key #'first))
          (destructuring-bind (start end replacement) e
            (write-string text out :start at :end start)
            (write-string replacement out)
            (setf at end)))
        (write-string text out :start at)
        (when appended
          (write-string (on-new-line text (length text)) out)
          (dolist (a (nreverse appended)) (write-string a out)))))))

;;; Subscriptions

(defun fetch-subscription (subscription &key (zone (system-time-zone)) (now (get-universal-time)))
  "Fetch SUBSCRIPTION, a calendar file at an address, and write it as its Org
file in the cache (SUBSCRIPTION-PATH), made again each time; how many events it holds."
  (multiple-value-bind (status headers text) (request '() "GET" (getf subscription :url))
    (declare (ignore headers))
    (unless (ok-status-p status) (caldav-fail "~a answered ~a" (getf subscription :url) status))
    (let* ((count 0)
           (entries (with-output-to-string (out)
                     (dolist (c (icalendar-read text))
                       (multiple-value-bind (written n) (calendar-entries c :level 2 :zone zone :now now)
                         (write-string written out)
                         (incf count n))))))
      (replace-file (subscription-path subscription)
                    (format nil "#+title: ~a~%# Made by caldav from ~a: written again at each sync, so changed here only until then.~%* ~a~%~a"
                            (heading-words (getf subscription :name)) (field (getf subscription :url))
                            (heading-words (getf subscription :name)) entries))
      count)))
