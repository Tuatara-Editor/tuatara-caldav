;;;; commands.lisp --- The commands: sync the calendars, delete what was deleted here, import and export calendar files.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:caldav)

;;; The calendars' file, changed while it may be open

(defun calendars-buffer ()
  "The buffer of the calendars' file, read when it is not open, never shown."
  (visit-file (sync-file-path)))

(defun put-into-calendars (change)
  "Make the calendars' file what CHANGE, a function of its text, gives, only
the lines that differ changed, one change to undo, and save it. True when it
changed."
  (let* ((buffer (calendars-buffer))
         (text (buffer-string buffer))
         (new (funcall change text)))
    (check-writable buffer)
    (when (string/= new text)
      (put-text-by-lines buffer (buffer-text buffer) new)
      (save-buffer-with-care buffer)
      t)))

(defun account-of (url)
  "The account of *CALDAV-ACCOUNTS* whose server holds the calendar at
URL: the one on its host, else the first."
  (let ((host (nth-value 1 (url-parts url))))
    (or (find-if (lambda (a) (equal (nth-value 1 (url-parts (getf a :url))) host)) *caldav-accounts*)
        (first *caldav-accounts*))))

(defparameter *password-command-seconds* 120
  "How long a password command may take: long enough to type a passphrase in
the window gpg-agent opens for it.")

(defun command-password (account)
  "The password ACCOUNT's :PASSWORD-COMMAND prints, its first line: the
command split into words as a shell splits it, though no shell runs, a word
beginning with ~/ in your home folder. A CALDAV-ERROR when it fails; what it
prints is never said."
  (multiple-value-bind (words ok why) (command-words (getf account :password-command))
    (unless (and ok words)
      (caldav-fail "The password command of ~a is no command~@[: ~a~]" (getf account :name) why))
    (let* ((words (mapcar (lambda (w) (if (uiop:string-prefix-p "~/" w) (native (expand-path w)) w)) words))
           (program (find-program (first words))))
      (unless program
        (caldav-fail "The password command of ~a: ~a is not installed" (getf account :name) (first words)))
      (multiple-value-bind (said ok) (program-output (cons program (rest words)) :seconds *password-command-seconds*)
        (let ((password (and said (string-right-trim '(#\Return) (subseq said 0 (or (position #\Newline said) (length said)))))))
          (unless (and ok password (plusp (length password)))
            (caldav-fail "The password command of ~a gave no password: ~a ~:[failed~;printed nothing~]"
                         (getf account :name) (first words) ok))
          password)))))

(defun signed-in (account)
  "ACCOUNT with its password: what its :PASSWORD-COMMAND prints when it has
one, else the keyring's; or a CALDAV-ERROR saying how to give it."
  (let ((password (if (getf account :password-command)
                      (command-password account)
                      (account-password account))))
    (unless password
      (caldav-fail "No password for ~a in the keyring: secret-tool store --label=\"Tuatara calendars\" service tuatara-caldav user ~a, or a :password-command"
                   (getf account :name) (getf account :user)))
    (list* :password password account)))

;;; A sync

(defstruct (caldav-job (:include job) (:constructor make-caldav-job (buffer quiet)) (:copier nil))
  "Syncing the calendars of *CALDAV-ACCOUNTS* and fetching
*CALDAV-SUBSCRIPTIONS*: PASSES, what each calendar's sync did;
SUBSCRIBED, how many events each subscription holds, (NAME . COUNT);
FAILED, what went wrong, in words, by account or subscription; QUIET, true
for a sync started on its own, which says nothing when nothing changed."
  passes subscribed failed quiet)

(setf (documentation 'caldav-job-p 'function) "True when OBJECT is a CALDAV-JOB.")

(define-job-words caldav-job
  :doing "Syncing the calendars"
  :description "Sending what changed here to the calendars' servers and fetching what changed there"
  :verb "sync the calendars"
  :left-words "nothing written")

(defmacro with-signed-in ((var account) &body body)
  "Run BODY with VAR, ACCOUNT signed in (SIGNED-IN); its password wiped from
memory when BODY ends, however it ends."
  `(let ((,var (signed-in ,account)))
     (unwind-protect (progn ,@body)
       (let ((password (getf ,var :password)))
         (when (stringp password) (fill password #\Nul))))))

(defmacro noting-failure ((job name) &body body)
  "Run BODY; an error in it is noted among JOB's failures under NAME, and the work goes on."
  `(handler-case (progn ,@body)
     (error (e) (push (format nil "~a: ~a" ,name e) (caldav-job-failed ,job)))))

(defun chosen-calendars (account calendars)
  "Those of CALENDARS, ACCOUNT's, its :CALENDARS names (any case), or every
one when it names none."
  (let ((names (getf account :calendars)))
    (if names
        (remove-if-not (lambda (c) (member (getf c :name) names :test #'string-equal)) calendars)
        calendars)))

(defun sync-work (job text)
  "Sync every account's calendars, TEXT the calendars' file as it was when
asked, and fetch the subscriptions: JOB's work. What goes wrong with one
calendar is noted and the others are synced all the same."
  (let ((sections (calendar-sections text (file-entries text))))
    (dolist (account *caldav-accounts*)
      (noting-failure (job (getf account :name))
        (with-signed-in (account account)
          (dolist (calendar (chosen-calendars account (account-calendars account)))
            (job-check-stop)
            (noting-failure (job (format nil "~a, ~a" (getf account :name) (getf calendar :name)))
              (let ((pass (sync-calendar account calendar (section-of sections (getf calendar :url)))))
                (push pass (caldav-job-passes job))
                (dolist (refusal (calendar-pass-failed pass)) (push refusal (caldav-job-failed job)))))))))
    (dolist (subscription *caldav-subscriptions*)
      (job-check-stop)
      (noting-failure (job (getf subscription :name))
        (push (cons (getf subscription :name) (fetch-subscription subscription)) (caldav-job-subscribed job))))
    (setf (caldav-job-passes job) (nreverse (caldav-job-passes job)))))

(defun tell-failures (title failures)
  "Tell TITLE and FAILURES, what went wrong in words, as a notification:
on the desktop and in *Notifications* (\\[view-notifications]), so that a
sync that failed is seen wherever you are looking."
  (when failures
    (notify title (format nil "~{~a~^; ~}" failures) :source "Calendars")))

(defvar *told-last* nil
  "What the last sync on its own said of what lasts from one sync to the
next, (FAILURES . MISSING): said again only when it changes.")

(defmethod job-take-in ((job caldav-job))
  "Write what the sync fetched into the calendars' file, keep what each calendar now agrees with its server, and say what was done."
  (let ((passes (caldav-job-passes job)))
    ;; what each calendar agrees with its server is kept only once it is in the file
    (when passes
      (handler-case
          (progn (put-into-calendars (lambda (text) (merged-text text passes)))
                 (dolist (pass passes)
                   (write-calendar-state (calendar-pass-folder pass) (calendar-pass-token pass) (calendar-pass-events pass))))
        (error (e)
          (push (format nil "~a not written: ~a" (abbreviated-path (sync-file-path)) e) (caldav-job-failed job)))))
    (let* ((missing (loop for p in passes sum (calendar-pass-missing p)))
           (failed (reverse (caldav-job-failed job)))
           (quiet (caldav-job-quiet job))
           (worked (some (lambda (p) (or (plusp (calendar-pass-sent p)) (plusp (calendar-pass-fetched p)) (calendar-pass-gone p)))
                         passes))
           ;; a sync on its own says failures and entries deleted here once, not again while they last
           (new (not (equal (cons failed missing) *told-last*)))
           (new-failures (and failed (or (not quiet) new) failed)))
      (when quiet (setf *told-last* (cons failed missing)))
      (when (or (not quiet) worked (and new (or failed (plusp missing))))
        (message "Calendars synced: ~:d sent, ~:d fetched~:[~*~;, ~:d removed there~]~@[; ~{~a ~:d~^, ~}~]~@[; ~{~:d deleted here (~a)~}~]~@[; failed: ~{~a~^; ~}~]"
                (loop for p in passes sum (calendar-pass-sent p))
                (loop for p in passes sum (calendar-pass-fetched p))
                (some #'calendar-pass-gone passes) (loop for p in passes sum (length (calendar-pass-gone p)))
                (loop for (name . count) in (caldav-job-subscribed job) append (list name count))
                (and (plusp missing) (list missing (substitute-command-keys "\\[caldav-deleted] deletes them there or puts them back")))
                failed))
      (tell-failures "Calendars not all synced" new-failures))))

(define-command caldav-sync ()
  "Sync the calendars of your CalDAV accounts (*caldav-accounts*) with
the calendars' Org file (*caldav-file*), a heading each: what changed
in an entry here is sent, what changed on the server is written into its
entry, your own lines in it kept, and new events are added under their
calendar. An event deleted on the server takes its entry with it, but one
holding lines of yours stays, tagged removed, its times made inactive so
that the agenda shows it no more; an entry deleted here is deleted on the server only when \\[caldav-deleted]
is asked; an entry cancelled here (CANCELLED) has its event deleted there,
and stays, tagged removed. When an event changed on both sides, the server's stands and your
copy is kept beside it, tagged conflict. The subscriptions
\(*caldav-subscriptions*) are fetched too. In the background."
  (if (and (null *caldav-accounts*) (null *caldav-subscriptions*))
      (message "No calendars to sync: *caldav-accounts* and *caldav-subscriptions* name none")
      (take-in-soon (start-sync nil) 0 "Syncing the calendars")))

(defvar *sync-job* nil
  "The sync running or last run, or NIL.")

(defun start-sync (quiet)
  "Start syncing the calendars in the background, QUIET for a sync on its
own (CALDAV-JOB), and return its job."
  (let* ((buffer (calendars-buffer))
         (job (make-caldav-job buffer quiet))
         (text (buffer-string buffer)))
    (setf *sync-job* job)
    (start-job-thread job (lambda (job) (sync-work job text)) :name "tuatara caldav")))

(defvar *next-sync* nil
  "The universal time the next sync on its own is due, or NIL before the first.")

(defparameter *first-sync-seconds* 10
  "How long after Tuatara starts the first sync on its own waits, so that starting stays quick.")

(defun sync-tick (editor)
  "Tick function: start a sync on its own every *CALDAV-MINUTES*, the
first a few seconds after Tuatara starts, quiet, none while one runs; the
seconds until the next is due, the editor sleeping meanwhile, or NIL when
there is none to do."
  (declare (ignore editor))
  (let ((minutes *caldav-minutes*)
        (now (get-universal-time)))
    (cond ((or (not (realp minutes)) (<= minutes 0)
               (and (null *caldav-accounts*) (null *caldav-subscriptions*)))
           (setf *next-sync* nil)
           nil)
          ((null *next-sync*)
           (setf *next-sync* (+ now *first-sync-seconds*))
           *first-sync-seconds*)
          ((< now *next-sync*) (- *next-sync* now))
          ((and *sync-job* (job-running-p *sync-job*)) 5)
          (t (start-sync t)
             (setf *next-sync* (+ now (max 60 (round (* 60 minutes)))))
             (- *next-sync* now)))))

;;; The calendars an account has

(defstruct (calendar-list-job (:include job) (:constructor make-calendar-list-job (buffer)) (:copier nil))
  "Asking each account which calendars it has: FOUND, each (ACCOUNT NAMES
CHOSEN), CHOSEN those of NAMES its :CALENDARS chooses; FAILED, what went
wrong, in words."
  found failed)

(setf (documentation 'calendar-list-job-p 'function) "True when OBJECT is a CALENDAR-LIST-JOB.")

(define-job-words calendar-list-job
  :doing "Asking for the calendars"
  :description "Asking each account's server which calendars it has"
  :verb "list the calendars"
  :left-words "nothing listed")

(defmethod job-take-in ((job calendar-list-job))
  "Say each account's calendars, those synced first."
  (message "~:[No accounts in *caldav-accounts*~;~:*~{~a~^; ~}~]~@[; failed: ~{~a~^; ~}~]"
           (loop for (account names chosen) in (reverse (calendar-list-job-found job))
                 collect (format nil "~a: ~:[no calendars~;~:*~{~a~^, ~}~]~@[ (synced: ~{~a~^, ~})~]"
                                 account names (and (set-difference names chosen :test #'string=) chosen)))
           (reverse (calendar-list-job-failed job)))
  (tell-failures "Calendars not listed" (reverse (calendar-list-job-failed job))))

(define-command caldav-calendars ()
  "Say the calendars each account of *caldav-accounts* has, by the
names its :CALENDARS takes, and which of them are synced: a way to see that
Tuatara reaches the server, and what it finds there. In the background."
  (let ((job (make-calendar-list-job (current-buffer))))
    (start-job-thread job
                      (lambda (job)
                        (dolist (account *caldav-accounts*)
                          (handler-case
                              (with-signed-in (signed account)
                                (let ((calendars (account-calendars signed)))
                                (push (list (getf account :name)
                                            (mapcar (lambda (c) (getf c :name)) calendars)
                                            (mapcar (lambda (c) (getf c :name)) (chosen-calendars account calendars)))
                                      (calendar-list-job-found job))))
                            (error (e) (push (format nil "~a: ~a" (getf account :name) e) (calendar-list-job-failed job))))))
                      :name "tuatara caldav list")
    (take-in-soon job 0 "Asking for the calendars")))

;;; Entries deleted here

(defun missing-events ()
  "The events the calendars' file no longer has an entry of, under a
calendar's heading that is there: each (FOLDER AGREED URL), URL the calendar's."
  (let* ((text (buffer-string (calendars-buffer)))
         (sections (calendar-sections text (file-entries text))))
    (loop for section in sections
          for folder = (calendar-folder (getf section :url))
          nconc (loop for a in (nth-value 1 (read-calendar-state folder))
                      unless (entry-of-uid section (agreed-uid a))
                        collect (list folder a (getf section :url))))))

(defun missing-title (missing)
  "The title of the event of MISSING, an element of MISSING-EVENTS."
  (destructuring-bind (folder agreed url) missing
    (declare (ignore url))
    (let* ((calendar (agreed-calendar folder agreed))
           (event (and calendar (find-if (lambda (c) (member (ical-component-name c) '("VEVENT" "VTODO") :test #'string-equal))
                                         (ical-components-of calendar)))))
      (or (and event (ical-value-of event "SUMMARY") (one-line (ical-text (ical-value-of event "SUMMARY"))))
          (agreed-uid agreed)))))

(defstruct (calendar-delete-job (:include job) (:constructor make-calendar-delete-job (buffer missing)) (:copier nil))
  "Deleting on their servers the events of MISSING (MISSING-EVENTS): DELETED,
those that went; FAILED, what went wrong, in words."
  missing deleted failed)

(setf (documentation 'calendar-delete-job-p 'function) "True when OBJECT is a CALENDAR-DELETE-JOB.")

(define-job-words calendar-delete-job
  :doing "Deleting events on their servers"
  :description "Deleting on their servers the events deleted from the calendars' file"
  :verb "delete the events"
  :left-words "nothing deleted")

(defmethod job-take-in ((job calendar-delete-job))
  "Forget the events deleted, and say what was done."
  (dolist (missing (calendar-delete-job-deleted job))
    (destructuring-bind (folder agreed url) missing
      (declare (ignore url))
      (multiple-value-bind (token events) (read-calendar-state folder)
        (write-calendar-state folder token (remove (agreed-url agreed) events :key #'agreed-url :test #'string=))
        (ignore-errors (remove-file (path-join folder (agreed-file agreed)))))))
  (message "~:d event~:p deleted on ~:*~[their servers~;its server~:;their servers~]~@[; failed: ~{~a~^; ~}~]"
           (length (calendar-delete-job-deleted job)) (reverse (calendar-delete-job-failed job)))
  (tell-failures "Events not all deleted on their servers" (reverse (calendar-delete-job-failed job))))

(define-command caldav-deleted ()
  "Delete on their servers the events whose entries were deleted from the
calendars' file, after asking; or, answered no, put their entries back as
the server has them."
  (let ((missing (missing-events)))
    (cond
      ((null missing) (message "Every event synced still has its entry"))
      ((yes-or-no-p* (format nil "Delete on the server ~:d event~:p deleted here (~{~a~^, ~})? n puts ~:*~:*~[them~;it~:;them~] back"
                             (length missing) (mapcar #'missing-title missing)))
       (let ((job (make-calendar-delete-job (current-buffer) missing)))
         (start-job-thread job
                           (lambda (job)
                             (dolist (m missing)
                               (destructuring-bind (folder agreed url) m
                                 (declare (ignore folder))
                                 (handler-case
                                     (with-signed-in (signed (account-of url))
                                       (let ((outcome (delete-event signed (agreed-url agreed) (agreed-etag agreed))))
                                         (if (eq outcome :done)
                                             (push m (calendar-delete-job-deleted job))
                                             (push (format nil "~a changed on the server since: kept, sync first" (missing-title m))
                                                   (calendar-delete-job-failed job)))))
                                   (caldav-error (e) (push (format nil "~a: ~a" (missing-title m) e) (calendar-delete-job-failed job)))))))
                           :name "tuatara caldav delete")
         (take-in-soon job 0 "Deleting the events on their servers")))
      (t
       (put-into-calendars
        (lambda (text)
          (merged-text text (loop for (folder agreed url) in missing
                                  for calendar = (agreed-calendar folder agreed)
                                  for members = (and calendar (group-of-uid calendar (agreed-uid agreed)))
                                  when members
                                    collect (let ((pass (make-calendar-pass url "" folder)))
                                              (setf (calendar-pass-incoming pass)
                                                    (list (list (agreed-uid agreed) (event-entry members calendar :level 2) nil)))
                                              pass)))))
       (message "~:d entr~:@p put back" (length missing))))))

;;; Calendar files

(defun calendar-headings (text)
  "The calendars' headings of TEXT, the calendars' file, each (TITLE . URL)."
  (loop for section in (calendar-sections text (file-entries text))
        collect (cons (agenda-entry-title (getf section :entry)) (getf section :url))))

(defparameter *not-synced* "Not synced: under a heading of its own"
  "The choice of CALDAV-IMPORT that puts what it reads under a heading of its own.")

(defstruct (calendar-import-job (:include job) (:constructor make-calendar-import-job (buffer path url)) (:copier nil))
  "Reading the calendar file at PATH into the calendars' file, under the
calendar at URL (NIL for a heading of its own): ENTRIES, its events as Org
entries; COUNT, how many; FAILED, what went wrong, in words."
  path url entries (count 0) failed)

(setf (documentation 'calendar-import-job-p 'function) "True when OBJECT is a CALENDAR-IMPORT-JOB.")

(define-job-words calendar-import-job
  :doing "Reading a calendar file"
  :description (format nil "Reading ~a into the calendars' file" (calendar-import-job-path job))
  :verb "import the calendar file"
  :left-words "nothing imported")

(defmethod job-take-in ((job calendar-import-job))
  "Put the entries read into the calendars' file, and say how many."
  (let ((url (calendar-import-job-url job)) (entries (calendar-import-job-entries job)))
    (cond ((calendar-import-job-failed job) (message "~a" (calendar-import-job-failed job)))
          ((zerop (calendar-import-job-count job))
           (message "No events in ~a" (abbreviated-path (calendar-import-job-path job))))
          (t
           (put-into-calendars
            (lambda (text)
              (let ((at (if url
                            (let ((section (section-of (calendar-sections text (file-entries text)) url)))
                              (if section (getf section :end) (length text)))
                            (length text))))
                (concatenate 'string (subseq text 0 at) (on-new-line text at)
                             (if url "" (format nil "* ~a~%" (heading-words (format nil "Imported from ~a" (native-file-name (calendar-import-job-path job))))))
                             entries (subseq text at)))))
           (message "~:d event~:p imported into ~a~:[~; (sent at the next sync)~]"
                    (calendar-import-job-count job) (abbreviated-path (sync-file-path)) url)))))

(define-command caldav-import ()
  "Read a calendar file (.ics) into the calendars' file: under one of your
calendars, whose server gets its events at the next sync, or under a
heading of its own, never synced. Read in the background."
  (let ((path (read-file-path "Import the calendar file: ")))
    (when path
      (let* ((native-path (native (expand-path path)))
             (headings (calendar-headings (buffer-string (calendars-buffer))))
             (target (completing-read "Import into: " (cons *not-synced* (mapcar #'car headings)) :require-match t)))
        (when target
          (let ((job (make-calendar-import-job (current-buffer) native-path
                                               (cdr (assoc target headings :test #'string=)))))
            (start-job-thread job
                              (lambda (job)
                                (handler-case
                                    (dolist (c (icalendar-read (octets-string (file-octets native-path :limit *icalendar-largest*))))
                                      (job-check-stop)
                                      (multiple-value-bind (text count) (calendar-entries c)
                                        (setf (calendar-import-job-entries job) (concatenate 'string (or (calendar-import-job-entries job) "") text))
                                        (incf (calendar-import-job-count job) count)))
                                  (error (e) (setf (calendar-import-job-failed job) (format nil "~a not imported: ~a" (abbreviated-path native-path) e)))))
                              :name "tuatara caldav import")
            (take-in-soon job 0.2 "Reading the calendar file")))))))

(define-command caldav-export ()
  "Write the entry at point and those under it, or the entries selected, as
a calendar file (.ics) other calendars read: their times, title, place,
tags and description."
  (let* ((buffer (current-buffer))
         (text (buffer-string buffer))
         (entries (org-agenda-file-entries (or (buffer-file buffer) (buffer-name buffer)) text))
         (region (selected-region buffer))
         (chosen (if region
                     (let ((from (length (buffer-string buffer :end (first region))))
                           (to (length (buffer-string buffer :end (second region)))))
                       (remove-if-not (lambda (e) (and (< (agenda-entry-position e) to) (> (agenda-entry-end e) from))) entries))
                     (let* ((at (length (buffer-string buffer :end (buffer-point buffer))))
                            (here (find-if (lambda (e) (<= (agenda-entry-position e) at)) entries :from-end t)))
                       (and here (cons here (loop for e in (rest (member here entries))
                                                  while (> (agenda-entry-level e) (agenda-entry-level here))
                                                  collect e))))))
         (calendars (loop for e in chosen
                          for c = (ignore-errors (entry-calendar e))
                          when (and c (some (lambda (x) (or (ical-property-of x "DTSTART") (ical-property-of x "DUE")))
                                            (ical-components-of c)))
                            collect c)))
    (if (null calendars)
        (message "No entry with a time here to write as a calendar file")
        (let ((path (read-new-native-path "Write the calendar file: ")))
          (when path
            (let* ((zones (remove-duplicates (loop for c in calendars append (ical-components-of c "VTIMEZONE"))
                                             :key (lambda (z) (ical-value-of z "TZID")) :test #'equal))
                   (events (loop for c in calendars append (calendar-events c)))
                   (whole (make-ical-component "VCALENDAR" (ical-component-properties (first calendars)) (append zones events))))
              (replace-file path (icalendar-write whole))
              (message "~:d entr~:@p written to ~a" (length calendars) (abbreviated-path path))))))))

;;; New events

(defparameter *new-event-form*
  (format nil "* %^{Title}~%:PROPERTIES:~%:LOCATION: %^{Place (RET for none)}~%:END:~%%^T~%#+name: description~%#+begin_example~%%?~%#+end_example")
  "The form CALDAV-EVENT-NEW opens: its title, place and time asked first,
in that order, then the description written between the block's two lines,
where point is.")

(define-command caldav-event-new ()
  "Write a new event for one of your calendars: choose the calendar (when
there is more than one), then give its title, its place (RET for none) and
its time in the calendar. A form opens with point in its description: write
it there, or nothing. \\<org-capture-mode>\\[org-capture-finalize] files the event under the calendar's
heading, and the server gets it at the next sync; \\[org-capture-refile] files it under
another heading, \\[org-capture-kill] throws it away."
  (let ((headings (calendar-headings (buffer-string (calendars-buffer)))))
    (if (null headings)
        (message "No calendars synced yet: ~a syncs them" (substitute-command-keys "\\<org-mode>\\[caldav-sync]"))
        (let ((calendar (if (rest headings)
                            (completing-read "New event in: " (mapcar #'car headings) :require-match t)
                            (car (first headings)))))
          (when calendar
            (org-capture-with "Calendar event" (list :olp (sync-file-path) calendar) *new-event-form*))))))

;;; Keys

(defun install-caldav ()
  "Bind caldav's keys under C-c s, in Org files and in the agenda."
  (dolist (mode '(org-mode org-agenda-mode))
    (define-keys (mode-keymap mode) '(("C-c s s" caldav-sync) ("C-c s d" caldav-deleted) ("C-c s i" caldav-import)
                                      ("C-c s l" caldav-calendars) ("C-c s n" caldav-event-new)))
    (label-keys (mode-keymap mode) '(("C-c s" "calendars"))))
  (define-key (mode-keymap 'org-mode) "C-c s e" 'caldav-export)
  (pushnew 'sync-tick *tick-functions*)
  (commands-for-mode 'org-mode "an Org file" 'caldav-export))

(install-caldav)
