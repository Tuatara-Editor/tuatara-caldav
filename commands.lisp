;;;; commands.lisp --- The commands: sync the calendars, delete what was deleted here, import and export calendar files.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:caldav)

;;; The calendars' file, changed while it may be open

(defun calendars-buffer ()
  "The buffer of the calendars' file, read when it is not open, never shown."
  (visit-file (sync-file-path)))

(define-record (calendars-file-job (:include job) (:copier nil))
  "A job that puts what it works out into the calendars' file when it is taken in."
  (before nil
          :documentation "The calendars' file as it was when the job started, as (VERSION ROPE TEXT): its buffer's
version, its text as a rope that later edits leave alone, and as a string.")
  (edits nil
         :documentation "What the job's thread worked out, as (VERSION . EDITS): the edits to the text of the buffer
at that version, NIL when not worked out."))

(defun note-calendars-file (job &optional (buffer (calendars-buffer)))
  "Take down in JOB the calendars' file as it stands, for PREPARE-CALENDARS-CHANGE
to work from on JOB's own thread. Run on the editor's thread, before JOB starts."
  (setf (calendars-file-job-before job) (list (buffer-version buffer) (buffer-text buffer) (buffer-string buffer))))

(defun prepare-calendars-change (job change)
  "On JOB's own thread, work out the edits that CHANGE, a function of the text of
the calendars' file, makes to it as NOTE-CALENDARS-FILE took it down, so that
PUT-INTO-CALENDARS only has to put them in. Nothing is kept when that fails, or
when the file was not taken down."
  (let ((before (calendars-file-job-before job)))
    (when before
      (destructuring-bind (version rope text) before
        (setf (calendars-file-job-edits job)
              (handler-case (cons version (line-edits rope (funcall change text)))
                (error () nil)))))))

(defun put-into-calendars (change &optional job)
  "Make the calendars' file what CHANGE, a function of its text, gives, only
the lines that differ changed, one change to undo, and save it. True when it
changed. The edits JOB worked out (PREPARE-CALENDARS-CHANGE) are put in as they
are while the file is as JOB took it down; else CHANGE is run on its text now."
  (let* ((buffer (calendars-buffer))
         (worked (and job (calendars-file-job-edits job)))
         (edits (if (and worked (eq (car worked) (buffer-version buffer)))
                    (cdr worked)
                    (let ((text (buffer-string buffer)))
                      (line-edits (buffer-text buffer) (funcall change text))))))
    (when (plusp (length edits))
      (check-writable buffer)
      (put-text-by-lines buffer (buffer-text buffer) nil :edits edits)
      (save-buffer-with-care buffer)
      t)))

(define-record (text-reading-job (:include job) (:constructor make-text-reading-job (buffer text work then))
                                 (:copier nil))
  "Working out from TEXT, a file's text as it stood when the job started, what
a command asks about: WORK, a function of the text, gives it on the job's own
thread, and THEN, a function of what WORK gave, asks on the editor's."
  (text nil :documentation "The text TEXT-READING-JOB works from.")
  (work nil :documentation "What TEXT-READING-JOB works out, a function of its text.")
  (then nil :documentation "What is done with what was worked out, on the editor's thread, a function of it.")
  (found nil :documentation "What WORK gave of the text.")
  (done nil :documentation "True once WORK has given what it found."))

(define-job-words text-reading-job
  :doing "Reading entries"
  :description "Reading the entries a command asks about"
  :verb "read the entries"
  :left-words "nothing asked")

(defmethod job-take-in ((job text-reading-job))
  "Ask what the command asks, with what was worked out."
  (if (text-reading-job-done job)
      (funcall (text-reading-job-then job) (text-reading-job-found job))
      (message "The entries were not read: ~a" (job-ending-words job))))

(defun text-reading-work (job)
  "Work out what JOB, a TEXT-READING-JOB, is for from its text: JOB's work."
  (setf (text-reading-job-found job) (funcall (text-reading-job-work job) (text-reading-job-text job))
        (text-reading-job-done job) t))

(defun with-text-read (text words work then)
  "Work out in the background what WORK, a function of TEXT, a file's text,
gives of it, then call THEN with that on the editor's thread, so that a
command parses the file before its question without holding the editor up.
WORDS say what is being done, should it take long enough to be said."
  (let ((job (make-text-reading-job (current-buffer) text work then)))
    (start-job-thread job #'text-reading-work :name "tuatara caldav reading")
    (take-in-soon job 0.2 words)))

(defun account-of (url)
  "The account of *CALDAV-ACCOUNTS* whose server holds the calendar at
web address (URL): the one on its host, else the first."
  (let ((host (nth-value 1 (url-parts url))))
    (or (find-if (lambda (a) (equal (nth-value 1 (url-parts (getf a :url))) host)) *caldav-accounts*)
        (first *caldav-accounts*))))

(defparameter *password-command-seconds* 120
  "How long a password command may take: long enough to type a passphrase in
the window gpg-agent opens for it.")

(defun command-password (account)
  "The password ACCOUNT's :PASSWORD-COMMAND prints, its first line: the
command split into words as a shell splits it, though no shell runs, a word
beginning with ~/ in your home directory. A CALDAV-ERROR when it fails; what it
prints is never said."
  (multiple-value-bind (words ok why) (command-words (getf account :password-command))
    (unless (and ok words)
      (caldav-fail "The password command of ~a is no command~@[: ~a~]" (getf account :name) why))
    (let* ((words (mapcar (lambda (w) (if (uiop:string-prefix-p "~/" w) (native (expand-path w)) w)) words))
           (program (find-program (first words))))
      (unless program
        (caldav-fail "The password command of ~a: ~a is not installed" (getf account :name) (first words)))
      (multiple-value-bind (said ok) (program-output (cons program (rest words)) :seconds *password-command-seconds*)
        (let ((password (and said
                             (let ((end (or (position #\Newline said) (length said))))
                               (subseq said 0 (if (and (plusp end) (char= #\Return (char said (1- end))))
                                                  (1- end)
                                                  end))))))
          (when said (fill said #\Nul))
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
      (caldav-fail "No password for ~a in the keyring: ~
                    secret-tool store --label=\"Tuatara calendars\" service tuatara-caldav user ~a, ~
                    or a :password-command"
                   (getf account :name) (getf account :user)))
    (list* :password password account)))

;;; A sync

(define-record (caldav-job (:include calendars-file-job) (:constructor make-caldav-job (buffer quiet)) (:copier nil))
  "Syncing the calendars of *CALDAV-ACCOUNTS* and fetching *CALDAV-SUBSCRIPTIONS*."
  (passes nil :documentation "What each calendar's sync did, one calendar-pass for each, as CALDAV-JOB does it.")
  (subscribed nil :documentation "How many events each subscription of CALDAV-JOB holds, each (NAME . COUNT).")
  (failed nil :documentation "What went wrong in CALDAV-JOB, in words, by account or subscription.")
  (quiet nil
         :documentation "True when CALDAV-JOB was started on its own, so that it says nothing when nothing changed."))

(define-job-words caldav-job
  :doing "Syncing the calendars"
  :description "Sending what changed here to the calendars' servers and fetching what changed there"
  :verb "sync the calendars"
  :left-words "nothing written")

(defmacro with-signed-in ((var account) &body body)
  "Run BODY with the variable (VAR) holding ACCOUNT signed in (SIGNED-IN). The
password kept there is wiped from memory when BODY ends, however it ends, as
the copies made of it to read it and to give it to curl are once used; what
the programs run keep of it is theirs. Meanwhile the password is a secret at
hand (*SECRET-AT-HAND*), from asking for it on, in the threads started from
BODY too: a failure is written to the debug log by its kind alone, without
the calls under way or what they were given."
  `(let* ((*secret-at-hand* t)
          (,var (signed-in ,account)))
     (unwind-protect (progn ,@body)
       (let ((password (getf ,var :password)))
         (when (stringp password) (fill password #\Nul))))))

(defun failure-said (condition)
  "CONDITION in words for telling what failed: a server or a calendar file gone
wrong (CALDAV-ERROR, ICALENDAR-ERROR) in the package's own words, in full; any
other failure by its kind alone (\"a type-error\", FAILURE-WORDS with a secret at
hand), as its words could hold the password of the account being synced."
  (if (typep condition '(or caldav-error icalendar-error))
      (princ-to-string condition)
      (let ((*secret-at-hand* t))
        (failure-words condition :said t))))

(defmacro noting-failure ((job name) &body body)
  "Run BODY; an error in it is noted among JOB's failures under NAME, in the words
FAILURE-SAID gives, and the work goes on."
  `(handler-case (progn ,@body)
     (error (e) (push (format nil "~a: ~a" ,name (failure-said e)) (caldav-job-failed ,job)))))

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

(defun sync-change (job)
  "The change to the calendars' file that what JOB fetched makes, a function of its text."
  (lambda (text) (merged-text text (caldav-job-passes job))))

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
  "Write what the sync fetched into the calendars' file, keep what each calendar
now agrees with its server, and say what was done."
  (let ((passes (caldav-job-passes job)))
    ;; what each calendar agrees with its server is kept only once it is in the file
    (handler-case
        (progn (when (some #'calendar-pass-edits-p passes)
                 (put-into-calendars (sync-change job) job))
               (mapc #'keep-calendar-state passes))
      (error (e)
        (push (format nil "~a not written: ~a" (abbreviated-path (sync-file-path)) e) (caldav-job-failed job))))
    (let* ((missing (loop for p in passes sum (calendar-pass-missing p)))
           (failed (reverse (caldav-job-failed job)))
           (quiet (caldav-job-quiet job))
           (worked (some (lambda (p)
                           (or (plusp (calendar-pass-sent p)) (plusp (calendar-pass-fetched p)) (calendar-pass-gone p)))
                         passes))
           ;; a sync on its own says failures and entries deleted here once, not again while they last
           (new (not (equal (cons failed missing) *told-last*)))
           (new-failures (and failed (or (not quiet) new) failed)))
      (when quiet (setf *told-last* (cons failed missing)))
      (when (or (not quiet) worked (and new (or failed (plusp missing))))
        (message "Calendars synced: ~:d sent, ~:d fetched~:[~*~;, ~:d removed there~]~@[; ~{~a ~:d~^, ~}~]~
                  ~@[; ~{~:d deleted here (~a)~}~]~@[; failed: ~{~a~^; ~}~]"
                (loop for p in passes sum (calendar-pass-sent p))
                (loop for p in passes sum (calendar-pass-fetched p))
                (some #'calendar-pass-gone passes) (loop for p in passes sum (length (calendar-pass-gone p)))
                (loop for (name . count) in (caldav-job-subscribed job) append (list name count))
                (and (plusp missing)
                     (list missing (substitute-command-keys "\\[caldav-deleted] deletes them there or puts them back")))
                failed))
      (tell-failures "Calendars not all synced" new-failures))))

(define-command caldav-sync ()
  "Sync the calendars of your Calendaring Extensions to WebDAV (CalDAV) accounts
\(*caldav-accounts*) with the calendars' Org file (*caldav-file*), a heading
each: what changed in an entry here is sent, what changed on the server is
written into its entry, your own lines in it kept, and new events are added
under their calendar. An event deleted on the server takes its entry with it,
but one holding lines of yours stays, tagged removed, its times made inactive so
that the agenda shows it no more; an entry deleted here is deleted on the server
only when \\[caldav-deleted] is asked; an entry cancelled here (CANCELLED) has
its event deleted there, and stays, tagged removed. When an event changed on
both sides, the server's stands and your copy is kept beside it, tagged
conflict. The subscriptions (*caldav-subscriptions*) are fetched too. In the
background."
  (if (and (null *caldav-accounts*) (null *caldav-subscriptions*))
      (message "No calendars to sync: *caldav-accounts* and *caldav-subscriptions* name none")
      (take-in-soon (start-sync nil) 0 "Syncing the calendars")))

(defvar *sync-job* nil
  "The sync running or last run, or NIL.")

(defun start-sync (quiet)
  "Start syncing the calendars in the background, QUIET for a sync on its
own (CALDAV-JOB), and return its job."
  (let* ((buffer (calendars-buffer))
         (job (make-caldav-job buffer quiet)))
    (setf *sync-job* job)
    (note-calendars-file job buffer)
    (start-job-thread job
                      (lambda (job)
                        (sync-work job (third (calendars-file-job-before job)))
                        (when (some #'calendar-pass-edits-p (caldav-job-passes job))
                          (prepare-calendars-change job (sync-change job))))
                      :name "tuatara caldav")))

(defvar *next-sync* nil
  "The universal time the next sync on its own is due, or NIL before the first.")

(defparameter *first-sync-seconds* 10
  "How long after Tuatara starts the first sync on its own waits, so that
starting stays quick.")

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

(define-record (calendar-list-job (:include job) (:constructor make-calendar-list-job (buffer)) (:copier nil))
  "Asking each account which calendars it has."
  (found nil
         :documentation "What CALENDAR-LIST-JOB found, each (ACCOUNT NAMES CHOSEN), CHOSEN those of NAMES its
:CALENDARS chooses.")
  (failed nil :documentation "What went wrong in CALENDAR-LIST-JOB, in words."))

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
                            (error (e)
                              (push (format nil "~a: ~a" (getf account :name) (failure-said e))
                                    (calendar-list-job-failed job))))))
                      :name "tuatara caldav list")
    (take-in-soon job 0 "Asking for the calendars")))

;;; Entries deleted here

(defun missing-events (text)
  "The events TEXT, the calendars' file, no longer has an entry of, under a
calendar's heading that is there: each a directory, an agreed event and the
calendar's web address (URL), as (DIRECTORY AGREED URL)."
  (let ((sections (calendar-sections text (file-entries text))))
    (loop for section in sections
          for directory = (calendar-directory (getf section :url))
          nconc (loop for a in (nth-value 1 (read-calendar-state directory))
                      unless (entry-of-uid section (agreed-uid a))
                        collect (list directory a (getf section :url))))))

(defun missing-title (missing)
  "The title of the event of MISSING, an element of MISSING-EVENTS."
  (destructuring-bind (directory agreed url) missing
    (declare (ignore url))
    (let* ((calendar (agreed-calendar directory agreed))
           (event (and calendar
                       (find-if (lambda (c) (member (ical-component-name c) '("VEVENT" "VTODO") :test #'string-equal))
                                         (ical-components-of calendar)))))
      (or (and event (ical-value-of event "SUMMARY") (one-line (ical-text (ical-value-of event "SUMMARY"))))
          (agreed-uid agreed)))))

(define-record (calendar-delete-job (:include job) (:constructor make-calendar-delete-job (buffer missing))
                                    (:copier nil))
  "Deleting on their servers the events of MISSING (MISSING-EVENTS)."
  (missing nil :documentation "The events CALENDAR-DELETE-JOB deletes on their servers, as MISSING-EVENTS gives them.")
  (deleted nil :documentation "The events of CALENDAR-DELETE-JOB that were deleted.")
  (failed nil :documentation "What went wrong in CALENDAR-DELETE-JOB, in words."))

(define-job-words calendar-delete-job
  :doing "Deleting events on their servers"
  :description "Deleting on their servers the events deleted from the calendars' file"
  :verb "delete the events"
  :left-words "nothing deleted")

(defmethod job-take-in ((job calendar-delete-job))
  "Forget the events deleted, and say what was done."
  (dolist (missing (calendar-delete-job-deleted job))
    (destructuring-bind (directory agreed url) missing
      (declare (ignore url))
      (multiple-value-bind (token events ctag) (read-calendar-state directory)
        (write-calendar-state directory token (remove (agreed-url agreed) events :key #'agreed-url :test #'string=)
                              ctag)
        (ignore-errors (remove-file (path-join directory (agreed-file agreed)))))))
  (message "~:d event~:p deleted on ~:*~[their servers~;its server~:;their servers~]~@[; failed: ~{~a~^; ~}~]"
           (length (calendar-delete-job-deleted job)) (reverse (calendar-delete-job-failed job)))
  (tell-failures "Events not all deleted on their servers" (reverse (calendar-delete-job-failed job))))

(defun delete-missing-work (job)
  "Delete on their servers the events of JOB's MISSING, each signed in to its
account: JOB's work. What goes wrong with one event is noted, in the words
FAILURE-SAID gives, and the others are deleted all the same."
  (dolist (m (calendar-delete-job-missing job))
    (destructuring-bind (directory agreed url) m
      (declare (ignore directory))
      (handler-case
          (with-signed-in (signed (account-of url))
            (let ((outcome (delete-event signed (agreed-url agreed) (agreed-etag agreed))))
              (if (eq outcome :done)
                  (push m (calendar-delete-job-deleted job))
                  (push (format nil "~a changed on the server since: kept, sync first" (missing-title m))
                        (calendar-delete-job-failed job)))))
        (error (e) (push (format nil "~a: ~a" (missing-title m) (failure-said e)) (calendar-delete-job-failed job)))))))

(define-record (calendar-restore-job (:include calendars-file-job)
                                     (:constructor make-calendar-restore-job (buffer missing))
                                     (:copier nil))
  "Writing again, as their servers have them, the entries deleted from the
calendars' file of the events of MISSING (MISSING-EVENTS)."
  (missing nil :documentation "The events whose entries are written again, as MISSING-EVENTS gives them.")
  (passes nil :documentation "What is put into the calendars' file once written, as MERGED-TEXT takes it."))

(define-job-words calendar-restore-job
  :doing "Writing the entries deleted again"
  :description "Writing again the entries deleted from the calendars' file, as their servers have them"
  :verb "put the entries back"
  :left-words "nothing put back")

(defmethod job-take-in ((job calendar-restore-job))
  "Put the entries written into the calendars' file, and say how many."
  (put-into-calendars (restore-change job) job)
  (message "~:d entr~:@p put back" (length (calendar-restore-job-missing job))))

(defun restore-change (job)
  "The change to the calendars' file that the entries JOB wrote again make, a function of its text."
  (lambda (text) (merged-text text (calendar-restore-job-passes job))))

(defun restore-missing-work (job)
  "Write the entries of JOB's MISSING as their servers have them, their rules
held together to the periods of WITH-PERIODS-TOGETHER: JOB's work."
  (with-periods-together
    (setf (calendar-restore-job-passes job)
          (loop for (directory agreed url) in (calendar-restore-job-missing job)
                for calendar = (progn (job-check-stop) (agreed-calendar directory agreed))
                for members = (and calendar (group-of-uid calendar (agreed-uid agreed)))
                when members
                  collect (let ((pass (make-calendar-pass url "" directory)))
                            (setf (calendar-pass-incoming pass)
                                  (list (list (agreed-uid agreed) (event-entry members calendar :level 2) nil)))
                            pass))))
  (prepare-calendars-change job (restore-change job)))

(define-command caldav-deleted ()
  "Delete on their servers the events whose entries were deleted from the
calendars' file, after asking; or, answered no, put their entries back as
the server has them, written in the background. The calendars' file and the
copies of the events are read in the background before it asks."
  (let ((buffer (current-buffer)))
    (with-text-read
     (buffer-string (calendars-buffer)) "Looking for the entries deleted"
     (lambda (text)
       (let ((missing (missing-events text)))
         (cons missing (mapcar #'missing-title missing))))
     (lambda (found)
       (destructuring-bind (missing . titles) found
         (cond
           ((null missing) (message "Every event synced still has its entry"))
           ((yes-or-no-p* (format nil "Delete on the server ~:d event~:p deleted here? " (length missing))
                          :details (format nil "Deleted here, still on the server:~%~{  ~a~%~}~%~
                                                yes deletes ~[them~;it~:;them~] on the server; no puts ~
                                                ~:*~[them~;it~:;them~] back in the file as the server has ~
                                                ~:*~[them~;it~:;them~].~%"
                                           titles (length missing)))
            (let ((job (make-calendar-delete-job buffer missing)))
              (start-job-thread job #'delete-missing-work :name "tuatara caldav delete")
              (take-in-soon job 0 "Deleting the events on their servers")))
           (t
            (let ((job (make-calendar-restore-job buffer missing)))
              (start-job-thread job #'restore-missing-work :name "tuatara caldav restore")
              (take-in-soon job 0.2 "Writing the entries again")))))))))

;;; Calendar files

(defun calendar-headings (text)
  "The calendars' headings of TEXT, the calendars' file, each a title and the
calendar's web address (URL), as (TITLE . URL)."
  (loop for section in (calendar-sections text (file-entries text))
        collect (cons (agenda-entry-get (getf section :entry) :title) (getf section :url))))

(defparameter *not-synced* "Not synced: under a heading of its own"
  "The choice of CALDAV-IMPORT that puts what it reads under a heading of its own.")

(define-record (calendar-import-job (:include calendars-file-job)
                                    (:constructor make-calendar-import-job (buffer path url))
                                    (:copier nil))
  "Reading the calendar file at PATH into the calendars' file, under the calendar at the web address (URL), NIL for
a heading of its own."
  (path nil :documentation "The calendar file CALENDAR-IMPORT-JOB reads.")
  (url nil
       :documentation "The web address of the calendar CALENDAR-IMPORT-JOB puts the events under, NIL for a heading
of their own.")
  (entries nil :documentation "The events CALENDAR-IMPORT-JOB read, as Org entries.")
  (count 0 :documentation "How many events CALENDAR-IMPORT-JOB read.")
  (failed nil :documentation "What went wrong in CALENDAR-IMPORT-JOB, in words."))

(define-job-words calendar-import-job
  :doing "Reading a calendar file"
  :description (format nil "Reading ~a into the calendars' file" (calendar-import-job-path job))
  :verb "import the calendar file"
  :left-words "nothing imported")

(defun import-change (job)
  "The change to the calendars' file that the entries JOB read make, a function of its text: put under the
calendar's heading, or under one of their own at the end."
  (let ((url (calendar-import-job-url job)) (entries (calendar-import-job-entries job)))
    (lambda (text)
      (let ((at (if url
                    (let ((section (section-of (calendar-sections text (file-entries text)) url)))
                      (if section (getf section :end) (length text)))
                    (length text))))
        (concatenate 'string (subseq text 0 at) (on-new-line text at)
                     (if url
                         ""
                         (format nil
                                 "* ~a~%"
                                 (heading-words (format nil
                                                        "Imported from ~a"
                                                        (native-file-name (calendar-import-job-path job))))))
                     entries (subseq text at))))))

(defmethod job-take-in ((job calendar-import-job))
  "Put the entries read into the calendars' file, and say how many."
  (let ((url (calendar-import-job-url job)))
    (cond ((calendar-import-job-failed job) (message "~a" (calendar-import-job-failed job)))
          ((zerop (calendar-import-job-count job))
           (message "No events in ~a" (abbreviated-path (calendar-import-job-path job))))
          (t
           (put-into-calendars (import-change job) job)
           (message "~:d event~:p imported into ~a~:[~; (sent at the next sync)~]"
                    (calendar-import-job-count job) (abbreviated-path (sync-file-path)) url)))))

(defun import-work (job)
  "Read JOB's calendar file as Org entries and work out the change they make
to the calendars' file: JOB's work. Why it could not be read is kept."
  (let ((path (calendar-import-job-path job)))
    (handler-case
        (progn
          (dolist (c (icalendar-read (octets-string (file-octets path :limit *icalendar-largest*))))
            (job-check-stop)
            (multiple-value-bind (text count) (calendar-entries c)
              (setf (calendar-import-job-entries job)
                    (concatenate 'string (or (calendar-import-job-entries job) "") text))
              (incf (calendar-import-job-count job) count)))
          (when (plusp (calendar-import-job-count job))
            (prepare-calendars-change job (import-change job))))
      (error (e)
        (setf (calendar-import-job-failed job) (format nil "~a not imported: ~a" (abbreviated-path path) e))))))

(define-command caldav-import ()
  "Read a calendar file (.ics) into the calendars' file: under one of your
calendars, whose server gets its events at the next sync, or under a
heading of its own, never synced. Read in the background."
  (let ((path (read-file-path "Import the calendar file: ")) (buffer (current-buffer)))
    (when path
      (let ((native-path (native (expand-path path))))
        (with-text-read
         (buffer-string (calendars-buffer)) "Reading the calendars' headings" #'calendar-headings
         (lambda (headings)
           (let ((target (completing-read "Import into: " (cons *not-synced* (mapcar #'car headings))
                                          :require-match t)))
             (when target
               (let ((job (make-calendar-import-job buffer native-path
                                                    (cdr (assoc target headings :test #'string=)))))
                 (note-calendars-file job)
                 (start-job-thread job #'import-work :name "tuatara caldav import")
                 (take-in-soon job 0.2 "Reading the calendar file"))))))))))

(define-record (calendar-export-job (:include job) (:constructor make-calendar-export-job (buffer entries path))
                                   (:copier nil))
  "Writing Org entries as a calendar file."
  (entries nil :documentation "The entries written, as ORG-AGENDA-FILE-ENTRIES gives them.")
  (path nil :documentation "Where the calendar file is written, a native path.")
  (count 0 :documentation "How many of the entries said a time and were written."))

(define-job-words calendar-export-job
  :doing "Writing a calendar file"
  :description (format nil "Writing ~a" (calendar-export-job-path job))
  :verb "write the calendar file"
  :left-words "nothing written")

(defmethod job-take-in ((job calendar-export-job))
  "Say how many entries were written, or that none had a time."
  (if (zerop (calendar-export-job-count job))
      (message "No entry with a time here to write as a calendar file")
      (message "~:d entr~:@p written to ~a"
               (calendar-export-job-count job) (abbreviated-path (calendar-export-job-path job)))))

(defun export-entries-work (job)
  "Write JOB's entries that say a time as one calendar file at JOB's path,
their rules held together to the periods of WITH-PERIODS-TOGETHER: JOB's work.
Nothing is written when none says a time."
  (with-periods-together
    (let ((calendars (loop for e in (calendar-export-job-entries job)
                           for c = (progn (job-check-stop) (ignore-errors (entry-calendar e)))
                           when (and c (some (lambda (x) (or (ical-property-of x "DTSTART")
                                                             (ical-property-of x "DUE")))
                                             (ical-components-of c)))
                             collect c)))
      (when calendars
        (let* ((zones (remove-duplicates (loop for c in calendars append (ical-components-of c "VTIMEZONE"))
                                         :key (lambda (z) (ical-value-of z "TZID")) :test #'equal))
               (events (loop for c in calendars append (calendar-events c)))
               (whole (make-ical-component "VCALENDAR"
                                           (ical-component-properties (first calendars))
                                           (append zones events))))
          (replace-file (calendar-export-job-path job) (icalendar-write whole))
          (setf (calendar-export-job-count job) (length calendars)))))))

(defun text-char-index (text octet)
  "The index in TEXT of the character its UTF-8 octet OCTET begins: how many
characters come before that octet."
  (let ((at 0) (index 0))
    (loop while (and (< index (length text)) (< at octet))
          do (incf at (char-octet-length (char text index)))
             (incf index))
    index))

(defun export-chosen-entries (name text region point)
  "The entries of TEXT, the Org file NAME, that CALDAV-EXPORT writes: those
REGION, a selection (FROM TO) in octets, touches, or with none the entry at
POINT, an octet position, and those under it."
  (let ((entries (org-agenda-file-entries name text)))
    (if region
        (let ((from (text-char-index text (first region)))
              (to (text-char-index text (second region))))
          (remove-if-not (lambda (e) (and (< (agenda-entry-get e :position) to) (> (agenda-entry-get e :end) from)))
                         entries))
        (let* ((at (text-char-index text point))
               (here (find-if (lambda (e) (<= (agenda-entry-get e :position) at)) entries :from-end t)))
          (and here (cons here (loop for e in (rest (member here entries))
                                     while (> (agenda-entry-get e :level) (agenda-entry-get here :level))
                                     collect e)))))))

(define-command caldav-export ()
  "Write the entry at point and those under it, or the entries selected, as
a calendar file (.ics) other calendars read: their times, title, place,
tags and description. The entries are read and written in the background."
  (let ((buffer (current-buffer)))
    (with-text-read
     (buffer-string buffer) "Reading the entries"
     (let ((name (or (buffer-file buffer) (buffer-name buffer)))
           (region (selected-region buffer))
           (point (buffer-point buffer)))
       (lambda (text) (export-chosen-entries name text region point)))
     (lambda (chosen)
       (if (null chosen)
           (message "No entry here to write as a calendar file")
           (let ((path (read-new-native-path "Write the calendar file: ")))
             (when path
               (let ((job (make-calendar-export-job buffer chosen path)))
                 (start-job-thread job #'export-entries-work :name "tuatara caldav export")
                 (take-in-soon job 0.2 "Writing the calendar file")))))))))

;;; New events

(defparameter *new-event-form*
  (format nil
          "* %^{Title}~%:PROPERTIES:~%:LOCATION: %^{Place (RET for none)}~%:END:~%%^T~%~
           #+name: description~%#+begin_example~%%?~%#+end_example")
  "The form CALDAV-EVENT-NEW opens: its title, place and time asked first,
in that order, then the description written between the block's two lines,
where point is.")

(define-command caldav-event-new ()
  "Write a new event for one of your calendars: choose the calendar (when there
is more than one), then give its title, its place (RET for none) and its time in
the calendar. A form opens with point in its description: write it there, or
nothing. \\<org-capture-mode>\\[org-capture-finalize] files the event under the
calendar's heading, and the server gets it at the next sync;
\\[org-capture-refile] files it under another heading, \\[org-capture-kill]
throws it away."
  (with-text-read
   (buffer-string (calendars-buffer)) "Reading the calendars' headings" #'calendar-headings
   (lambda (headings)
     (if (null headings)
         (message "No calendars synced yet: ~a syncs them" (substitute-command-keys "\\<org-mode>\\[caldav-sync]"))
         (let ((calendar (if (rest headings)
                             (completing-read "New event in: " (mapcar #'car headings) :require-match t)
                             (car (first headings)))))
           (when calendar
             (org-capture-with "Calendar event" (list :olp (sync-file-path) calendar) *new-event-form*)))))))

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
