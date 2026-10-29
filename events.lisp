;;;; events.lisp --- Calendar events and tasks written as Org entries, and entries read back as events.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; An event's times are written in Org's own words where Org has them: a
;;;; timestamp or a range of them, a repeater (+1w), a date by rule
;;;; (<%%(diary-float t 2 2)>), and a note in the logbook for each time
;;;; cancelled or moved. Times Org cannot say are written out one by one
;;;; within the days kept, the rule kept in the RRULE property, and are never
;;;; sent back. Times are on the system's clock. Words from a calendar are
;;;; untrusted: a title stays one line kept from markup, a description stays
;;;; inside its block, and neither becomes a heading, a link, a timestamp or
;;;; a note the agenda reads.

(in-package #:caldav)

(defparameter *uid-property* "UID"
  "The property of an entry that holds the UID of its event.")

(defparameter *rule-property* "RRULE"
  "The property of an entry whose times were written out one by one: the
rules of recurrence of its event as written. Its times are the calendar's and
are not sent back.")

(defparameter *description-name* "description"
  "The name of the example block that holds an entry's description.")

(defparameter *cancelled-tag* "cancelled"
  "The tag of an entry whose event is cancelled.")

(defparameter *days-written-before* 365
  "How many days back the times of an event Org cannot say are written out.")

(defparameter *days-written-after* 730
  "How many days ahead the times of an event Org cannot say are written out.")

(defparameter *most-times-written* 500
  "The most times of one event written out one by one: a calendar file holds
what it likes, and the rest of an event's times past these are not written.")

(defparameter *mark* (string (code-char #x200B))
  "The zero-width space set into words from a calendar where Org would read
them as something other than words; taken out again when they are sent back.")

(defparameter *managed-properties*
  '("UID" "SUMMARY" "LOCATION" "CATEGORIES" "STATUS" "DESCRIPTION" "DTSTAMP" "COMPLETED")
  "The properties of an event an entry says; the others are kept as the
calendar last had them.")

(defparameter *timing-properties*
  '("DTSTART" "DTEND" "DURATION" "DUE" "RRULE" "RDATE" "EXDATE")
  "The properties of an event that say its times.")

;;; Words

(defun tag-name (category)
  "CATEGORY as an Org tag: each run of characters a tag cannot hold (any but
letters, digits and _ @ # %) an underscore, none at its ends; NIL when
nothing is left."
  (let* ((marked (map 'string (lambda (c) (if (or (alphanumericp c) (find c "_@#%")) c #\Space)) category))
         (name (format nil "~{~a~^_~}" (uiop:split-string (string-trim " " marked) :separator " "))))
    (setf name (cl-ppcre:regex-replace-all "_{2,}" name "_"))
    (and (plusp (length name)) name)))

(defun agenda-guarded (text)
  "TEXT with a mark where the agenda would read it as more than words: after
each < that begins a timestamp, after CLOCK and CLOSED before their colon,
and after the - of a note's - State."
  (cl-ppcre:regex-replace-all "<(?=\\d|%%)|\\b(?:CLOCK|CLOSED)(?=:)|-(?=[ \\t]+State[ \\t])" text
                              (lambda (match) (concatenate 'string match *mark*))
                              :simple-calls t))

(defparameter *unguard-scanner*
  (cl-ppcre:create-scanner (format nil "(<|\\bCLOCK|\\bCLOSED|-)~a(?=\\d|%%|:|[ \\t]+State[ \\t])" *mark*))
  "The marks AGENDA-GUARDED sets, where it sets them, made ready once.")

(defun agenda-unguarded (text)
  "TEXT, written by AGENDA-GUARDED, as it was: as it is when it holds no mark."
  (if (find (char *mark* 0) text)
      (cl-ppcre:regex-replace-all *unguard-scanner* text "\\1")
      text))

(defun heading-words (summary)
  "SUMMARY, an event's title, as an entry's heading writes it: one line kept
from markup (ORG-SAFE-LINE), a mark before a first word Org could read as a
keyword or a priority, and after a last word it could read as tags."
  (let ((title (agenda-guarded (org-safe-line (one-line summary)))))
    (when (cl-ppcre:scan "^(?:[A-Z]{2,}(?:\\s|$)|\\[#)" title)
      (setf title (concatenate 'string *mark* title)))
    (when (cl-ppcre:scan "\\s:\\S+:$" title)
      (setf title (concatenate 'string title *mark*)))
    title))

(defun summary-words (title)
  "TITLE, an entry's, as an event's title: what HEADING-WORDS wrote it from."
  (org-safe-line-words (agenda-unguarded (string-trim *mark* title))))

(defun description-block (description)
  "DESCRIPTION written as an entry's description, an example block named
*DESCRIPTION-NAME*: its lines that would end the block, or begin a heading or
a keyword, escaped as Org escapes them, and marked where the agenda would
read more than words (AGENDA-GUARDED)."
  (let ((text (string-right-trim '(#\Newline) (remove #\Return description))))
    (format nil "#+name: ~a~%#+begin_example~%~a~%#+end_example~%" *description-name* (org-escape-block (agenda-guarded text)))))

(defun block-description (text start end)
  "The description of the entry in TEXT from START below END, from its block
named *DESCRIPTION-NAME*, as DESCRIPTION-BLOCK wrote it from; NIL when it has none."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings
       (format nil "(?ims)^[ \\t]*#\\+name:[ \\t]*~a[ \\t]*\\n[ \\t]*#\\+begin_example[^\\n]*\\n(.*?)^[ \\t]*#\\+end_example" *description-name*)
       text :start start :end end)
    (when match
      (agenda-unguarded (string-right-trim '(#\Newline) (org-unescape-block (aref groups 0)))))))

;;; Times on the system's clock

(defun day-text (day)
  "The day number DAY as a date by rule writes it: month, day, year (11 3 2026)."
  (multiple-value-bind (year month date) (day-date day)
    (format nil "~d ~d ~d" month date year)))

(defun local-timestamp (local &key end whole-day repeat)
  "The active timestamp of the local time LOCAL (a day number for WHOLE-DAY),
to END when it is given (the day after the last for WHOLE-DAY): one
timestamp for one day, a range of two for days apart. REPEAT, (VALUE UNIT),
is a repeater for it."
  (flet ((stamp (day &optional hour minute end-hour end-minute)
           (multiple-value-bind (year month date) (day-date day)
             (org-ts-text (make-org-ts :active t :year year :month month :day date :hour hour :minute minute
                                       :end-hour end-hour :end-minute end-minute
                                       :repeater (and repeat :cumulate) :repeat-value (first repeat) :repeat-unit (second repeat))))))
    (if whole-day
        (let ((last (max local (1- (or end (1+ local))))))
          (if (= last local)
              (stamp local)
              (format nil "~a--~a" (stamp local) (stamp last))))
        (multiple-value-bind (day seconds) (floor local 86400)
          (let ((hour (floor seconds 3600)) (minute (floor (mod seconds 3600) 60)))
            (if (or (null end) (= end local))
                (stamp day hour minute)
                (multiple-value-bind (end-day end-seconds) (floor end 86400)
                  (let ((end-hour (floor end-seconds 3600)) (end-minute (floor (mod end-seconds 3600) 60)))
                    (if (= end-day day)
                        (stamp day hour minute end-hour end-minute)
                        (format nil "~a--~a" (stamp day hour minute) (stamp end-day end-hour end-minute)))))))))))

(defun occurrence-of-local (local whole-day)
  "The local time LOCAL as an occurrence a note says, (DAY . TIME), TIME 930
for 09:30 or NIL for WHOLE-DAY."
  (multiple-value-bind (day seconds) (floor local 86400)
    (cons day (and (not whole-day) (+ (* 100 (floor seconds 3600)) (floor (mod seconds 3600) 60))))))

(defun event-span (component zones zone)
  "COMPONENT's times on ZONE's clock, as four values: its start and end
\(day numbers, the end the day after the last, when it is of whole days;
else local times), true when it is of whole days, and the ICAL-TIME it
starts at. NIL when it has no start."
  (multiple-value-bind (start own length) (component-span component zones zone)
    (when start
      (if (ical-time-date-p start)
          (let ((day (ical-time-day start)))
            (values day (+ day (max 1 (ceiling length 86400))) t start))
          (let ((local (local-in start zone zones zone)))
            (values local (zone-local-time zone (zone-universal-time own (+ (ical-local start) length))) nil start))))))

(defun repeat-of (recur start-day)
  "What Org's repeater says of RECUR, a rule of an event first on the day
number START-DAY, as (VALUE UNIT); NIL when Org's repeater says something else."
  (multiple-value-bind (year month date) (day-date start-day)
    (declare (ignore year))
    (let ((freq (ical-recur-freq recur)))
      (and (null (ical-recur-count recur)) (null (ical-recur-until recur))
           (null (ical-recur-by-second recur)) (null (ical-recur-by-minute recur)) (null (ical-recur-by-hour recur))
           (null (ical-recur-by-set-pos recur)) (null (ical-recur-by-week-no recur)) (null (ical-recur-by-year-day recur))
           (case freq
             (:daily (and (null (ical-recur-by-day recur)) (null (ical-recur-by-month-day recur)) (null (ical-recur-by-month recur))))
             (:weekly (and (member (ical-recur-by-day recur) (list nil (list (cons nil (weekday start-day)))) :test #'equal)
                           (null (ical-recur-by-month-day recur)) (null (ical-recur-by-month recur))))
             (:monthly (and (null (ical-recur-by-day recur)) (<= date 28)
                            (member (ical-recur-by-month-day recur) (list nil (list date)) :test #'equal)
                            (null (ical-recur-by-month recur))))
             (:yearly (and (null (ical-recur-by-day recur)) (not (and (= month 2) (= date 29)))
                           (member (ical-recur-by-month-day recur) (list nil (list date)) :test #'equal)
                           (member (ical-recur-by-month recur) (list nil (list month)) :test #'equal))))
           (list (ical-recur-interval recur)
                 (ecase freq (:daily #\d) (:weekly #\w) (:monthly #\m) (:yearly #\y)))))))

(defun rule-form (recur start-day last)
  "What a date by rule says of RECUR, a rule of an event first on the day
number START-DAY, its last day LAST or NIL, written as the repeat menu of
the date prompt writes it (diary-float t 2 2), without its first and last
days; and true when the form begins at START-DAY of itself. NIL when no form
says it."
  (multiple-value-bind (year month date) (day-date start-day)
    (let ((by-day (ical-recur-by-day recur)) (interval (ical-recur-interval recur))
          (month-day (ical-recur-by-month-day recur)) (by-month (ical-recur-by-month recur)))
      (flet ((cyclic (n day)
               (format nil "diary-cyclic ~d ~a" n (day-text day)))
             (plain-days ()
               (and (every (lambda (d) (null (car d))) by-day) (sort (mapcar #'cdr by-day) #'<))))
        (when (and (null (ical-recur-by-second recur)) (null (ical-recur-by-minute recur)) (null (ical-recur-by-hour recur))
                   (null (ical-recur-by-set-pos recur)) (null (ical-recur-by-week-no recur)) (null (ical-recur-by-year-day recur)))
          (case (ical-recur-freq recur)
            (:daily
             (when (and (null month-day) (null by-month))
               (let ((days (plain-days)))
                 (cond ((null by-day) (values (cyclic interval start-day) t))
                       ((null days) nil)
                       ((= interval 1) (format nil "memq (calendar-day-of-week date) '~a" days))
                       (t (values (format nil "and (~a) (memq (calendar-day-of-week date) '~a)" (cyclic interval start-day) days) t))))))
            (:weekly
             (when (and (null month-day) (null by-month))
               (let ((days (if by-day (plain-days) (list (weekday start-day)))))
                 (flet ((first-on (d) (+ start-day (mod (- d (weekday start-day)) 7))))
                   (cond ((null days) nil)
                         ((null (rest days)) (values (cyclic (* 7 interval) (first-on (first days))) t))
                         ((= interval 1) (format nil "memq (calendar-day-of-week date) '~a" days))
                         (t (values (format nil "or~{ (~a)~}" (mapcar (lambda (d) (cyclic (* 7 interval) (first-on d))) days)) t)))))))
            (:monthly
             (let ((months (repeat-months start-day interval)))
               (when (and months (null by-month))
                 (cond ((and (= (length by-day) 1) (car (first by-day)) (<= (abs (car (first by-day))) 5) (null month-day))
                        (format nil "diary-float ~a ~d ~d" months (cdr (first by-day)) (car (first by-day))))
                       ((and (null by-day) (member month-day (list nil (list date)) :test #'equal))
                        (format nil "diary-date ~a ~d t" months date))))))
            (:yearly
             (cond ((and (= interval 1) (= (length by-month) 1) (= (length by-day) 1) (car (first by-day))
                         (<= (abs (car (first by-day))) 5) (null month-day))
                    (format nil "diary-float ~d ~d ~d" (first by-month) (cdr (first by-day)) (car (first by-day))))
                   ((and (null by-day) (member by-month (list nil (list month)) :test #'equal)
                         (member month-day (list nil (list date)) :test #'equal))
                    (cond ((= interval 1) (format nil "diary-anniversary ~d ~d" month date))
                          (last (format nil "diary-date ~d ~d '~a" month date
                                        (loop for y from year to (day-date last) by interval collect y)))))))))))))

(defun rule-last-day (recur start-local whole-day zones zone)
  "The day number of the last time of RECUR, a rule beginning at the local
time START-LOCAL, on ZONE's clock, by its UNTIL or COUNT; NIL when it has
neither, or more times than are worked out."
  (cond ((ical-recur-until recur)
         (let ((until (ical-recur-until recur)))
           (floor (if (or whole-day (ical-time-date-p until)) (ical-local until) (local-in until zone zones zone)) 86400)))
        ((ical-recur-count recur)
         (when (<= (ical-recur-count recur) *icalendar-most-occurrences*)
           (let ((times (recur-expand recur start-local start-local (+ start-local (* 86400 366 400)) :date whole-day)))
             (and times (= (length times) (ical-recur-count recur))
                  (floor (car (last times)) 86400)))))))

(defun org-times-of (master changes zones zone now)
  "The times of MASTER, an event, and CHANGES, the components changing its
occurrences, as Org says them on ZONE's clock: a list of timestamps' texts
and a list of the notes of times cancelled and moved, noted at the universal
time NOW; NIL when Org cannot say them."
  (multiple-value-bind (start end whole-day start-time) (event-span master zones zone)
    (when start
      (let* ((rules (ical-properties-of master "RRULE"))
             (recur (and rules (read-ical-recur (ical-property-value (first rules)) (ical-time-zone start-time))))
             (local (if whole-day (* 86400 start) start))
             (start-day (floor local 86400))
             (one-day (if whole-day (= end (1+ start)) (= (floor end 86400) start-day)))
             (own-clock (or whole-day (null (ical-time-zone start-time))
                            (equal (ical-time-zone start-time) (time-zone-name zone))
                            (and (stringp (ical-time-zone start-time)) (eq (funcall zones (ical-time-zone start-time)) zone)))))
        (cond ((and (null rules) (null (ical-property-of master "RDATE")) (null changes))
               (list (list (local-timestamp start :end end :whole-day whole-day)) '()))
              ((or (rest rules) (null recur) (ical-property-of master "RDATE") (not one-day) (not own-clock))
               nil)
              (t
               (let ((notes '()))
                 (dolist (p (ical-properties-of master "EXDATE"))
                   (dolist (time (ical-property-times p))
                     (push (org-occurrence-note :cancelled (occurrence-of-local (if whole-day (ical-local time) (local-in time zone zones zone)) whole-day) nil now)
                           notes)))
                 (dolist (c changes)
                   (let* ((id (ical-property-time (ical-property-of c "RECURRENCE-ID")))
                          (from (and id (occurrence-of-local (if whole-day (ical-local id) (local-in id zone zones zone)) whole-day))))
                     (when from
                       (if (string-equal (or (ical-value-of c "STATUS") "") "CANCELLED")
                           (push (org-occurrence-note :cancelled from nil now) notes)
                           (multiple-value-bind (moved-start moved-end moved-whole) (event-span c zones zone)
                             (declare (ignore moved-end))
                             (let ((to (and moved-start (occurrence-of-local (if moved-whole (* 86400 moved-start) moved-start) moved-whole))))
                               (when (and to (not (equal to from)))
                                 (push (org-occurrence-note :moved from to now) notes))))))))
                 ;; more times cancelled or moved than are written out: its times written out instead
                 (when (> (length notes) *most-times-written*)
                   (return-from org-times-of nil))
                 (let ((repeat (repeat-of recur start-day)))
                   (if repeat
                       (list (list (local-timestamp start :end end :whole-day whole-day :repeat repeat)) (nreverse notes))
                       (let ((last (rule-last-day recur local whole-day zones zone)))
                         (multiple-value-bind (form from-start) (rule-form recur start-day last)
                           (when (and form (or last (and (null (ical-recur-count recur)) (null (ical-recur-until recur)))))
                             (let* ((time (and (not whole-day) (org-ts-time-text (org-read-ts (local-timestamp start :end end)))))
                                    (text (if (or last (not from-start))
                                              (repeat-rule-text form time start-day (or last (repeat-open-end start-day)))
                                              (repeat-rule-text form time)))
                                    (rule (org-read-rule text)))
                               ;; the form must take the event's first day, or it says other days than the event's
                               (when (and rule (eql (org-rule-next rule start-day) start-day))
                                 (list (list text) (nreverse notes))))))))))))))))

(defun times-written-out (members calendar zone from to)
  "The occurrences of MEMBERS, an event and the components changing it, of
CALENDAR, from the universal time FROM below TO, those not cancelled, as
timestamps' texts on ZONE's clock, *MOST-TIMES-WRITTEN* at most."
  (let ((zones (ical-components-of calendar "VTIMEZONE"))
        ;; a rule of hours, minutes or seconds would be a timestamp each: its first time alone
        (most (if (some (lambda (p) (let ((r (read-ical-recur (ical-property-value p))))
                                      (and r (member (ical-recur-freq r) '(:hourly :minutely :secondly)))))
                        (ical-properties-of (first members) "RRULE"))
                  1
                  *most-times-written*)))
    (loop for o in (icalendar-occurrences (make-ical-component "VCALENDAR" '() (append zones members)) from to :zone zone)
          for count from 0 below most
          unless (string-equal (or (ical-value-of (ical-occurrence-component o) "STATUS") "") "CANCELLED")
            collect (if (ical-occurrence-whole-day o)
                        (local-timestamp (ical-occurrence-start o) :end (ical-occurrence-end o) :whole-day t)
                        (local-timestamp (zone-local-time zone (ical-occurrence-start o))
                                         :end (zone-local-time zone (ical-occurrence-end o)))))))

;;; Events written as entries

(defun event-entry (members calendar &key (level 2) (zone (system-time-zone)) (now (get-universal-time))
                                          (from (- now (* 86400 *days-written-before*)))
                                          (to (+ now (* 86400 *days-written-after*))))
  "The Org entry, a heading of LEVEL stars and its text, for MEMBERS, an event
or a task of CALENDAR and the components changing its occurrences
\(UID-GROUPS), on ZONE's clock. Times Org cannot say are written out from
the universal time FROM below TO; notes are noted at NOW."
  (let* ((master (or (find-if-not (lambda (c) (ical-property-of c "RECURRENCE-ID")) members) (first members)))
         (changes (remove master members))
         (zones (calendar-zones calendar))
         (task (string-equal (ical-component-name master) "VTODO"))
         (status (string-upcase (or (ical-value-of master "STATUS") "")))
         (done (and task (or (string= status "COMPLETED") (ical-property-of master "COMPLETED"))))
         (tags (remove-duplicates
                (append (loop for p in (ical-properties-of master "CATEGORIES")
                              nconc (loop for c in (ical-texts (ical-property-value p)) for tag = (tag-name c) when tag collect tag))
                        (and (string= status "CANCELLED") (list *cancelled-tag*)))
                :test #'string= :from-end t))
         (summary (ical-text (or (ical-value-of master "SUMMARY") "")))
         (location (let ((v (ical-value-of master "LOCATION"))) (and v (one-line (ical-text v)))))
         (description (let ((v (ical-value-of master "DESCRIPTION"))) (and v (ical-text v))))
         (uid (ical-value-of master "UID"))
         (times (and (not task) (org-times-of master changes zones zone now)))
         (written-out (and (not task) (null times)
                           (times-written-out members calendar zone from to))))
    (with-output-to-string (out)
      (format out "~a ~a~a~@[ :~{~a~^:~}:~]~%"
              (make-string level :initial-element #\*) (cond ((not task) "") (done "DONE ") (t "TODO "))
              (heading-words (if (string= summary "") "(no title)" summary)) tags)
      (when task
        (flet ((planned (name)
                 (let ((time (ical-property-time (ical-property-of master name))))
                   (and time (local-timestamp (if (ical-time-date-p time) (ical-time-day time) (local-in time zone zones zone))
                                              :whole-day (ical-time-date-p time))))))
          (let ((deadline (planned "DUE")) (scheduled (planned "DTSTART"))
                (closed (let* ((time (ical-property-time (ical-property-of master "COMPLETED")))
                               (local (and time (local-in time zone zones zone))))
                          (and local (org-day-timestamp-text (floor local 86400) :hour (floor (mod local 86400) 3600)
                                                                                 :minute (floor (mod local 3600) 60))))))
            (when (or deadline scheduled closed)
              (format out "~{~a~^ ~}~%" (remove nil (list (and closed (format nil "CLOSED: ~a" closed))
                                                          (and deadline (format nil "DEADLINE: ~a" deadline))
                                                          (and scheduled (format nil "SCHEDULED: ~a" scheduled)))))))))
      (format out ":PROPERTIES:~%")
      (when uid (format out ":~a: ~a~%" *uid-property* (agenda-guarded (one-line uid))))
      (when location (format out ":LOCATION: ~a~%" (agenda-guarded location)))
      (when written-out
        (format out ":~a: ~{~a~^ ~}~%" *rule-property*
                (or (loop for p in (ical-properties-of master "RRULE") collect (agenda-guarded (one-line (ical-property-value p)))) (list "RDATE"))))
      (format out ":END:~%")
      (when (second times)
        (format out ":LOGBOOK:~%~{~a~%~}:END:~%" (second times)))
      (dolist (stamp (or (first times) written-out)) (format out "~a~%" stamp))
      (when (and description (plusp (length (string-trim '(#\Space #\Newline #\Return) description))))
        (write-string (description-block description) out)))))

(defun calendar-entries (calendar &rest arguments &key (level 2) zone now from to)
  "The Org entries of the events and tasks of CALENDAR, a VCALENDAR, each a
heading of LEVEL stars, as EVENT-ENTRY writes them with ZONE, NOW, FROM and
TO; how many as a second value."
  (declare (ignore level zone now from to))
  (let ((groups (uid-groups calendar)))
    (values (format nil "~{~a~}" (loop for members in groups collect (apply #'event-entry members calendar arguments)))
            (length groups))))

;;; Entries read back as events

(defun zone-tzid (zone)
  "The TZID ZONE is written with: its name, when it is a zone's name; NIL when it has none, and times are written in UTC."
  (let ((name (time-zone-name zone)))
    (and name (zone-name-p name) name)))

(defun local-ical-time (local zone)
  "The local time LOCAL on ZONE's clock as an ICAL-TIME: in ZONE by its name, else in UTC."
  (let ((tzid (zone-tzid zone)))
    (if tzid
        (make-ical-time (floor local 86400) (mod local 86400) tzid)
        (let ((universal (zone-universal-time zone local)))
          (universal-ical-time universal)))))

(defun ts-local (ts)
  "TS's start as a local time, midnight when it has no time."
  (+ (* 86400 (org-ts-day-number ts)) (* 3600 (or (org-ts-hour ts) 0)) (* 60 (or (org-ts-minute ts) 0))))

(defun timing-of-stamp (text zone)
  "The properties saying the times of TEXT, a timestamp or a range of two, on
ZONE's clock: DTSTART, DTEND, and RRULE for a repeater; NIL when TEXT is none."
  (multiple-value-bind (ts until) (stamp-ends text)
    (when ts
      (append
       (if (or (null (org-ts-hour ts)) (and until (null (org-ts-hour until))))
           (let ((day (org-ts-day-number ts)))
             (list (ical-time-property "DTSTART" (make-ical-time day))
                   (ical-time-property "DTEND" (make-ical-time (1+ (max day (if until (org-ts-day-number until) day)))))))
           (let* ((start (ts-local ts))
                  (end (cond (until (ts-local until))
                             ((org-ts-end-hour ts)
                              (let ((end (+ (* 86400 (org-ts-day-number ts)) (* 3600 (org-ts-end-hour ts)) (* 60 (org-ts-end-minute ts)))))
                                (if (< end start) (+ end 86400) end))))))
             (list* (ical-time-property "DTSTART" (local-ical-time start zone))
                    (and end (> end start) (list (ical-time-property "DTEND" (local-ical-time end zone)))))))
       (when (and (org-ts-repeat-value ts) (plusp (org-ts-repeat-value ts)))
         (list (make-ical-property "RRULE"
                                   (format nil "FREQ=~a~@[;INTERVAL=~d~]"
                                           (ecase (org-ts-repeat-unit ts) (#\h "HOURLY") (#\d "DAILY") (#\w "WEEKLY") (#\m "MONTHLY") (#\y "YEARLY"))
                                           (and (/= 1 (org-ts-repeat-value ts)) (org-ts-repeat-value ts))))))))))

(defun evenly-interval (months)
  "The interval of MONTHS, a list of months a date by rule takes, when they
come evenly through the year (every 2 months for six of them), and the first
of them; NIL when they do not."
  (let* ((sorted (sort (copy-list months) #'<)) (count (length sorted)))
    (when (and (plusp count) (zerop (mod 12 count))
               (loop for (a b) on sorted while b always (= (- b a) (/ 12 count))))
      (values (/ 12 count) (first sorted)))))

(defun recur-of-rule (rule)
  "What RULE, a date by rule as the repeat menu writes it, says as a rule of
recurrence: an ICAL-RECUR with no end, and the first and last day numbers it
gives, or NIL for each; NIL when no rule of recurrence says it."
  (let ((parts (if (eq (org-rule-kind rule) :all) (first (org-rule-arguments rule)) (list rule)))
        (first nil) (last nil))
    (let ((block (find :block parts :key #'org-rule-kind)))
      (when block
        (setf parts (remove block parts)
              first (first (org-rule-arguments block))
              last (second (org-rule-arguments block)))
        (when (= last (repeat-open-end first)) (setf last nil))))
    (flet ((kind (k) (find k parts :key #'org-rule-kind)))
      (let ((recur
              (cond
                ;; every so many days on some days of the week
                ((and (= (length parts) 2) (kind :cyclic) (kind :weekdays))
                 (destructuring-bind (n start) (org-rule-arguments (kind :cyclic))
                   (setf first (or first start))
                   (make-ical-recur :daily :interval n :by-day (mapcar (lambda (wd) (cons nil wd)) (sort (copy-list (first (org-rule-arguments (kind :weekdays)))) #'<)))))
                ((rest parts) nil)
                (t
                 (let ((main (first parts)))
                   (destructuring-bind (&optional a b c d) (org-rule-arguments main)
                     (case (org-rule-kind main)
                       (:float (and (integerp b) (null d)
                                    (cond ((eq a t) (make-ical-recur :monthly :by-day (list (cons c b))))
                                          ((integerp a) (make-ical-recur :yearly :by-month (list a) :by-day (list (cons c b))))
                                          ((consp a) (let ((interval (evenly-interval a)))
                                                       (and interval (make-ical-recur :monthly :interval interval :by-day (list (cons c b)))))))))
                       (:weekdays (make-ical-recur :weekly :by-day (mapcar (lambda (wd) (cons nil wd)) (sort (copy-list a) #'<))))
                       (:cyclic (setf first (or first b))
                        (if (zerop (mod a 7)) (make-ical-recur :weekly :interval (/ a 7)) (make-ical-recur :daily :interval a)))
                       ;; every so many weeks on several days: a cycle of weeks each
                       (:any (let ((cycles (mapcar #'org-rule-arguments a)))
                               (when (and (every (lambda (r) (eq (org-rule-kind r) :cyclic)) a)
                                          (every (lambda (c) (and (= (first c) (first (first cycles))) (zerop (mod (first c) 7)))) cycles))
                                 (setf first (or first (reduce #'min cycles :key #'second)))
                                 (make-ical-recur :weekly :interval (/ (first (first cycles)) 7)
                                                          :by-day (sort (mapcar (lambda (c) (cons nil (weekday (second c)))) cycles) #'< :key #'cdr)))))
                       (:date (cond ((and (eq a t) (integerp c) (eq b t)) (make-ical-recur :monthly :by-month-day (list c)))
                                    ((and (eq a t) (integerp c) (integerp b)) (make-ical-recur :yearly :by-month (list b) :by-month-day (list c)))
                                    ((and (eq a t) (integerp c) (consp b))
                                     (let ((interval (evenly-interval b)))
                                       (and interval (make-ical-recur :monthly :interval interval :by-month-day (list c)))))
                                    ((and (consp a) (integerp b) (integerp c) (rest a))
                                     (let* ((years (sort (copy-list a) #'<)) (step (- (second years) (first years))))
                                       (when (and (plusp step) (loop for (x y) on years while y always (= (- y x) step)))
                                         (setf first (or first (day-number (first years) b c))
                                               last (or last (day-number (car (last years)) b c)))
                                         (make-ical-recur :yearly :interval step :by-month (list b) :by-month-day (list c)))))))
                       (:anniversary (setf first (or first (and c (day-number c a b))))
                        (make-ical-recur :yearly :by-month (list a) :by-month-day (list b))))))))))
        (and recur (values recur first last))))))

(defun timing-of-rule (text zone today agreed-start)
  "The properties saying the times of TEXT, a timestamp by rule, on ZONE's
clock: DTSTART, DTEND and RRULE, the first time that of its range of days,
else AGREED-START (a day number) when the rule takes it, else the first the
rule takes from the day number TODAY; NIL when no rule of recurrence says it."
  (let ((rule (org-read-rule text)))
    (when rule
      (multiple-value-bind (recur first last) (recur-of-rule rule)
        (when recur
          (let ((first (or (and first (org-rule-next rule first))
                           (and agreed-start (eql (org-rule-next rule agreed-start) agreed-start) agreed-start)
                           (org-rule-next rule today))))
            (when first
              (let* ((hour (org-rule-hour rule))
                     (start (+ (* 86400 first) (* 3600 (or hour 0)) (* 60 (or (org-rule-minute rule) 0))))
                     (end (and hour (org-rule-end-hour rule)
                               (+ (* 86400 first) (* 3600 (org-rule-end-hour rule)) (* 60 (org-rule-end-minute rule))))))
                (when last
                  (setf (ical-recur-until recur)
                        (if hour
                            (let ((u (zone-universal-time zone (+ (* 86400 last) (mod start 86400)))))
                              (universal-ical-time u))
                            (make-ical-time last))))
                (append (if hour
                            (list* (ical-time-property "DTSTART" (local-ical-time start zone))
                                   (and end (> end start) (list (ical-time-property "DTEND" (local-ical-time end zone)))))
                            (list (ical-time-property "DTSTART" (make-ical-time first))
                                  (ical-time-property "DTEND" (make-ical-time (1+ first)))))
                        (list (make-ical-property "RRULE" (ical-recur-text recur))))))))))))

(defun entry-description (entry)
  "ENTRY's description, from its block (BLOCK-DESCRIPTION), or NIL."
  (block-description (agenda-entry-text entry) (agenda-entry-position entry) (agenda-entry-end entry)))

(defun unless-blank (text)
  "TEXT, or NIL when it is NIL or holds nothing but spaces and line breaks."
  (and text (string/= "" (string-trim '(#\Space #\Tab #\Newline #\Return) text)) text))

(defun entry-property (entry name)
  "The value of ENTRY's property NAME, or NIL."
  (let ((value (cdr (assoc name (agenda-entry-properties entry) :test #'string-equal))))
    (and value (agenda-unguarded value))))

(defun uid-day (uid text)
  "The UID of the event of TEXT, a timestamp of an entry whose UID is UID
other than its first: UID and the day it says (UID-20261103)."
  (let ((ts (stamp-ends text)))
    (if ts
        (multiple-value-bind (year month day) (day-date (org-ts-day-number ts))
          (format nil "~a-~4,'0d~2,'0d~2,'0d" uid year month day))
        (format nil "~a-~36r" uid (string-hash text)))))

(defun occurrence-time (occurrence start zone)
  "OCCURRENCE, (DAY . TIME) as a note says it, as an ICAL-TIME of the kind of
START, the ICAL-TIME of its event's first time: a date for a date, else its
time (the first time's when the note gives none) on ZONE's clock."
  (destructuring-bind (day . time) occurrence
    (if (ical-time-date-p start)
        (make-ical-time day)
        (let ((seconds (if time (+ (* 3600 (floor time 100)) (* 60 (mod time 100)))
                           (mod (if (stringp (ical-time-zone start)) (ical-time-second start)
                                    (zone-local-time zone (ical-local start)))
                                86400))))
          (local-ical-time (+ (* 86400 day) seconds) zone)))))

(defun changed-properties (kept managed)
  "The properties of an event: KEPT, those it had, in their order, each of a
name MANAGED gives replaced by MANAGED's of that name in its place, and
MANAGED's others after."
  (let ((names (remove-duplicates (mapcar #'ical-property-name managed) :test #'string-equal))
        (placed '()))
    (append (loop for p in kept
                  for name = (ical-property-name p)
                  if (member name names :test #'string-equal)
                    nconc (unless (member name placed :test #'string-equal)
                            (push name placed)
                            ;; a copy: REMOVE may give MANAGED's own tail, which NCONC would lengthen
                            (copy-list (remove name managed :key #'ical-property-name :test-not #'string-equal)))
                  else unless (or (member name *managed-properties* :test #'string-equal)
                                  (member name *timing-properties* :test #'string-equal))
                         collect p)
            (remove-if (lambda (p) (member (ical-property-name p) placed :test #'string-equal)) managed))))

(defun agreed-members (agreed uid)
  "The components of AGREED, a VCALENDAR or NIL, of UID: the one recurring first, those changing it after."
  (and agreed (group-of-uid agreed uid)))

(defun agreed-times-hold-p (members agreed zone stamp cancelled moved)
  "True when MEMBERS, an event as agreed with the calendar (of AGREED, its
VCALENDAR), says the same times as an entry with the timestamp STAMP and the
times CANCELLED and MOVED (ORG-OCCURRENCE-NOTES): as Org would write them."
  (let* ((master (first members))
         (said (org-times-of master (rest members) (calendar-zones agreed) zone 0)))
    (and said
         (equal (first said) (list stamp))
         (let ((text (format nil "~{~a~%~}" (second said))))
           (multiple-value-bind (c m) (org-occurrence-notes text 0 (length text))
             (and (null (set-exclusive-or c cancelled :test #'equal))
                  (null (set-exclusive-or m moved :test #'equal))))))))

(defun new-uid ()
  "A UID for an event made here, no other like it."
  (format nil "~(~a~)@tuatara" (random-name)))

(defun stamp-ends (text)
  "TEXT, a timestamp or a range of two, read: its first timestamp and its
second, or NIL for a single one (ORG-READ-TS each)."
  (let ((dash (search ">--<" text)))
    (values (org-read-ts (if dash (subseq text 0 (1+ dash)) text))
            (and dash (org-read-ts (subseq text (+ dash 3)))))))

(defun repeating-p (text)
  "True when TEXT, a timestamp of an entry, says a time that repeats: by rule, or with a repeater."
  (or (org-rule-text-p text)
      (let ((ts (stamp-ends text)))
        (and ts (org-ts-repeat-value ts) (plusp (org-ts-repeat-value ts))))))

(defun entry-components (entry &key agreed uid (zone (system-time-zone)) (now (get-universal-time)))
  "The events or the task ENTRY, an agenda entry (ORG-AGENDA-FILE-ENTRIES),
says, on ZONE's clock: a list of components, each event's changes of its
occurrences after it. Its UID property names the event of its first time;
another time is another event, its UID that and the day. AGREED, a VCALENDAR
holding the events as the calendar last had them, gives what an entry does
not say: the properties it keeps, the categories as written, and the times of
an event whose times the entry does not say (written out, or said as the
calendar said them). UID, when given, is the UID of an entry that has none
yet (NEW-UID); DTSTAMP is NOW."
  (let* ((uid (one-line (or (entry-property entry *uid-property*) uid (new-uid))))
         (local-tags (agenda-entry-local-tags entry))
         (cancelled-tag (member *cancelled-tag* local-tags :test #'string=))
         (tags (remove *cancelled-tag* local-tags :test #'string=))
         ;; a field left blank, as a form leaves it, is none
         (description (unless-blank (entry-description entry)))
         (location (unless-blank (entry-property entry "LOCATION")))
         (stamps (agenda-entry-stamps entry))
         (ranges (mapcar (lambda (r) (format nil "~a--~a" (first r) (second r))) (agenda-entry-ranges entry)))
         (task (and (agenda-entry-todo-type entry) (agenda-entry-deadline entry) (null stamps) (null ranges)))
         (times (cond (task nil)
                      ((or stamps ranges)
                       ;; in the order written
                       (let ((text (agenda-entry-text entry)) (from (agenda-entry-position entry)))
                         (sort (append stamps ranges) #'<
                               :key (lambda (stamp) (or (search stamp text :start2 from :end2 (agenda-entry-end entry)) 0)))))
                      ((and (agenda-entry-scheduled entry) (char= (char (agenda-entry-scheduled entry) 0) #\<))
                       (list (agenda-entry-scheduled entry)))))
         (stamp-utc (let ((u now)) (universal-ical-time u)))
         (today (floor (zone-local-time zone now) 86400)))
    (multiple-value-bind (cancelled moved)
        (org-occurrence-notes (agenda-entry-text entry) (agenda-entry-position entry) (agenda-entry-end entry))
      (flet ((said (kept-master kind status)
               ;; what the entry says of an event, its kept properties KEPT-MASTER's
               (let ((categories (loop for tag in tags
                                       collect (or (and kept-master
                                                        (loop for p in (ical-properties-of kept-master "CATEGORIES")
                                                              thereis (find tag (ical-texts (ical-property-value p)) :key #'tag-name :test #'equal)))
                                                   tag))))
                 (remove nil
                         (list (make-ical-property "SUMMARY" (ical-text-escaped (summary-words (agenda-entry-title entry))))
                               (and location (make-ical-property "LOCATION" (ical-text-escaped location)))
                               (and categories (make-ical-property "CATEGORIES" (ical-texts-escaped categories)))
                               (and status (make-ical-property "STATUS" status))
                               (and description (make-ical-property "DESCRIPTION" (ical-text-escaped description)))
                               (ical-time-property "DTSTAMP" stamp-utc)
                               (and (eq kind :changed) kept-master (ical-value-of kept-master "SEQUENCE")
                                    (make-ical-property "SEQUENCE" (princ-to-string (1+ (or (ignore-errors (parse-integer (ical-value-of kept-master "SEQUENCE"))) 0)))))))))
             (status-of (kept-master)
               (let ((was (and kept-master (string-upcase (or (ical-value-of kept-master "STATUS") "")))))
                 (cond (cancelled-tag "CANCELLED")
                       ((and was (plusp (length was)) (string/= was "CANCELLED")) was)))))
        (cond
          (task
           (let* ((members (agreed-members agreed uid))
                  (kept (first members))
                  (done (string= (symbol-name (agenda-entry-todo-type entry)) "DONE"))
                  (timing (flet ((planned (name text)
                                   (let ((props (timing-of-stamp text zone)))
                                     (and props (let ((p (first props))) (make-ical-property name (ical-property-value p) (ical-property-parameters p)))))))
                            (remove nil (list (planned "DUE" (agenda-entry-deadline entry))
                                              (and (agenda-entry-scheduled entry) (planned "DTSTART" (agenda-entry-scheduled entry)))))))
                  (closed (and done (agenda-entry-closed entry) (org-read-ts (agenda-entry-closed entry))))
                  (status (cond (done "COMPLETED")
                                ((and kept (string-equal (or (ical-value-of kept "STATUS") "") "COMPLETED")) "NEEDS-ACTION")
                                (t (status-of kept)))))
             (list (make-ical-component
                    "VTODO"
                    (changed-properties (if kept (ical-component-properties kept) '())
                                        (append (list (make-ical-property "UID" uid))
                                                (said kept :task status)
                                                timing
                                                (and closed (list (ical-time-property "COMPLETED"
                                                                                      (let ((u (zone-universal-time zone (ts-local closed))))
                                                                                        (universal-ical-time u)))))))))))
          ((or (entry-property entry *rule-property*) (and (null times) (agreed-members agreed uid)))
           ;; times written out, or none written (all before the days written): the
           ;; calendar's stand, the rest as the entry says
           (let* ((members (agreed-members agreed uid)) (kept (first members)))
             (when kept
               (cons (make-ical-component (ical-component-name kept)
                                          (changed-properties (ical-component-properties kept)
                                                              (append (list (make-ical-property "UID" uid))
                                                                      (said kept :same (status-of kept))
                                                                      (remove-if-not (lambda (p) (member (ical-property-name p) *timing-properties* :test #'string-equal))
                                                                                     (ical-component-properties kept)))))
                     (rest members)))))
          (t
           (loop with noted = (find-if #'repeating-p times)
                 for text in times
                 for first = t then nil
                 for this-uid = (if first uid (uid-day uid text))
                 for members = (agreed-members agreed this-uid)
                 for kept = (first members)
                 for notes-here = (eq text noted)
                 for hold = (and kept (agreed-times-hold-p members agreed zone text (and notes-here cancelled) (and notes-here moved)))
                 for timing = (cond (hold (remove-if-not (lambda (p) (member (ical-property-name p) *timing-properties* :test #'string-equal))
                                                         (ical-component-properties kept)))
                                    ((org-rule-text-p text)
                                     (timing-of-rule text zone today
                                                     (let ((s (and kept (ical-property-time (ical-property-of kept "DTSTART")))))
                                                       (and s (ical-time-day s)))))
                                    (t (timing-of-stamp text zone)))
                 when timing
                   nconc (let* ((start (ical-property-time (find "DTSTART" timing :key #'ical-property-name :test #'string-equal)))
                                (end (ical-property-time (find "DTEND" timing :key #'ical-property-name :test #'string-equal)))
                                (length (and start end (- (ical-local end) (ical-local start))))
                                (exdates (and notes-here (not hold) cancelled
                                              (list (let ((times (mapcar (lambda (o) (occurrence-time o start zone)) cancelled)))
                                                      (make-ical-property "EXDATE" (format nil "~{~a~^,~}" (mapcar #'ical-time-text times))
                                                                          (ical-property-parameters (ical-time-property "EXDATE" (first times))))))))
                                (master (make-ical-component (if kept (ical-component-name kept) "VEVENT")
                                                             (changed-properties (if kept (ical-component-properties kept) '())
                                                                                 (append (list (make-ical-property "UID" this-uid))
                                                                                         (said kept (if hold :same :changed) (status-of kept))
                                                                                         timing exdates)))))
                           (cons master
                                 (if hold
                                     (rest members)
                                     (and notes-here
                                          (loop for (from . to) in moved
                                                for id = (occurrence-time from start zone)
                                                for new = (occurrence-time to start zone)
                                                for was = (find-if (lambda (c) (equal (ical-value-of c "RECURRENCE-ID") (ical-time-text id))) (rest members))
                                                collect (make-ical-component
                                                         (ical-component-name master)
                                                         (changed-properties
                                                          (if was (ical-component-properties was) '())
                                                          (append (list (make-ical-property "UID" this-uid)
                                                                        (ical-time-property "RECURRENCE-ID" id))
                                                                  (said nil :changed (status-of nil))
                                                                  (list (ical-time-property "DTSTART" new))
                                                                  (and length (list (ical-time-property "DTEND"
                                                                                                        (if (ical-time-date-p new)
                                                                                                            (make-ical-time (+ (ical-time-day new) (floor length 86400)))
                                                                                                            (let ((l (+ (ical-local new) length)))
                                                                                                              (make-ical-time (floor l 86400) (mod l 86400) (ical-time-zone new))))))))))))))))))))))

(defun entry-calendar (entry &rest arguments &key agreed uid zone now)
  "A VCALENDAR of what ENTRY says (ENTRY-COMPONENTS, with AGREED, UID, ZONE
and NOW), as COMPONENTS-CALENDAR makes one."
  (declare (ignore uid zone now))
  (components-calendar (apply #'entry-components entry arguments) agreed))

(defun components-calendar (components agreed)
  "A VCALENDAR of COMPONENTS, with the VTIMEZONE of each zone their times
name: AGREED's own (a VCALENDAR, or NIL), else one written from the system's
zone of that name."
  (let* ((tzids (remove-duplicates
                 (loop for c in components
                       nconc (loop for p in (ical-component-properties c)
                                   for tzid = (ical-parameter-of p "TZID")
                                   when tzid collect tzid))
                 :test #'string=)))
    (make-ical-component
     "VCALENDAR"
     (list (make-ical-property "VERSION" "2.0") (make-ical-property "PRODID" "-//Tuatara//caldav//EN"))
     (append (loop for tzid in tzids
                   for zone = (zone-named tzid)
                   for kept = (and agreed (calendar-vtimezone agreed tzid))
                   when (or kept zone) collect (or kept (zone-vtimezone zone)))
             components))))
