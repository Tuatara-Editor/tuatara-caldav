;;;; zones.lisp --- A calendar file's time zones: its VTIMEZONE components read, and written for a zone.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; RFC 5545, section 3.6.5. A VTIMEZONE is observances, STANDARD and
;;;; DAYLIGHT, each the local time it begins (in the offset before it), the
;;;; offsets before and after, and when it begins again: dates, or a rule.
;;;; Read, it becomes a time zone as the system's are in Tuatara: a
;;;; table of changes, and the yearly rule after it where the last rules are
;;;; one a zoneinfo rule can say.

(in-package #:caldav)

(defparameter *zone-years* 2100
  "The last year a time zone's changes are worked out to when its rules are none a yearly rule can say.")

(defun rule-day-of-recur (recur)
  "The day of a ZONE-RULE that RECUR, a rule of a VTIMEZONE observance, says:
yearly in one month on the Nth or last weekday, or on the weekday of seven
days in a row beginning on the 1st, 8th, 15th or 22nd; NIL for any other."
  (let ((months (ical-recur-by-month recur)) (days (ical-recur-by-day recur)) (month-days (ical-recur-by-month-day recur)))
    (when (and (eq (ical-recur-freq recur) :yearly) (= 1 (ical-recur-interval recur))
               (null (ical-recur-count recur)) (null (ical-recur-until recur))
               (= 1 (length months)) (= 1 (length days))
               (not (or (ical-recur-by-year-day recur) (ical-recur-by-week-no recur) (ical-recur-by-set-pos recur)
                        (ical-recur-by-hour recur) (ical-recur-by-minute recur) (ical-recur-by-second recur))))
      (destructuring-bind ((ordinal . wd)) days
        (cond ((and ordinal (null month-days) (or (<= 1 ordinal 4) (= ordinal -1)))
               (list :month (first months) (if (= ordinal -1) 5 ordinal) wd))
              ((and (null ordinal) (= 7 (length month-days)))
               (let ((low (reduce #'min month-days)))
                 (when (and (member low '(1 8 15 22)) (equal (sort (copy-list month-days) #'<) (loop for d from low below (+ low 7) collect d)))
                   (list :month (first months) (1+ (floor low 7)) wd)))))))))

(defparameter *longest-zone-name* 200
  "The longest name of a time zone looked for: a TZID longer is no zone's.")

(defparameter *most-observances* 100
  "The most observances (STANDARD and DAYLIGHT) of one VTIMEZONE read; the rest are passed over.")

(defun vtimezone-zone (component)
  "The time zone COMPONENT, a VTIMEZONE, says, named by its TZID; NIL when it
says none. *MOST-OBSERVANCES* of it are read at most."
  (let ((observances '()))
    (dolist (o (let ((all (ical-components-of component))) (subseq all 0 (min (length all) *most-observances*))))
      (let ((start (ical-property-time (ical-property-of o "DTSTART")))
            (from (let ((v (ical-value-of o "TZOFFSETFROM"))) (and v (read-utc-offset v))))
            (to (let ((v (ical-value-of o "TZOFFSETTO"))) (and v (read-utc-offset v)))))
        (when (and (member (ical-component-name o) '("STANDARD" "DAYLIGHT") :test #'string=) start from to)
          (push (list :daylight (string= (ical-component-name o) "DAYLIGHT")
                      :start (ical-local start)
                      :from from :to to
                      :name (let ((v (ical-value-of o "TZNAME"))) (and v (ical-text v)))
                      :rules (loop for p in (ical-properties-of o "RRULE")
                                   for r = (read-ical-recur (ical-property-value p))
                                   when r collect r)
                      :dates (loop for p in (ical-properties-of o "RDATE")
                                   nconc (loop for time in (ical-property-times p)
                                               collect (ical-local time))))
                observances))))
    (when observances
      (let* ((open (remove-if-not (lambda (o) (some (lambda (r) (not (or (ical-recur-count r) (ical-recur-until r)))) (getf o :rules)))
                                  observances))
             (daylight (find t open :key (lambda (o) (getf o :daylight))))
             (standard (find nil open :key (lambda (o) (getf o :daylight))))
             (day-of (lambda (o) (and o (= 1 (length (getf o :rules))) (rule-day-of-recur (first (getf o :rules))))))
             (rule (cond ((and daylight standard (funcall day-of daylight) (funcall day-of standard) (= 2 (length open)))
                          (make-zone-rule (or (getf standard :name) "") (getf standard :to)
                                          (or (getf daylight :name) "") (getf daylight :to)
                                          (list (funcall day-of daylight) (mod (getf daylight :start) 86400))
                                          (list (funcall day-of standard) (mod (getf standard :start) 86400))))
                         ((and standard (null daylight) (= 1 (length open)))
                          (make-zone-rule (or (getf standard :name) "") (getf standard :to)))))
             (last-year (if rule
                            (reduce #'max open :key (lambda (o) (day-date (floor (getf o :start) 86400))))
                            *zone-years*))
             (end (* 86400 (day-number (1+ last-year) 1 1)))
             (onsets '()))
        (dolist (o observances)
          (let ((starts (append (list (getf o :start)) (getf o :dates)
                                (loop for r in (getf o :rules)
                                      nconc (recur-expand r (getf o :start) (getf o :start) end)))))
            (loop for (local . more) on (sort starts #'<)
                  when (and (< local end) (not (eql local (first more))))
                    do (push (cons (- local (getf o :from)) o) onsets))))
        (setf onsets (sort onsets #'< :key #'car))
        ;; no change of offset before the years worked out to, and no rule: no zone it can say
        (when (and (null onsets) (null rule))
          (return-from vtimezone-zone nil))
        (ruled-time-zone (let ((v (ical-value-of component "TZID"))) (and v (ical-text v)))
                         (mapcar #'car onsets)
                         (mapcar (lambda (onset) (getf (cdr onset) :to)) onsets)
                         (if onsets (getf (cdr (first onsets)) :from) (zone-rule-standard-offset rule))
                         rule)))))

(defun calendar-vtimezone (calendar tzid)
  "CALENDAR's own VTIMEZONE whose TZID is TZID, or NIL."
  (find tzid (ical-components-of calendar "VTIMEZONE")
        :key (lambda (c) (ical-text (or (ical-value-of c "TZID") ""))) :test #'string=))

(defun calendar-zones (calendar)
  "A function of a TZID's name giving the time zone it names in CALENDAR, a
VCALENDAR: its own VTIMEZONE of that name first, then the system's zone of
that name, or of the name's end (/mozilla.org/20050126_1/Europe/Stockholm is
Europe/Stockholm); NIL for a name of no zone. Each zone is read once."
  (let ((own (make-hash-table :test 'equal)) (read (make-hash-table :test 'equal)))
    (dolist (c (ical-components-of calendar "VTIMEZONE"))
      (let ((name (ical-value-of c "TZID")))
        (when name (setf (gethash (ical-text name) own) c))))
    (lambda (name)
      (multiple-value-bind (zone known) (gethash name read)
        (cond
          (known zone)
          ;; no zone's name is long: a name of a calendar file's is not looked through when it is
          ((> (length name) *longest-zone-name*) nil)
          (t
            (setf (gethash name read)
                  (or (let ((c (gethash name own))) (and c (vtimezone-zone c)))
                      (loop for parts on (uiop:split-string name :separator "/")
                            for zone = (zone-named (format nil "~{~a~^/~}" parts))
                            thereis zone)))))))))

;;; Written

(defun rule-day-recur (day)
  "The rule of recurrence of a VTIMEZONE observance for DAY, a ZONE-RULE's day: an ICAL-RECUR, or NIL for one it cannot say."
  (when (eq (first day) :month)
    (destructuring-bind (month week wd) (rest day)
      (make-ical-recur :yearly :by-month (list month) :by-day (list (cons (if (= week 5) -1 week) wd))))))

(defun zone-vtimezone (zone &key (first-year 1970) (last-year *zone-years*))
  "A VTIMEZONE component for ZONE, a time zone with a name, from its yearly
rule: an observance for standard time, and one for summer time when it has
it, by rule, or by dates FIRST-YEAR to LAST-YEAR where no rule can say it."
  (let ((rule (time-zone-rule zone))
        (name (or (time-zone-name zone) "UTC")))
    (flet ((observance (kind day time from to label)
             (let* ((local (+ (* 86400 (rule-day day first-year)) time))
                    (recur (and (<= 0 time 86399) (rule-day-recur day))))
               (make-ical-component kind
                                    (append (list (ical-time-property "DTSTART" (make-ical-time (floor local 86400) (mod local 86400)))
                                                  (make-ical-property "TZOFFSETFROM" (utc-offset-text from))
                                                  (make-ical-property "TZOFFSETTO" (utc-offset-text to)))
                                            (when (plusp (length label)) (list (make-ical-property "TZNAME" (ical-text-escaped label))))
                                            (if recur
                                                (list (make-ical-property "RRULE" (ical-recur-text recur)))
                                                (list (make-ical-property "RDATE"
                                                                          (format nil "~{~a~^,~}"
                                                                                  (loop for year from (1+ first-year) to last-year
                                                                                        for at = (+ (* 86400 (rule-day day year)) time)
                                                                                        collect (ical-time-text (make-ical-time (floor at 86400) (mod at 86400)))))))))))))
      (make-ical-component
       "VTIMEZONE"
       (list (make-ical-property "TZID" (ical-text-escaped name)))
       (cond ((and rule (zone-rule-to-daylight rule))
              (let ((standard (zone-rule-standard-offset rule)) (daylight (zone-rule-daylight-offset rule)))
                (destructuring-bind (to-daylight daylight-time) (zone-rule-to-daylight rule)
                  (destructuring-bind (to-standard standard-time) (zone-rule-to-standard rule)
                    (list (observance "DAYLIGHT" to-daylight daylight-time standard daylight (zone-rule-daylight-name rule))
                          (observance "STANDARD" to-standard standard-time daylight standard (zone-rule-standard-name rule)))))))
             (t (let ((offset (if rule (zone-rule-standard-offset rule) (zone-offset zone (get-universal-time)))))
                  (list (make-ical-component "STANDARD"
                                             (append (list (make-ical-property "DTSTART" "19700101T000000")
                                                           (make-ical-property "TZOFFSETFROM" (utc-offset-text offset))
                                                           (make-ical-property "TZOFFSETTO" (utc-offset-text offset)))
                                                     (when (and rule (plusp (length (zone-rule-standard-name rule))))
                                                       (list (make-ical-property "TZNAME" (ical-text-escaped (zone-rule-standard-name rule)))))))))))))))
