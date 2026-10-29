;;;; recur.lisp --- Rules of recurrence expanded into the times they give, within the times asked for.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; RFC 5545, section 3.3.10. A rule gives a set of times in each period of
;;;; its frequency (a year, a month, a week, a day, an hour, a minute, a
;;;; second), every INTERVALth period from the first one's: the days of the
;;;; period that every BY- part of the day allows, the times of day the
;;;; BY- parts of the time give, then BYSETPOS's picks of that set. A part of
;;;; the period's own size or larger only limits; a smaller one gives
;;;; several; one not given is the first time's. The first time, DTSTART, is
;;;; always the first occurrence. Times here are local, counted as universal
;;;; times are, on the clock of the first time's zone.

(in-package #:caldav)

(defparameter *icalendar-most-occurrences* 10000
  "The most occurrences of one rule worked out at once: past them the rest are left.")

(defparameter *icalendar-most-periods* 1000000
  "The most periods of one rule looked through at once, occurrences or not:
a rule that gives none for that long gives no more.")

(defparameter *recur-levels* '(:secondly 0 :minutely 1 :hourly 2 :daily 3 :weekly 4 :monthly 5 :yearly 6)
  "A rule's frequencies by the size of their periods, smallest 0.")

(defun week-one-start (year week-start)
  "The first day of YEAR's week 1, weeks beginning on WEEK-START (a weekday):
the first week with four of its days or more in YEAR."
  (let* ((first (day-number year 1 1))
         (start (- first (mod (- (weekday first) week-start) 7))))
    (if (<= (- first start) 3) start (+ start 7))))

(defun matches-number (n count list)
  "True when N, 1 to COUNT, is in LIST as itself or as N - COUNT - 1, counted from the end."
  (or (member n list) (member (- n count 1) list)))

(defun day-allowed (recur day year days &key month-days week-days)
  "True when the BY- parts of the day of RECUR allow DAY in the period of
year YEAR: BYMONTH, BYWEEKNO, BYYEARDAY, BYMONTHDAY and BYDAY. DAYS are
RECUR's BY-DAY as used, MONTH-DAYS its BY-MONTH-DAY as used, WEEK-DAYS the
weekdays any ordinal allows."
  (multiple-value-bind (y m d) (day-date day)
    (let ((freq (ical-recur-freq recur)))
      (and (or (null (ical-recur-by-month recur)) (member m (ical-recur-by-month recur)))
           (or (null (ical-recur-by-week-no recur))
               (let ((first (week-one-start year (ical-recur-week-start recur))))
                 (matches-number (1+ (floor (- day first) 7))
                                 (floor (- (week-one-start (1+ year) (ical-recur-week-start recur)) first) 7)
                                 (ical-recur-by-week-no recur))))
           (or (null (ical-recur-by-year-day recur))
               (matches-number (1+ (- day (day-number y 1 1))) (if (leap-year-p y) 366 365) (ical-recur-by-year-day recur)))
           (or (null month-days) (matches-number d (month-length y m) month-days))
           (or (null days)
               (let ((wd (weekday day))
                     ;; an ordinal counts within the month or the year the rule's period is
                     (scope (cond ((or (ical-recur-by-week-no recur) (not (member freq '(:monthly :yearly)))) nil)
                                  ((or (eq freq :monthly) (ical-recur-by-month recur)) :month)
                                  (t :year))))
                 (or (member wd week-days)
                     (loop for (ordinal . wanted) in days
                           thereis (and ordinal (= wd wanted)
                                        (case scope
                                          (:month (= ordinal (if (plusp ordinal)
                                                                 (1+ (floor (1- d) 7))
                                                                 (- (1+ (floor (- (month-length y m) d) 7))))))
                                          (:year (let ((index (- day (day-number y 1 1)))
                                                       (length (if (leap-year-p y) 366 365)))
                                                   (= ordinal (if (plusp ordinal)
                                                                  (1+ (floor index 7))
                                                                  (- (1+ (floor (- length index 1) 7)))))))
                                          (t t)))))))))))

(defun recur-period-days (freq period-start interval k recur year0 month0)
  "The first day of RECUR's Kth period and how many days it has, as two
values, the periods beginning at PERIOD-START (a day) every INTERVAL; for
:YEARLY and :MONTHLY counted from YEAR0 and MONTH0. A third value: the
period's year."
  (ecase freq
    (:yearly (let ((year (+ year0 (* k interval))))
               (if (ical-recur-by-week-no recur)
                   (let ((first (week-one-start year (ical-recur-week-start recur))))
                     (values first (- (week-one-start (1+ year) (ical-recur-week-start recur)) first) year))
                   (values (day-number year 1 1) (if (leap-year-p year) 366 365) year))))
    (:monthly (multiple-value-bind (year month) (add-months year0 month0 (* k interval))
                (values (day-number year month 1) (month-length year month) year)))
    (:weekly (let ((first (+ period-start (* 7 k interval))))
               (values first 7 (day-date first))))
    (:daily (let ((first (+ period-start (* k interval))))
              (values first 1 (day-date first))))))

(defun recur-expand (recur start from to &key (universal #'identity) date)
  "The occurrences of RECUR from START, the first, a local time, whose
universal times (UNIVERSAL of each local time) are at FROM or after and
before TO: a list of local times in order. DATE true when START is a date
alone, so every occurrence's time of day is 0. COUNT and UNTIL count from
START; at most *ICALENDAR-MOST-OCCURRENCES* are given and
*ICALENDAR-MOST-PERIODS* periods looked through."
  (let* ((freq (ical-recur-freq recur))
         (level (getf *recur-levels* freq))
         (interval (ical-recur-interval recur))
         (count (ical-recur-count recur))
         (until (ical-recur-until recur))
         (until-past (cond ((null until) (constantly nil))
                           ((ical-time-date-p until)
                            (let ((day (ical-time-day until))) (lambda (local) (> (floor local 86400) day))))
                           ((eq (ical-time-zone until) :utc)
                            (let ((u (ical-local until)))
                              (lambda (local) (> (funcall universal local) u))))
                           (t (let ((u (ical-local until)))
                                (lambda (local) (> local u))))))
         (start-day (floor start 86400))
         (start-second (mod start 86400)))
    (multiple-value-bind (year0 month0 day0) (day-date start-day)
      (multiple-value-bind (hour0 rest) (floor start-second 3600)
        (multiple-value-bind (minute0 second0) (floor rest 60)
          (let* ((by-day (ical-recur-by-day recur))
                 (by-month-day (ical-recur-by-month-day recur))
                 (day-parts (or (ical-recur-by-week-no recur) (ical-recur-by-year-day recur) by-month-day by-day))
                 ;; the first time's own day where the rule says no day
                 (by-month-day (cond ((and (eq freq :yearly) (not day-parts)) (list day0))
                                     ((and (eq freq :monthly) (not by-month-day) (not by-day) (not (ical-recur-by-year-day recur))) (list day0))
                                     (t by-month-day)))
                 (by-day (cond ((and (eq freq :weekly) (not by-day)) (list (cons nil (weekday start-day))))
                               ((and (eq freq :yearly) (ical-recur-by-week-no recur) (not by-day) (not by-month-day)
                                     (not (ical-recur-by-year-day recur)))
                                (list (cons nil (weekday start-day))))
                               (t by-day)))
                 (by-month (if (and (eq freq :yearly) (not day-parts) (not (ical-recur-by-month recur)))
                               (list month0)
                               (ical-recur-by-month recur)))
                 (recur (if (eq by-month (ical-recur-by-month recur)) recur
                            (let ((copy (copy-ical-recur recur))) (setf (ical-recur-by-month copy) by-month) copy)))
                 (week-days (loop for (ordinal . wd) in by-day
                                  when (or (null ordinal) (ical-recur-by-week-no recur) (not (member freq '(:monthly :yearly))))
                                    collect wd))
                 (hours (sort (copy-list (ical-recur-by-hour recur)) #'<))
                 (minutes (sort (copy-list (ical-recur-by-minute recur)) #'<))
                 (seconds (sort (copy-list (ical-recur-by-second recur)) #'<))
                 (week-start-day (- start-day (mod (- (weekday start-day) (ical-recur-week-start recur)) 7)))
                 (unit (case freq (:hourly 3600) (:minutely 60) (:secondly 1)))
                 (base (if unit (* unit (floor start unit)) 0))
                 (found '()) (counted 1) (taken 0)
                 (low (- from 172800)) (high (+ to 172800)))
            (labels ((take (local)
                       ;; T when the work is done
                       (let ((u (funcall universal local)))
                         (cond ((>= u to) t)
                               (t (when (>= u from) (push local found) (incf taken))
                                  (>= taken *icalendar-most-occurrences*)))))
                     (times-of (day fixed-hour fixed-minute fixed-second)
                       ;; the times of DAY the rule gives, in order
                       (if date
                           (list (* 86400 day))
                           (flet ((part (by fixed own)
                                    (cond ((null fixed) (or by (list own)))
                                          ((or (null by) (member fixed by)) (list fixed)))))
                             (loop for h in (part hours fixed-hour hour0)
                                   nconc (loop for m in (part minutes fixed-minute minute0)
                                               nconc (loop for s in (part seconds fixed-second second0)
                                                           collect (+ (* 86400 day) (* 3600 h) (* 60 m) s)))))))
                     (picked (set)
                       (if (null (ical-recur-by-set-pos recur))
                           set
                           (let* ((vector (coerce set 'vector)) (n (length vector)))
                             (sort (remove-duplicates
                                    (loop for pos in (ical-recur-by-set-pos recur)
                                          for index = (if (plusp pos) (1- pos) (+ n pos))
                                          when (< -1 index n) collect (aref vector index)))
                                   #'<))))
                     (period-set (k)
                       ;; the period's times and its first local time, two values
                       (if unit
                           (let* ((at (+ base (* k interval unit)))
                                  (day (floor at 86400)))
                             (multiple-value-bind (h rest) (floor (mod at 86400) 3600)
                               (multiple-value-bind (m s) (floor rest 60)
                                 (values (and (day-allowed recur day (day-date day) by-day :month-days by-month-day :week-days week-days)
                                              (times-of day h (and (<= level 1) m) (and (= level 0) s)))
                                         at))))
                           (multiple-value-bind (first days year) (recur-period-days freq (if (eq freq :weekly) week-start-day start-day) interval k recur year0 month0)
                             (values (loop for day from first below (+ first days)
                                           when (day-allowed recur day year by-day :month-days by-month-day :week-days week-days)
                                             nconc (times-of day nil nil nil))
                                     (* 86400 first)))))
                     (first-period ()
                       ;; the first period worth looking through: none before FROM when nothing is counted
                       (if (or count (< low start))
                           0
                           (max 0 (1- (case freq
                                        (:yearly (floor (- (day-date (floor low 86400)) year0) interval))
                                        (:monthly (multiple-value-bind (y m) (day-date (floor low 86400))
                                                    (floor (+ (* 12 (- y year0)) (- m month0)) interval)))
                                        (:weekly (floor (- (floor low 86400) week-start-day) (* 7 interval)))
                                        (:daily (floor (- (floor low 86400) start-day) interval))
                                        (t (floor (- low base) (* unit interval)))))))))
              (block expanding
                (when (funcall until-past start) (return-from expanding))
                (when (take start) (return-from expanding))
                (when (and count (>= counted count)) (return-from expanding))
                (loop for k from (first-period)
                      repeat *icalendar-most-periods*
                      do (multiple-value-bind (set period-start) (period-set k)
                           (when (> period-start high) (return-from expanding))
                           (dolist (local (picked set))
                             (when (> local start)
                               (when (funcall until-past local) (return-from expanding))
                               (incf counted)
                               (when (and count (> counted count)) (return-from expanding))
                               (when (take local) (return-from expanding))))))))
            (nreverse found)))))))
