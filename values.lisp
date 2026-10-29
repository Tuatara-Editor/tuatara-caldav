;;;; values.lisp --- A calendar file's values: text, dates and times, durations, offsets and rules of recurrence.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; RFC 5545, section 3.3. A value that is not what its kind says is read as
;;;; NIL, never as an error: one bad property leaves the rest of the file.

(in-package #:caldav)

;;; Text

(defun ical-text (value)
  "VALUE, a text property's value as written, with \\n read as a line break
and \\\\ \\; \\, as the character after the backslash."
  (if (not (find #\\ value))
      value
      (with-output-to-string (out)
        (loop with i = 0
              while (< i (length value))
              do (let ((c (char value i)))
                   (if (and (char= c #\\) (< (1+ i) (length value)))
                       (let ((next (char value (1+ i))))
                         (write-char (if (char-equal next #\n) #\Newline next) out)
                         (incf i 2))
                       (progn (write-char c out) (incf i))))))))

(defun ical-texts (value)
  "VALUE, a property's value of several texts (CATEGORIES) as written, as a
list of texts: apart at each comma no backslash escapes, each read as
ICAL-TEXT reads one."
  (let ((parts '()) (start 0))
    (loop with i = 0
          while (< i (length value))
          do (case (char value i)
               (#\\ (incf i 2))
               (#\, (push (subseq value start i) parts) (setf start (1+ i)) (incf i))
               (t (incf i))))
    (push (subseq value start) parts)
    (mapcar #'ical-text (nreverse parts))))

(defun ical-text-escaped (text)
  "TEXT as a text property's value is written: \\ ; and , after a backslash, line breaks as \\n."
  (with-output-to-string (out)
    (loop for c across text
          do (case c
               ((#\\ #\; #\,) (write-char #\\ out) (write-char c out))
               (#\Newline (write-string "\\n" out))
               (#\Return)
               (t (write-char c out))))))

(defun ical-texts-escaped (texts)
  "TEXTS, a list of texts, as one property's value of several, apart by commas."
  (format nil "~{~a~^,~}" (mapcar #'ical-text-escaped texts)))

;;; Dates and times

(defstruct (ical-time (:constructor make-ical-time (day &optional second zone)))
  "A date or a time of a calendar file: its DAY (a day number, as DAY-NUMBER
counts), SECOND, seconds into that day, or NIL for a date alone, and its
ZONE: :UTC, a time zone's name as the file's TZID gives it, or NIL for a
floating time, counted on whatever clock reads it."
  (day 0 :type integer)
  (second nil)
  (zone nil))

(defun universal-ical-time (universal)
  "The universal time UNIVERSAL as an ICAL-TIME in UTC."
  (make-ical-time (floor universal 86400) (mod universal 86400) :utc))

(defun ical-local (time)
  "TIME, an ICAL-TIME, as a local time, counted as universal times are."
  (+ (* 86400 (ical-time-day time)) (or (ical-time-second time) 0)))

(defun ical-time-date-p (time)
  "True when TIME is a date alone."
  (null (ical-time-second time)))

(defun digits-at (text at count)
  "The number COUNT ASCII digits at AT of TEXT write, or NIL."
  (and (<= (+ at count) (length text))
       (loop for i from at below (+ at count) always (char<= #\0 (char text i) #\9))
       (parse-integer text :start at :end (+ at count))))

(defun read-ical-time (value &optional zone)
  "VALUE, a date (20261102) or a date and time (20261102T090000, with Z for
UTC) as written, as an ICAL-TIME; ZONE, a TZID's name, for a time without Z.
NIL when VALUE is neither."
  (let* ((value (string-trim " " value))
         (year (digits-at value 0 4)) (month (digits-at value 4 2)) (day (digits-at value 6 2)))
    (when (and year month day (valid-date-p year month day))
      (case (length value)
        (8 (make-ical-time (day-number year month day)))
        ((15 16)
         (let ((hour (digits-at value 9 2)) (minute (digits-at value 11 2)) (second (digits-at value 13 2))
               (utc (= (length value) 16)))
           (when (and (char-equal (char value 8) #\T) hour minute second
                      (<= hour 23) (<= minute 59) (<= second 60)
                      (or (not utc) (char-equal (char value 15) #\Z)))
             (make-ical-time (day-number year month day) (+ (* 3600 hour) (* 60 minute) (min second 59))
                             (if utc :utc zone)))))))))

(defun ical-time-text (time)
  "TIME, an ICAL-TIME, as its value is written: 20261102, 20261102T090000, or with Z when in UTC."
  (multiple-value-bind (year month day) (day-date (ical-time-day time))
    (if (ical-time-date-p time)
        (format nil "~4,'0d~2,'0d~2,'0d" year month day)
        (multiple-value-bind (hour rest) (floor (ical-time-second time) 3600)
          (multiple-value-bind (minute second) (floor rest 60)
            (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0d~:[~;Z~]" year month day hour minute second
                    (eq (ical-time-zone time) :utc)))))))

(defun ical-property-times (property)
  "The times PROPERTY's value gives (DTSTART, or the list of an EXDATE or
RDATE), as ICAL-TIME structures, its TZID their zone; a period's start for
a period. Values that are no time are left out."
  (let ((zone (ical-parameter-of property "TZID")))
    (loop for part in (uiop:split-string (ical-property-value property) :separator ",")
          for time = (read-ical-time (subseq part 0 (position #\/ part)) zone)
          when time collect time)))

(defun ical-property-time (property)
  "The time PROPERTY's value gives (DTSTART), an ICAL-TIME; NIL when it gives none."
  (and property (first (ical-property-times property))))

(defun ical-time-property (name time)
  "A property NAME (DTSTART) of TIME, an ICAL-TIME: VALUE=DATE for a date, its TZID for a time in a named zone."
  (make-ical-property name (ical-time-text time)
                      (cond ((ical-time-date-p time) (list (list "VALUE" "DATE")))
                            ((stringp (ical-time-zone time)) (list (list "TZID" (ical-time-zone time)))))))

;;; Durations and offsets

(defun read-ical-duration (value)
  "VALUE, a duration as written (P1D, PT1H30M, -P1W), as two values: the days
it counts (weeks seven each), and the seconds. NIL when VALUE is no duration."
  (let* ((value (string-trim " " value)) (end (length value)) (at 0) (sign 1) (days 0) (seconds 0) (any nil) (timep nil))
    (when (and (< at end) (find (char value at) "+-"))
      (when (char= (char value at) #\-) (setf sign -1))
      (incf at))
    (unless (and (< at end) (char-equal (char value at) #\P)) (return-from read-ical-duration nil))
    (incf at)
    (loop while (< at end)
          do (if (char-equal (char value at) #\T)
                 (progn (when timep (return-from read-ical-duration nil)) (setf timep t) (incf at))
                 (let ((stop (or (position-if-not #'digit-char-p value :start at) end)))
                   (when (or (= stop at) (>= stop end) (> (- stop at) 12)) (return-from read-ical-duration nil))
                   (let ((n (parse-integer value :start at :end stop)))
                     (case (char-upcase (char value stop))
                       (#\W (when timep (return-from read-ical-duration nil)) (incf days (* 7 n)))
                       (#\D (when timep (return-from read-ical-duration nil)) (incf days n))
                       (#\H (unless timep (return-from read-ical-duration nil)) (incf seconds (* 3600 n)))
                       (#\M (unless timep (return-from read-ical-duration nil)) (incf seconds (* 60 n)))
                       (#\S (unless timep (return-from read-ical-duration nil)) (incf seconds n))
                       (t (return-from read-ical-duration nil)))
                     (setf any t at (1+ stop))))))
    (and any (values (* sign days) (* sign seconds)))))

(defun ical-duration-text (days seconds)
  "DAYS and SECONDS, both of one sign, as a duration is written: P1D, PT1H30M, -P1DT2H."
  (let ((sign (if (or (minusp days) (minusp seconds)) "-" "")) (days (abs days)) (seconds (abs seconds)))
    (multiple-value-bind (hours rest) (floor seconds 3600)
      (multiple-value-bind (minutes secs) (floor rest 60)
        (format nil "~aP~:[~;~:*~dD~]~:[~;T~]~:[~;~:*~dH~]~:[~;~:*~dM~]~:[~;~:*~dS~]"
                sign (and (or (plusp days) (zerop seconds)) days) (plusp seconds)
                (and (plusp hours) hours) (and (plusp minutes) minutes) (and (plusp secs) secs))))))

(defun read-utc-offset (value)
  "VALUE, an offset from UTC as written (+0100, -053000), as seconds east of UTC; NIL when it is none."
  (let* ((value (string-trim " " value))
         (hours (digits-at value 1 2)) (minutes (digits-at value 3 2))
         (seconds (if (= (length value) 7) (digits-at value 5 2) 0)))
    (when (and (member (length value) '(5 7)) (find (char value 0) "+-") hours minutes seconds (<= minutes 59) (<= seconds 59))
      (* (if (char= (char value 0) #\-) -1 1) (+ (* 3600 hours) (* 60 minutes) seconds)))))

(defun utc-offset-text (offset)
  "OFFSET, seconds east of UTC, as written: +0100, -0530, +053010 with seconds."
  (multiple-value-bind (hours rest) (floor (abs offset) 3600)
    (multiple-value-bind (minutes seconds) (floor rest 60)
      (format nil "~:[+~;-~]~2,'0d~2,'0d~:[~;~:*~2,'0d~]" (minusp offset) hours minutes (and (plusp seconds) seconds)))))

;;; Rules of recurrence

(defstruct (ical-recur (:constructor make-ical-recur (freq &key (interval 1) count until by-second by-minute by-hour by-day
                                                              by-month-day by-year-day by-week-no by-month by-set-pos (week-start 1))))
  "A rule of recurrence (RRULE): FREQ, one of :SECONDLY :MINUTELY :HOURLY
:DAILY :WEEKLY :MONTHLY :YEARLY; every INTERVALth; COUNT times in all or
UNTIL an ICAL-TIME, or neither; the BY- parts, lists of numbers, BY-DAY a
list of (ORDINAL . WEEKDAY), ORDINAL NIL for every such weekday and WEEKDAY
0 for Sunday; and WEEK-START, the weekday weeks begin on, Monday (1) unless
it says."
  (freq :daily :type keyword)
  (interval 1 :type (integer 1))
  (count nil)
  (until nil)
  (by-second nil) (by-minute nil) (by-hour nil) (by-day nil) (by-month-day nil)
  (by-year-day nil) (by-week-no nil) (by-month nil) (by-set-pos nil)
  (week-start 1))

(defparameter *recur-weekdays* #("SU" "MO" "TU" "WE" "TH" "FR" "SA")
  "The weekdays as a rule of recurrence writes them, Sunday first, by WEEKDAY's numbers.")

(defparameter *recur-frequencies*
  '(("SECONDLY" . :secondly) ("MINUTELY" . :minutely) ("HOURLY" . :hourly) ("DAILY" . :daily)
    ("WEEKLY" . :weekly) ("MONTHLY" . :monthly) ("YEARLY" . :yearly))
  "A rule of recurrence's frequencies as written, and as read.")

(defun once-each (numbers)
  "NUMBERS sorted, each once."
  (loop for (n . more) on (sort numbers #'<) unless (eql n (first more)) collect n))

(defun read-ical-recur (value &optional zone)
  "VALUE, a rule of recurrence as written (FREQ=WEEKLY;BYDAY=MO,WE), as an
ICAL-RECUR, its UNTIL read in ZONE when it gives none; NIL when it is no
rule, or has a part out of its range. Parts it does not know are left out."
  (let ((freq nil) (parts '()))
    (flet ((numbers (text low high &key (negative t))
             ;; the comma-apart numbers of TEXT, each in LOW to HIGH (or its negative), each
             ;; once, in order, or :BAD; no more of them than there are such numbers
             (let ((words (uiop:split-string text :separator ",")))
               (if (> (length words) (* 2 (1+ (- high low))))
                   :bad
                   (or (once-each (loop for word in words
                                        for n = (ignore-errors (parse-integer word))
                                        if (and n (or (<= low n high) (and negative (<= (- high) n (- low))))) collect n
                                          else do (return-from numbers :bad)))
                       :bad))))
           (weekday (text)
             (position text *recur-weekdays* :test #'string-equal)))
      (dolist (part (uiop:split-string (string-trim " " value) :separator ";"))
        (let* ((equals (position #\= part))
               (key (and equals (string-upcase (subseq part 0 equals))))
               (text (and equals (subseq part (1+ equals)))))
          (when key
            (flet ((put (name value)
                     (when (eq value :bad) (return-from read-ical-recur nil))
                     (setf (getf parts name) value)))
              (cond ((string= key "FREQ") (setf freq (cdr (assoc text *recur-frequencies* :test #'string-equal))))
                    ((string= key "INTERVAL") (let ((n (ignore-errors (parse-integer text)))) (put :interval (if (and n (plusp n)) n :bad))))
                    ((string= key "COUNT") (let ((n (ignore-errors (parse-integer text)))) (put :count (if (and n (plusp n)) n :bad))))
                    ((string= key "UNTIL") (put :until (or (read-ical-time text zone) :bad)))
                    ((string= key "BYSECOND") (put :by-second (numbers text 0 60 :negative nil)))
                    ((string= key "BYMINUTE") (put :by-minute (numbers text 0 59 :negative nil)))
                    ((string= key "BYHOUR") (put :by-hour (numbers text 0 23 :negative nil)))
                    ((string= key "BYMONTHDAY") (put :by-month-day (numbers text 1 31)))
                    ((string= key "BYYEARDAY") (put :by-year-day (numbers text 1 366)))
                    ((string= key "BYWEEKNO") (put :by-week-no (numbers text 1 53)))
                    ((string= key "BYMONTH") (put :by-month (numbers text 1 12 :negative nil)))
                    ((string= key "BYSETPOS") (put :by-set-pos (numbers text 1 366)))
                    ((string= key "WKST") (put :week-start (or (weekday text) :bad)))
                    ((string= key "BYDAY")
                     ;; no more weekdays than there are (7, by 106 ordinals or none), each once
                     (let ((words (uiop:split-string text :separator ",")))
                       (put :by-day
                            (if (> (length words) (* 7 107))
                                :bad
                                (let ((days (loop for word in words
                                                  for size = (length word)
                                                  for day = (and (>= size 2) (weekday (subseq word (- size 2))))
                                                  for ordinal = (and (> size 2) (ignore-errors (parse-integer word :end (- size 2))))
                                                  if (and day (or (= size 2) (and ordinal (<= 1 (abs ordinal) 53))))
                                                    collect (cons (and (> size 2) ordinal) day)
                                                  else do (return :bad))))
                                  (cond ((eq days :bad) :bad)
                                        ((null days) :bad)
                                        (t (remove-duplicates days :test #'equal))))))))))))))
    (and freq (apply #'make-ical-recur freq parts))))

(defun ical-recur-text (recur)
  "RECUR, an ICAL-RECUR, as a rule of recurrence is written."
  (format nil "FREQ=~a~@[;INTERVAL=~d~]~@[;COUNT=~d~]~@[;UNTIL=~a~]~@[;BYMONTH=~{~d~^,~}~]~@[;BYWEEKNO=~{~d~^,~}~]~@[;BYYEARDAY=~{~d~^,~}~]~@[;BYMONTHDAY=~{~d~^,~}~]~@[;BYDAY=~{~a~^,~}~]~@[;BYHOUR=~{~d~^,~}~]~@[;BYMINUTE=~{~d~^,~}~]~@[;BYSECOND=~{~d~^,~}~]~@[;BYSETPOS=~{~d~^,~}~]~@[;WKST=~a~]"
          (car (rassoc (ical-recur-freq recur) *recur-frequencies*))
          (and (/= 1 (ical-recur-interval recur)) (ical-recur-interval recur))
          (ical-recur-count recur)
          (and (ical-recur-until recur) (ical-time-text (ical-recur-until recur)))
          (ical-recur-by-month recur) (ical-recur-by-week-no recur) (ical-recur-by-year-day recur) (ical-recur-by-month-day recur)
          (loop for (ordinal . day) in (ical-recur-by-day recur)
                collect (format nil "~@[~d~]~a" ordinal (aref *recur-weekdays* day)))
          (ical-recur-by-hour recur) (ical-recur-by-minute recur) (ical-recur-by-second recur) (ical-recur-by-set-pos recur)
          (and (/= 1 (ical-recur-week-start recur)) (aref *recur-weekdays* (ical-recur-week-start recur)))))

(setf (documentation 'make-ical-time 'function) "An ICAL-TIME of DAY (a day number), SECOND into it, NIL for a date alone, and ZONE: :UTC, a TZID's name, or NIL for a floating time."
      (documentation 'ical-time-p 'function) "True when the object given is an ICAL-TIME."
      (documentation 'ical-time-day 'function) "The day of an ICAL-TIME, a day number as DAY-NUMBER counts."
      (documentation 'ical-time-second 'function) "The seconds into its day of an ICAL-TIME, or NIL for a date alone."
      (documentation 'ical-time-zone 'function) "The zone of an ICAL-TIME: :UTC, a TZID's name, or NIL for a floating time."
      (documentation 'make-ical-recur 'function) "An ICAL-RECUR of FREQ and the parts given by their keywords, each as ICAL-RECUR says."
      (documentation 'ical-recur-p 'function) "True when the object given is an ICAL-RECUR."
      (documentation 'ical-recur-freq 'function) "A rule's frequency: :SECONDLY, :MINUTELY, :HOURLY, :DAILY, :WEEKLY, :MONTHLY or :YEARLY."
      (documentation 'ical-recur-interval 'function) "Which of a rule's periods it gives times in: every one for 1, every other for 2."
      (documentation 'ical-recur-count 'function) "How many times a rule gives in all, the first time counted, or NIL."
      (documentation 'ical-recur-until 'function) "The last time a rule may give, an ICAL-TIME, or NIL."
      (documentation 'ical-recur-by-second 'function) "The seconds of the minute a rule's BYSECOND gives, or NIL."
      (documentation 'ical-recur-by-minute 'function) "The minutes of the hour a rule's BYMINUTE gives, or NIL."
      (documentation 'ical-recur-by-hour 'function) "The hours of the day a rule's BYHOUR gives, or NIL."
      (documentation 'ical-recur-by-day 'function) "The weekdays a rule's BYDAY gives, a list of (ORDINAL . WEEKDAY), or NIL."
      (documentation 'ical-recur-by-month-day 'function) "The days of the month a rule's BYMONTHDAY gives, below 0 from the month's end, or NIL."
      (documentation 'ical-recur-by-year-day 'function) "The days of the year a rule's BYYEARDAY gives, below 0 from the year's end, or NIL."
      (documentation 'ical-recur-by-week-no 'function) "The weeks of the year a rule's BYWEEKNO gives, below 0 from the year's end, or NIL."
      (documentation 'ical-recur-by-month 'function) "The months a rule's BYMONTH gives, or NIL."
      (documentation 'ical-recur-by-set-pos 'function) "Which of each period's times a rule's BYSETPOS keeps, below 0 from the last, or NIL."
      (documentation 'ical-recur-week-start 'function) "The weekday a rule's weeks begin on, 0 for Sunday.")
