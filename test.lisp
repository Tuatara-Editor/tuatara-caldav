;;;; test.lisp --- The caldav package: calendar files read and written, their rules of recurrence, time zones and occurrences.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test caldav
  :description "Calendar files read and written back whole, their values, rules of recurrence against RFC 5545's own examples, VTIMEZONE components against the system's zones, the occurrences of events and tasks, and the bounds an untrusted file meets.")

(defun cs-ical-lines (&rest lines)
  "LINES as a calendar file's text, each ending CR LF."
  (format nil "~{~a~c~c~}" (loop for line in lines nconc (list line #\Return #\Newline))))

(defun cs-ical-local (text)
  "TEXT, a date and time as a calendar file writes it, as a local time."
  (let ((time (caldav::read-ical-time text)))
    (+ (* 86400 (caldav::ical-time-day time)) (or (caldav::ical-time-second time) 0))))

(defun cs-ical-written (local)
  "LOCAL, a local time, as a calendar file writes it."
  (caldav::ical-time-text (caldav::make-ical-time (floor local 86400) (mod local 86400))))

(defun cs-recur-times (rule start &key (to "21000101T000000") (from start) (most 200))
  "The occurrences of RULE, as written, from START, written: those from FROM
before TO, local times all, the first MOST of them."
  (let ((caldav::*icalendar-most-occurrences* most))
    (mapcar #'cs-ical-written
            (caldav::recur-expand (caldav::read-ical-recur rule) (cs-ical-local start) (cs-ical-local from) (cs-ical-local to)))))

(define-test (caldav lines-and-components)
  (let* ((text (cs-ical-lines "BEGIN:VCALENDAR" "VERSION:2.0" "BEGIN:VEVENT" "UID:one@example.org"
                           "SUMMARY:A meeting about the ha"
                           " rbour, the wh" ; folded within a word
                           "	arf and the café"
                           "ATTENDEE;CN=\"Hemi Taupō, Rotorua\";ROLE=REQ-PARTICIPANT:mailto:hemi@example.org"
                           "X-NOTE;X-SAYS=line^nbreak ^'quoted^' ^^:value"
                           "END:VEVENT" "END:VCALENDAR"))
         (calendars (caldav::icalendar-read text))
         (event (first (caldav::ical-components-of (first calendars) "VEVENT")))
         (attendee (caldav::ical-property-of event "attendee")))
    (is = 1 (length calendars))
    (is string= "VCALENDAR" (caldav::ical-component-name (first calendars)))
    (is string= "A meeting about the harbour, the wharf and the café" (caldav::ical-value-of event "SUMMARY"))
    (is string= "Hemi Taupō, Rotorua" (caldav::ical-parameter-of attendee "CN"))
    (is string= "mailto:hemi@example.org" (caldav::ical-property-value attendee))
    (is string= (format nil "line~%break \"quoted\" ^")
        (caldav::ical-parameter-of (caldav::ical-property-of event "X-NOTE") "X-SAYS"))
    ;; written back and read again: the same tree; no line past 75 octets, none split inside a character
    (let* ((written (caldav::icalendar-write calendars))
           (again (caldav::icalendar-read written)))
      (is string= written (caldav::icalendar-write again))
      (is equal '() (loop for line in (uiop:split-string written :separator (string #\Newline))
                          when (> (length (sb-ext:string-to-octets line :external-format :utf-8)) 76) collect line))
      (true (search "CN=\"Hemi Taupō, Rotorua\"" written))
      (true (search "X-SAYS=line^nbreak ^'quoted^' ^^:" written)))
    (let* ((long (make-string 200 :initial-element #\ō))
           (written (caldav::icalendar-write
                     (caldav::make-ical-component "VEVENT" (list (caldav::make-ical-property "SUMMARY" long))))))
      (true (every (lambda (line) (<= (length (sb-ext:string-to-octets (string-right-trim '(#\Return) line) :external-format :utf-8)) 75))
                   (uiop:split-string written :separator (string #\Newline))))
      (is string= long (caldav::ical-value-of (first (caldav::icalendar-read written)) "SUMMARY")))
    ;; LF alone ends lines too, and a byte order mark is passed over
    (is string= "x" (caldav::ical-value-of
                     (first (caldav::icalendar-read (format nil "~cBEGIN:VTODO~%SUMMARY:x~%END:VTODO~%" (code-char #xFEFF))))
                     "SUMMARY"))))

(define-test (caldav not-calendar-files)
  (flet ((refused (text)
           (handler-case (progn (caldav::icalendar-read text) nil)
             (caldav::icalendar-error (e) (or (caldav::icalendar-error-line e) t)))))
    (is = 3 (refused (cs-ical-lines "BEGIN:VCALENDAR" "BEGIN:VEVENT" "END:VCALENDAR")))
    (is = 1 (refused (cs-ical-lines "SUMMARY:outside")))
    (is = 2 (refused (cs-ical-lines "BEGIN:VEVENT" "no colon here")))
    (is = 2 (refused (cs-ical-lines "BEGIN:VEVENT" "ATTENDEE;CN=\"never closed:x")))
    (true (refused (cs-ical-lines "BEGIN:VCALENDAR")))
    (let ((caldav::*icalendar-deepest* 3))
      (is = 4 (refused (cs-ical-lines "BEGIN:A" "BEGIN:B" "BEGIN:C" "BEGIN:D"))))
    (let ((caldav::*icalendar-largest* 100))
      (true (refused (format nil "~{~a~}" (loop repeat 10 collect (cs-ical-lines "SUMMARY:twenty characters"))))))))

(define-test (caldav values)
  (is string= (format nil "a, b; c\\ d~%e") (caldav::ical-text "a\\, b\\; c\\\\ d\\ne"))
  (is string= "a\\, b\\; c\\\\ d\\ne" (caldav::ical-text-escaped (format nil "a, b; c\\ d~%e")))
  (is equal '("work" "a, b" "home") (caldav::ical-texts "work,a\\, b,home"))
  (is string= "work,a\\, b" (caldav::ical-texts-escaped '("work" "a, b")))
  ;; dates and times
  (let ((time (caldav::read-ical-time "20261102T090000" "Europe/Stockholm")))
    (is = (tuatara.dates:day-number 2026 11 2) (caldav::ical-time-day time))
    (is = 32400 (caldav::ical-time-second time))
    (is string= "Europe/Stockholm" (caldav::ical-time-zone time)))
  (is eq :utc (caldav::ical-time-zone (caldav::read-ical-time "20261102T090000Z" "Europe/Stockholm")))
  (true (caldav::ical-time-date-p (caldav::read-ical-time "20261102")))
  (dolist (bad '("20260230" "20261102T250000" "2026110" "20261102 090000" "20261102T090000X" "x"))
    (false (caldav::read-ical-time bad) "~a is no time" bad))
  (dolist (text '("20261102" "20261102T090000" "20261102T235959Z"))
    (is string= text (caldav::ical-time-text (caldav::read-ical-time text))))
  (let ((property (caldav::make-ical-property "EXDATE" "20261102T090000,20261109T090000" '(("TZID" "Europe/Stockholm")))))
    (is equal '("20261102T090000" "20261109T090000")
        (mapcar #'caldav::ical-time-text (caldav::ical-property-times property))))
  ;; durations and offsets
  (is equal '(1 0) (multiple-value-list (caldav::read-ical-duration "P1D")))
  (is equal '(0 5400) (multiple-value-list (caldav::read-ical-duration "PT1H30M")))
  (is equal '(-14 -7200) (multiple-value-list (caldav::read-ical-duration "-P2WT2H")))
  (is equal '(0 15) (multiple-value-list (caldav::read-ical-duration "PT15S")))
  (dolist (bad '("P" "1D" "PT1D" "P1H" "PTT1H" "P1.5D"))
    (false (caldav::read-ical-duration bad) "~a is no duration" bad))
  (is string= "P1D" (caldav::ical-duration-text 1 0))
  (is string= "PT1H30M" (caldav::ical-duration-text 0 5400))
  (is string= "-P1DT2H" (caldav::ical-duration-text -1 -7200))
  (is string= "P0D" (caldav::ical-duration-text 0 0))
  (is = 3600 (caldav::read-utc-offset "+0100"))
  (is = -19800 (caldav::read-utc-offset "-0530"))
  (is = 3610 (caldav::read-utc-offset "+010010"))
  (false (caldav::read-utc-offset "0100"))
  (is string= "+1300" (caldav::utc-offset-text 46800))
  (is string= "-0530" (caldav::utc-offset-text -19800))
  ;; rules read and written
  (dolist (text '("FREQ=WEEKLY;INTERVAL=2;UNTIL=19971224T000000Z;BYDAY=MO,WE,FR;WKST=SU"
                  "FREQ=MONTHLY;COUNT=10;BYDAY=1SU,-1SU"
                  "FREQ=YEARLY;BYMONTH=11;BYMONTHDAY=2,3,4,5,6,7,8;BYDAY=TU"
                  "FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-2"))
    (is string= text (caldav::ical-recur-text (caldav::read-ical-recur text))))
  (let ((recur (caldav::read-ical-recur "freq=yearly;byday=20mo;x-name=kept-out")))
    (is eq :yearly (caldav::ical-recur-freq recur))
    (is equal '((20 . 1)) (caldav::ical-recur-by-day recur)))
  (dolist (bad '("INTERVAL=2" "FREQ=FORTNIGHTLY" "FREQ=DAILY;INTERVAL=0" "FREQ=DAILY;BYHOUR=24"
                 "FREQ=MONTHLY;BYMONTHDAY=32" "FREQ=MONTHLY;BYDAY=1XX" "FREQ=DAILY;UNTIL=soon" "FREQ=YEARLY;BYMONTH=0"))
    (false (caldav::read-ical-recur bad) "~a is no rule" bad)))

(define-test (caldav rules-of-recurrence)
  ;; RFC 5545, section 3.8.5.3, its examples and the times it lists, in local time
  (flet ((check (rule start expected &key (to "21000101T000000"))
           (is equal expected (subseq (cs-recur-times rule start :to to) 0 (min (length expected) (length (cs-recur-times rule start :to to))))
               "~a from ~a" rule start)))
    (check "FREQ=DAILY;COUNT=10" "19970902T090000"
           '("19970902T090000" "19970903T090000" "19970904T090000" "19970905T090000" "19970906T090000"
             "19970907T090000" "19970908T090000" "19970909T090000" "19970910T090000" "19970911T090000"))
    (is = 10 (length (cs-recur-times "FREQ=DAILY;COUNT=10" "19970902T090000")))
    (is = 113 (length (cs-recur-times "FREQ=DAILY;UNTIL=19971224T000000" "19970902T090000")))
    (check "FREQ=DAILY;INTERVAL=10;COUNT=5" "19970902T090000"
           '("19970902T090000" "19970912T090000" "19970922T090000" "19971002T090000" "19971012T090000"))
    (is = 93 (length (cs-recur-times "FREQ=YEARLY;UNTIL=20000131T140000;BYMONTH=1;BYDAY=SU,MO,TU,WE,TH,FR,SA" "19980101T090000")))
    (is = 93 (length (cs-recur-times "FREQ=DAILY;UNTIL=20000131T140000;BYMONTH=1" "19980101T090000")))
    (check "FREQ=WEEKLY;UNTIL=19971007T000000;WKST=SU;BYDAY=TU,TH" "19970902T090000"
           '("19970902T090000" "19970904T090000" "19970909T090000" "19970911T090000" "19970916T090000"
             "19970918T090000" "19970923T090000" "19970925T090000" "19970930T090000" "19971002T090000"))
    (is = 10 (length (cs-recur-times "FREQ=WEEKLY;UNTIL=19971007T000000;WKST=SU;BYDAY=TU,TH" "19970902T090000")))
    (check "FREQ=WEEKLY;INTERVAL=2;UNTIL=19971224T000000;WKST=SU;BYDAY=MO,WE,FR" "19970901T090000"
           '("19970901T090000" "19970903T090000" "19970905T090000" "19970915T090000" "19970917T090000"
             "19970919T090000" "19970929T090000" "19971001T090000" "19971003T090000" "19971013T090000"))
    (is = 25 (length (cs-recur-times "FREQ=WEEKLY;INTERVAL=2;UNTIL=19971224T000000;WKST=SU;BYDAY=MO,WE,FR" "19970901T090000")))
    (check "FREQ=MONTHLY;COUNT=10;BYDAY=1FR" "19970905T090000"
           '("19970905T090000" "19971003T090000" "19971107T090000" "19971205T090000" "19980102T090000"
             "19980206T090000" "19980306T090000" "19980403T090000" "19980501T090000" "19980605T090000"))
    (check "FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=1SU,-1SU" "19970907T090000"
           '("19970907T090000" "19970928T090000" "19971102T090000" "19971130T090000" "19980104T090000"
             "19980125T090000" "19980301T090000" "19980329T090000" "19980503T090000" "19980531T090000"))
    (check "FREQ=MONTHLY;COUNT=6;BYDAY=-2MO" "19970922T090000"
           '("19970922T090000" "19971020T090000" "19971117T090000" "19971222T090000" "19980119T090000" "19980216T090000"))
    (check "FREQ=MONTHLY;BYMONTHDAY=-3" "19970928T090000"
           '("19970928T090000" "19971029T090000" "19971128T090000" "19971229T090000" "19980129T090000" "19980226T090000"))
    (check "FREQ=MONTHLY;COUNT=10;BYMONTHDAY=1,-1" "19970930T090000"
           '("19970930T090000" "19971001T090000" "19971031T090000" "19971101T090000" "19971130T090000"
             "19971201T090000" "19971231T090000" "19980101T090000" "19980131T090000" "19980201T090000"))
    (check "FREQ=MONTHLY;INTERVAL=18;COUNT=10;BYMONTHDAY=10,11,12,13,14,15" "19970910T090000"
           '("19970910T090000" "19970911T090000" "19970912T090000" "19970913T090000" "19970914T090000"
             "19970915T090000" "19990310T090000" "19990311T090000" "19990312T090000" "19990313T090000"))
    (check "FREQ=MONTHLY;INTERVAL=2;BYDAY=TU" "19970902T090000"
           '("19970902T090000" "19970909T090000" "19970916T090000" "19970923T090000" "19970930T090000"
             "19971104T090000" "19971111T090000" "19971118T090000" "19971125T090000" "19980106T090000"))
    (check "FREQ=YEARLY;COUNT=10;BYMONTH=6,7" "19970610T090000"
           '("19970610T090000" "19970710T090000" "19980610T090000" "19980710T090000" "19990610T090000"
             "19990710T090000" "20000610T090000" "20000710T090000" "20010610T090000" "20010710T090000"))
    (check "FREQ=YEARLY;INTERVAL=2;COUNT=10;BYMONTH=1,2,3" "19970310T090000"
           '("19970310T090000" "19990110T090000" "19990210T090000" "19990310T090000" "20010110T090000"
             "20010210T090000" "20010310T090000" "20030110T090000" "20030210T090000" "20030310T090000"))
    (check "FREQ=YEARLY;INTERVAL=3;COUNT=10;BYYEARDAY=1,100,200" "19970101T090000"
           '("19970101T090000" "19970410T090000" "19970719T090000" "20000101T090000" "20000409T090000"
             "20000718T090000" "20030101T090000" "20030410T090000" "20030719T090000" "20060101T090000"))
    (check "FREQ=YEARLY;BYDAY=20MO" "19970519T090000" '("19970519T090000" "19980518T090000" "19990517T090000"))
    (check "FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO" "19970512T090000" '("19970512T090000" "19980511T090000" "19990517T090000"))
    (check "FREQ=YEARLY;BYMONTH=3;BYDAY=TH" "19970313T090000"
           '("19970313T090000" "19970320T090000" "19970327T090000" "19980305T090000" "19980312T090000"
             "19980319T090000" "19980326T090000" "19990304T090000" "19990311T090000" "19990318T090000" "19990325T090000"))
    ;; Friday the 13th: the first time, not one, is counted all the same
    (check "FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13" "19970902T090000"
           '("19970902T090000" "19980213T090000" "19980313T090000" "19981113T090000" "19990813T090000" "20001013T090000"))
    (check "FREQ=MONTHLY;BYDAY=SA;BYMONTHDAY=7,8,9,10,11,12,13" "19970913T090000"
           '("19970913T090000" "19971011T090000" "19971108T090000" "19971213T090000" "19980110T090000"
             "19980207T090000" "19980307T090000" "19980411T090000" "19980509T090000" "19980613T090000"))
    (check "FREQ=YEARLY;INTERVAL=4;BYMONTH=11;BYDAY=TU;BYMONTHDAY=2,3,4,5,6,7,8" "19961105T090000"
           '("19961105T090000" "20001107T090000" "20041102T090000"))
    (check "FREQ=MONTHLY;COUNT=3;BYDAY=TU,WE,TH;BYSETPOS=3" "19970904T090000"
           '("19970904T090000" "19971007T090000" "19971106T090000"))
    (check "FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-2" "19970929T090000"
           '("19970929T090000" "19971030T090000" "19971127T090000" "19971230T090000" "19980129T090000"
             "19980226T090000" "19980330T090000"))
    (check "FREQ=MINUTELY;INTERVAL=15;COUNT=6" "19970902T090000"
           '("19970902T090000" "19970902T091500" "19970902T093000" "19970902T094500" "19970902T100000" "19970902T101500"))
    (check "FREQ=MINUTELY;INTERVAL=90;COUNT=4" "19970902T090000"
           '("19970902T090000" "19970902T103000" "19970902T120000" "19970902T133000"))
    (let ((daily (cs-recur-times "FREQ=DAILY;BYHOUR=9,10,11,12,13,14,15,16;BYMINUTE=0,20,40" "19970902T090000" :to "19970904T000000"))
          (minutely (cs-recur-times "FREQ=MINUTELY;INTERVAL=20;BYHOUR=9,10,11,12,13,14,15,16" "19970902T090000" :to "19970904T000000")))
      (is = 48 (length daily))
      (is equal daily minutely)
      (is string= "19970902T164000" (nth 23 daily))
      (is string= "19970903T090000" (nth 24 daily)))
    ;; the week's first day changes which weeks are every other one
    (check "FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=MO" "19970805T090000"
           '("19970805T090000" "19970810T090000" "19970819T090000" "19970824T090000"))
    (check "FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=SU" "19970805T090000"
           '("19970805T090000" "19970817T090000" "19970819T090000" "19970831T090000"))
    ;; a day the month has not is no occurrence
    (check "FREQ=MONTHLY;BYMONTHDAY=15,30;COUNT=5" "20070115T090000"
           '("20070115T090000" "20070130T090000" "20070215T090000" "20070315T090000" "20070330T090000")))
  ;; only those within the times asked for, the ones before skipped over quickly
  (is equal '("20261102T090000" "20261109T090000")
      (cs-recur-times "FREQ=WEEKLY" "19970901T090000" :from "20261101T000000" :to "20261110T000000"))
  (is equal '("20261102T090000" "20261102T090010")
      (cs-recur-times "FREQ=SECONDLY;INTERVAL=10" "20200101T090000" :from "20261102T090000" :to "20261102T090020"))
  ;; COUNT counts from the first time even when the times asked for begin later
  (is equal '("19970904T090000") (cs-recur-times "FREQ=DAILY;COUNT=3" "19970902T090000" :from "19970904T000000"))
  ;; bounded: a rule of no end gives no more than it may, one that gives nothing stops
  (is = 7 (length (cs-recur-times "FREQ=SECONDLY" "20260101T000000" :most 7)))
  (is equal '("20260101T000000") (cs-recur-times "FREQ=YEARLY;BYMONTH=2;BYMONTHDAY=30" "20260101T000000")))

(define-test (caldav vtimezone-components)
  (let* ((text (cs-ical-lines "BEGIN:VCALENDAR"
                           "BEGIN:VTIMEZONE" "TZID:Eastern"
                           ;; the way some programs write it: from 1601, by rule
                           "BEGIN:STANDARD" "DTSTART:16011104T020000" "RRULE:FREQ=YEARLY;BYDAY=1SU;BYMONTH=11"
                           "TZOFFSETFROM:-0400" "TZOFFSETTO:-0500" "TZNAME:EST" "END:STANDARD"
                           "BEGIN:DAYLIGHT" "DTSTART:16010311T020000" "RRULE:FREQ=YEARLY;BYDAY=2SU;BYMONTH=3"
                           "TZOFFSETFROM:-0500" "TZOFFSETTO:-0400" "TZNAME:EDT" "END:DAYLIGHT"
                           "END:VTIMEZONE"
                           "BEGIN:VTIMEZONE" "TZID:Old rules"
                           "BEGIN:STANDARD" "DTSTART:19701025T030000" "RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10;UNTIL=19961027T010000Z"
                           "TZOFFSETFROM:+0200" "TZOFFSETTO:+0100" "END:STANDARD"
                           "BEGIN:DAYLIGHT" "DTSTART:19700329T020000" "RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=3"
                           "TZOFFSETFROM:+0100" "TZOFFSETTO:+0200" "END:DAYLIGHT"
                           "BEGIN:STANDARD" "DTSTART:19961027T030000" "RRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=10"
                           "TZOFFSETFROM:+0200" "TZOFFSETTO:+0100" "END:STANDARD"
                           "END:VTIMEZONE"
                           "END:VCALENDAR"))
         (calendar (first (caldav::icalendar-read text)))
         (zones (caldav::calendar-zones calendar))
         (eastern (funcall zones "Eastern"))
         (new-york (tuatara.dates:zone-named "America/New_York"))
         (stockholm (tuatara.dates:zone-named "Europe/Stockholm"))
         (from (* 86400 (tuatara.dates:day-number 2008 1 1)))
         (to (* 86400 (tuatara.dates:day-number 2040 1 1))))
    (true new-york)
    (true (tuatara.dates:time-zone-rule eastern) "a rule the system's zones can say too")
    (is equal '() (loop for time from from below to by 86399
                        unless (= (tuatara.dates:zone-offset eastern time) (tuatara.dates:zone-offset new-york time)) collect time))
    (let ((old (funcall zones "Old rules")))
      (is equal '() (loop for time from (* 86400 (tuatara.dates:day-number 1997 1 1)) below to by 86399
                          unless (= (tuatara.dates:zone-offset old time) (tuatara.dates:zone-offset stockholm time)) collect time)))
    ;; names of no VTIMEZONE: the system's, or the end of a longer name
    (is eq stockholm (funcall zones "Europe/Stockholm"))
    (is eq stockholm (funcall zones "/mozilla.org/20050126_1/Europe/Stockholm"))
    (false (funcall zones "Nowhere/Particular"))
    (false (funcall zones "../../etc/passwd"))
    ;; written for a zone and read back: the same offsets
    (dolist (name '("Europe/Stockholm" "Pacific/Auckland" "America/New_York" "Asia/Kolkata" "Australia/Lord_Howe" "America/Santiago"))
      (let* ((zone (tuatara.dates:zone-named name))
             (component (caldav::zone-vtimezone zone))
             (read (caldav::vtimezone-zone
                    (first (caldav::icalendar-read (caldav::icalendar-write component))))))
        (is string= name (caldav::ical-value-of component "TZID"))
        (is equal '() (loop for time from (* 86400 (tuatara.dates:day-number 2026 1 1)) below to by 3599
                            unless (= (tuatara.dates:zone-offset zone time) (tuatara.dates:zone-offset read time)) collect time)
            "~a written and read" name)))))

(define-test (caldav occurrences)
  (let* ((text (cs-ical-lines "BEGIN:VCALENDAR"
                           "BEGIN:VEVENT" "UID:weekly" "SUMMARY:Stand-up" "DTSTART;TZID=Europe/Stockholm:20261019T090000"
                           "DTEND;TZID=Europe/Stockholm:20261019T091500" "RRULE:FREQ=WEEKLY;BYDAY=MO;COUNT=6"
                           "EXDATE;TZID=Europe/Stockholm:20261102T090000" "RDATE;TZID=Europe/Stockholm:20261104T130000" "END:VEVENT"
                           ;; one occurrence moved to the Tuesday, another moved into the days asked for from outside them
                           "BEGIN:VEVENT" "UID:weekly" "RECURRENCE-ID;TZID=Europe/Stockholm:20261109T090000"
                           "DTSTART;TZID=Europe/Stockholm:20261110T100000" "DTEND;TZID=Europe/Stockholm:20261110T103000"
                           "SUMMARY:Stand-up, moved" "END:VEVENT"
                           "BEGIN:VEVENT" "UID:weekly" "RECURRENCE-ID;TZID=Europe/Stockholm:20261123T090000"
                           "DTSTART;TZID=Europe/Stockholm:20261106T090000" "DURATION:PT15M" "SUMMARY:Stand-up, early" "END:VEVENT"
                           "BEGIN:VEVENT" "UID:away" "SUMMARY:Away" "DTSTART;VALUE=DATE:20261105" "DTEND;VALUE=DATE:20261108" "END:VEVENT"
                           "BEGIN:VEVENT" "UID:call" "SUMMARY:Call" "DTSTART:20261103T150000Z" "DURATION:PT1H" "END:VEVENT"
                           "BEGIN:VEVENT" "UID:floating" "SUMMARY:Lunch" "DTSTART:20261103T120000" "END:VEVENT"
                           "BEGIN:VTODO" "UID:task" "SUMMARY:Report" "DUE;VALUE=DATE:20261106" "END:VTODO"
                           "BEGIN:VEVENT" "UID:later" "SUMMARY:Later" "DTSTART:20261201T090000Z" "END:VEVENT"
                           "END:VCALENDAR"))
         (calendar (first (caldav::icalendar-read text)))
         (stockholm (tuatara.dates:zone-named "Europe/Stockholm"))
         (from (tuatara.dates:zone-universal-time stockholm (* 86400 (tuatara.dates:day-number 2026 11 2))))
         (to (tuatara.dates:zone-universal-time stockholm (* 86400 (tuatara.dates:day-number 2026 11 16))))
         (found (caldav::icalendar-occurrences calendar from to :zone stockholm)))
    (flet ((shown (o)
             (let ((summary (caldav::ical-value-of (caldav::ical-occurrence-component o) "SUMMARY")))
               (if (caldav::ical-occurrence-whole-day o)
                   (list summary (cs-ical-written (* 86400 (caldav::ical-occurrence-start o)))
                         (cs-ical-written (* 86400 (caldav::ical-occurrence-end o))))
                   (list summary (cs-ical-written (tuatara.dates:zone-local-time stockholm (caldav::ical-occurrence-start o)))
                         (cs-ical-written (tuatara.dates:zone-local-time stockholm (caldav::ical-occurrence-end o))))))))
      ;; the Monday of 16 November begins as the times asked for end: not in them
      (is equal '(("Lunch" "20261103T120000" "20261103T120000")
                  ("Call" "20261103T160000" "20261103T170000")
                  ("Stand-up" "20261104T130000" "20261104T131500")
                  ("Away" "20261105T000000" "20261108T000000")
                  ("Report" "20261106T000000" "20261107T000000")
                  ("Stand-up, early" "20261106T090000" "20261106T091500")
                  ("Stand-up, moved" "20261110T100000" "20261110T103000"))
          (mapcar #'shown found)))
    (is string= "20261109T090000"
        (caldav::ical-time-text
         (caldav::ical-occurrence-recurrence-id
          (find "Stand-up, moved" found :key (lambda (o) (caldav::ical-value-of (caldav::ical-occurrence-component o) "SUMMARY"))
                :test #'string=))))
    ;; an event that does not recur has no recurrence id
    (false (caldav::ical-occurrence-recurrence-id
            (find "Call" found :key (lambda (o) (caldav::ical-value-of (caldav::ical-occurrence-component o) "SUMMARY"))
                  :test #'string=)))))

;;; Events and entries

(defun cs-calendar (&rest lines)
  "The VCALENDAR of LINES, the lines between its BEGIN and END."
  (first (caldav::icalendar-read
          (apply #'cs-ical-lines (append '("BEGIN:VCALENDAR" "VERSION:2.0") lines '("END:VCALENDAR"))))))

(defun cs-now ()
  "The time the tests of entries take as now: 2026-10-28 12:00 UTC."
  (+ (* 86400 (tuatara:day-number 2026 10 28)) (* 12 3600)))

(defun cs-zone ()
  "The clock the tests of entries are on."
  (tuatara:zone-named "Europe/Stockholm"))

(defun cs-entries (calendar &rest arguments)
  "CALENDAR's entries as the package writes them, and as the agenda reads them back: two values."
  (let ((text (apply #'caldav::calendar-entries calendar :zone (cs-zone) :now (cs-now) arguments)))
    (values text (tuatara.defaults::org-agenda-file-entries "/x/agenda_caldav.org" (format nil "* Calendar~%~a" text)))))

(defun cs-sent (entry &rest arguments)
  "What ENTRY says as events, on the tests' clock and at their now."
  (apply #'caldav::entry-components entry :zone (cs-zone) :now (cs-now) arguments))

(defparameter *cs-description*
  (format nil "Agenda:~%* not a heading~%#+end_example~%- State \"CANCELLED\" for [2026-11-10 Tue 09:00]~%- [ ] not a task~%Bring <2026-11-04 Wed> and <%%(diary-float t 1 1)>~%CLOCK: [2026-11-03 Tue 09:00]")
  "A description holding what Org would read as markup: a heading, a block's
end, a note, a checkbox, timestamps and a clock.")

(define-test (caldav events-as-entries)
  (let ((calendar (cs-calendar "BEGIN:VEVENT" "UID:meet@example.org" "SUMMARY:Harbour board"
                               "LOCATION:Wharf office\\, Nelson" "CATEGORIES:Work,Public Holiday"
                               "DTSTART;TZID=Europe/Stockholm:20261103T090000" "DTEND;TZID=Europe/Stockholm:20261103T100000"
                               "END:VEVENT"
                               ;; in UTC, shown on the system's clock
                               "BEGIN:VEVENT" "UID:utc@example.org" "SUMMARY:Call" "DTSTART:20261103T130000Z" "END:VEVENT"
                               ;; whole days, the end the day after the last
                               "BEGIN:VEVENT" "UID:days@example.org" "SUMMARY:Christmas" "DTSTART;VALUE=DATE:20261224"
                               "DTEND;VALUE=DATE:20261226" "STATUS:CANCELLED" "END:VEVENT"
                               "BEGIN:VTODO" "UID:task@example.org" "SUMMARY:Pay the rates" "DUE;VALUE=DATE:20261120" "END:VTODO")))
    (is string= (format nil "** Harbour board :Work:Public_Holiday:~%:PROPERTIES:~%:UID: meet@example.org~%:LOCATION: Wharf office, Nelson~%:END:~%<2026-11-03 Tue 09:00-10:00>~%~
                             ** Call~%:PROPERTIES:~%:UID: utc@example.org~%:END:~%<2026-11-03 Tue 14:00>~%~
                             ** Christmas :cancelled:~%:PROPERTIES:~%:UID: days@example.org~%:END:~%<2026-12-24 Thu>--<2026-12-25 Fri>~%~
                             ** TODO Pay the rates~%DEADLINE: <2026-11-20 Fri>~%:PROPERTIES:~%:UID: task@example.org~%:END:~%")
        (values (cs-entries calendar)))))

(define-test (caldav repeats-as-entries)
  (let ((calendar (cs-calendar
                   ;; weekly, one time left out and one moved: Org's repeater and two notes
                   "BEGIN:VEVENT" "UID:weekly@example.org" "SUMMARY:Stand-up"
                   "DTSTART;TZID=Europe/Stockholm:20261103T090000" "DTEND;TZID=Europe/Stockholm:20261103T093000"
                   "RRULE:FREQ=WEEKLY" "EXDATE;TZID=Europe/Stockholm:20261110T090000" "END:VEVENT"
                   "BEGIN:VEVENT" "UID:weekly@example.org" "RECURRENCE-ID;TZID=Europe/Stockholm:20261117T090000"
                   "SUMMARY:Stand-up" "DTSTART;TZID=Europe/Stockholm:20261118T140000" "DTEND;TZID=Europe/Stockholm:20261118T143000" "END:VEVENT"
                   ;; the second Tuesday three times: a date by rule from its first day to its last
                   "BEGIN:VEVENT" "UID:board@example.org" "SUMMARY:Board"
                   "DTSTART;TZID=Europe/Stockholm:20261110T180000" "DTEND;TZID=Europe/Stockholm:20261110T200000"
                   "RRULE:FREQ=MONTHLY;BYDAY=2TU;COUNT=3" "END:VEVENT"
                   ;; weekdays with no end: a date by rule from its first day on
                   "BEGIN:VEVENT" "UID:walk@example.org" "SUMMARY:Walk"
                   "DTSTART;VALUE=DATE:20261102" "RRULE:FREQ=WEEKLY;BYDAY=MO,WE,FR" "END:VEVENT"
                   ;; every other week: Org's repeater; three times only: a cycle of days to its last
                   "BEGIN:VEVENT" "UID:bins@example.org" "SUMMARY:Bins"
                   "DTSTART;VALUE=DATE:20261104" "RRULE:FREQ=WEEKLY;INTERVAL=2" "END:VEVENT"
                   "BEGIN:VEVENT" "UID:glass@example.org" "SUMMARY:Glass"
                   "DTSTART;VALUE=DATE:20261104" "RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=3" "END:VEVENT"
                   ;; every other week on two days: a cycle for each
                   "BEGIN:VEVENT" "UID:choir@example.org" "SUMMARY:Choir"
                   "DTSTART;TZID=Europe/Stockholm:20261103T190000" "RRULE:FREQ=WEEKLY;INTERVAL=2;BYDAY=TU,TH" "END:VEVENT"
                   ;; the last weekday of the month: no Org words for it, so its times written out
                   "BEGIN:VEVENT" "UID:pay@example.org" "SUMMARY:Payday"
                   "DTSTART;VALUE=DATE:20261030" "RRULE:FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1" "END:VEVENT")))
    (multiple-value-bind (text entries)
        (cs-entries calendar :to (+ (cs-now) (* 86400 70)))
      (is string= (format nil "** Stand-up~%:PROPERTIES:~%:UID: weekly@example.org~%:END:~%:LOGBOOK:~%~
                               - State \"CANCELLED\"  for [2026-11-10 Tue 09:00] [2026-10-28 Wed 13:00]~%~
                               - State \"MOVED\"      for [2026-11-17 Tue 09:00] to [2026-11-18 Wed 14:00] [2026-10-28 Wed 13:00]~%~
                               :END:~%<2026-11-03 Tue 09:00-09:30 +1w>~%~
                               ** Board~%:PROPERTIES:~%:UID: board@example.org~%:END:~%~
                               <%%(and (diary-block 11 10 2026 1 12 2027) (diary-float t 2 2)) 18:00-20:00>~%~
                               ** Walk~%:PROPERTIES:~%:UID: walk@example.org~%:END:~%~
                               <%%(and (diary-block 11 2 2026 1 1 2126) (memq (calendar-day-of-week date) '(1 3 5)))>~%~
                               ** Bins~%:PROPERTIES:~%:UID: bins@example.org~%:END:~%~
                               <2026-11-04 Wed +2w>~%~
                               ** Glass~%:PROPERTIES:~%:UID: glass@example.org~%:END:~%~
                               <%%(and (diary-block 11 4 2026 12 2 2026) (diary-cyclic 14 11 4 2026))>~%~
                               ** Choir~%:PROPERTIES:~%:UID: choir@example.org~%:END:~%~
                               <%%(or (diary-cyclic 14 11 3 2026) (diary-cyclic 14 11 5 2026)) 19:00>~%~
                               ** Payday~%:PROPERTIES:~%:UID: pay@example.org~%:RRULE: FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1~%:END:~%~
                               <2026-10-30 Fri>~%<2026-11-30 Mon>~%<2026-12-31 Thu>~%")
          text)
      ;; as the agenda reads them: the times cancelled and moved, and each rule the days the calendar says
      (let ((weekly (find "Stand-up" entries :key #'tuatara:agenda-entry-title :test #'string=)))
        (multiple-value-bind (cancelled moved)
            (tuatara:org-occurrence-notes (tuatara:agenda-entry-text weekly) (tuatara:agenda-entry-position weekly) (tuatara:agenda-entry-end weekly))
          (is equal (list (cons (tuatara:day-number 2026 11 10) 900)) cancelled)
          (is equal (list (cons (cons (tuatara:day-number 2026 11 17) 900) (cons (tuatara:day-number 2026 11 18) 1400))) moved)))
      (flet ((days (title from count)
               (let ((rule (tuatara:org-read-rule (first (tuatara:agenda-entry-stamps (find title entries :key #'tuatara:agenda-entry-title :test #'string=))))))
                 (loop repeat count
                       for day = (tuatara:org-rule-next rule from) then (tuatara:org-rule-next rule (1+ day))
                       while day collect (multiple-value-list (tuatara:day-date day))))))
        (is equal '((2026 11 10) (2026 12 8) (2027 1 12)) (days "Board" (tuatara:day-number 2026 10 1) 5))
        (is equal '((2026 11 2) (2026 11 4) (2026 11 6) (2026 11 9)) (days "Walk" (tuatara:day-number 2026 10 1) 4))
        (is equal '((2026 11 4) (2026 11 18) (2026 12 2)) (days "Glass" (tuatara:day-number 2026 10 1) 5))
        (is equal '((2026 11 3) (2026 11 5) (2026 11 17) (2026 11 19)) (days "Choir" (tuatara:day-number 2026 10 1) 4))))))

(define-test (caldav untrusted-words-stay-words)
  (let ((calendar (cs-calendar "BEGIN:VEVENT" "UID:odd@example.org"
                               "SUMMARY:TODO [[file:/etc/passwd]] costs $5 \\\\input{x} <2026-11-05 Thu> :x:"
                               (format nil "DESCRIPTION:~a" (caldav::ical-text-escaped *cs-description*))
                               "LOCATION:Room <2026-11-06 Fri>"
                               "DTSTART;VALUE=DATE:20261103" "END:VEVENT")))
    (multiple-value-bind (text entries) (cs-entries calendar)
      (let ((entry (find "odd@example.org" entries :key (lambda (e) (cdr (assoc "UID" (tuatara:agenda-entry-properties e) :test #'string=))) :test #'equal)))
        (true entry "the heading is one: ~a" text)
        ;; nothing in it the agenda reads but its own day: no keyword, tags, other times, notes, clocks or tasks
        (is = 2 (length entries))
        (false (tuatara:agenda-entry-todo entry))
        (is equal '() (tuatara:agenda-entry-local-tags entry))
        (is equal '("<2026-11-03 Tue>") (tuatara:agenda-entry-stamps entry))
        (false (tuatara.defaults::agenda-entry-logs entry))
        (is equal '(nil nil) (multiple-value-list (tuatara:org-occurrence-notes (tuatara:agenda-entry-text entry) (tuatara:agenda-entry-position entry) (tuatara:agenda-entry-end entry))))
        (false (search "[[" (tuatara:agenda-entry-title entry)))
        ;; and each read back as the calendar said it
        (let ((event (first (cs-sent entry))))
          (is string= "TODO [[file:/etc/passwd]] costs $5 \\input{x} <2026-11-05 Thu> :x:"
              (caldav::ical-text (caldav::ical-value-of event "SUMMARY")))
          (is string= *cs-description* (caldav::ical-text (caldav::ical-value-of event "DESCRIPTION")))
          (is string= "Room <2026-11-06 Fri>" (caldav::ical-text (caldav::ical-value-of event "LOCATION"))))))))

(defun cs-entry (text)
  "The agenda entry of TEXT, one entry, as the agenda reads it."
  (first (tuatara.defaults::org-agenda-file-entries "/x/agenda_caldav.org" text)))

(defun cs-property-line (component name)
  "COMPONENT's property NAME as one line, or NIL."
  (let ((p (caldav::ical-property-of component name)))
    (and p (caldav::property-line p))))

(define-test (caldav entries-as-events)
  ;; written in Org: times on the system's clock, a repeater, tags, a place and a description
  (let* ((entry (cs-entry (format nil "* Dentist :health:~%:PROPERTIES:~%:LOCATION: Nelson~%:END:~%<2026-11-05 Thu 14:00-15:00 +2w>~%~a"
                                  (caldav::description-block "Bring the card"))))
         (events (cs-sent entry))
         (event (first events)))
    (is = 1 (length events))
    (is string= "VEVENT" (caldav::ical-component-name event))
    (true (cl-ppcre:scan "@tuatara$" (caldav::ical-value-of event "UID")))
    (is string= "SUMMARY:Dentist" (cs-property-line event "SUMMARY"))
    (is string= "DTSTART;TZID=Europe/Stockholm:20261105T140000" (cs-property-line event "DTSTART"))
    (is string= "DTEND;TZID=Europe/Stockholm:20261105T150000" (cs-property-line event "DTEND"))
    (is string= "RRULE:FREQ=WEEKLY;INTERVAL=2" (cs-property-line event "RRULE"))
    (is string= "LOCATION:Nelson" (cs-property-line event "LOCATION"))
    (is string= "CATEGORIES:health" (cs-property-line event "CATEGORIES"))
    (is string= "DESCRIPTION:Bring the card" (cs-property-line event "DESCRIPTION"))
    (is string= "DTSTAMP:20261028T120000Z" (cs-property-line event "DTSTAMP")))
  ;; a form's fields left blank, as the capture template in the README leaves them: none sent
  (let ((event (first (cs-sent (cs-entry (format nil "* Swim~%:PROPERTIES:~%:LOCATION:~%:END:~%<2026-11-05 Thu 07:00-08:00>~%#+name: description~%#+begin_example~%~%#+end_example~%"))))))
    (is string= "SUMMARY:Swim" (cs-property-line event "SUMMARY"))
    (false (cs-property-line event "LOCATION"))
    (false (cs-property-line event "DESCRIPTION")))
  ;; a task with a deadline alone, done
  (let ((task (first (cs-sent (cs-entry (format nil "* DONE Pay the rates~%CLOSED: [2026-11-19 Thu 10:00] DEADLINE: <2026-11-20 Fri>~%:PROPERTIES:~%:UID: task@example.org~%:END:~%"))))))
    (is string= "VTODO" (caldav::ical-component-name task))
    (is string= "DUE;VALUE=DATE:20261120" (cs-property-line task "DUE"))
    (is string= "STATUS:COMPLETED" (cs-property-line task "STATUS"))
    (is string= "COMPLETED:20261119T090000Z" (cs-property-line task "COMPLETED")))
  ;; a time cancelled and one moved: left out, and a change of that one time
  (let* ((events (cs-sent (cs-entry (format nil "* Stand-up :cancelled:~%:PROPERTIES:~%:UID: weekly@example.org~%:END:~%:LOGBOOK:~%~a~%~a~%:END:~%<2026-11-03 Tue 09:00-09:30 +1w>~%"
                                            (tuatara:org-occurrence-note :cancelled (cons (tuatara:day-number 2026 11 10) 900) nil (cs-now))
                                            (tuatara:org-occurrence-note :moved (cons (tuatara:day-number 2026 11 17) 900) (cons (tuatara:day-number 2026 11 18) 1400) (cs-now))))))
         (master (first events)) (change (second events)))
    (is = 2 (length events))
    (is string= "STATUS:CANCELLED" (cs-property-line master "STATUS"))
    (is string= "EXDATE;TZID=Europe/Stockholm:20261110T090000" (cs-property-line master "EXDATE"))
    (is string= "RECURRENCE-ID;TZID=Europe/Stockholm:20261117T090000" (cs-property-line change "RECURRENCE-ID"))
    (is string= "DTSTART;TZID=Europe/Stockholm:20261118T140000" (cs-property-line change "DTSTART"))
    (is string= "DTEND;TZID=Europe/Stockholm:20261118T143000" (cs-property-line change "DTEND"))
    (is string= "UID:weekly@example.org" (cs-property-line change "UID")))
  ;; a date by rule of no first day: from the first day it takes from today
  (let ((event (first (cs-sent (cs-entry (format nil "* Club~%<%%(diary-float t 1 1) 19:00>~%"))))))
    (is string= "DTSTART;TZID=Europe/Stockholm:20261102T190000" (cs-property-line event "DTSTART"))
    (is string= "RRULE:FREQ=MONTHLY;BYDAY=1MO" (cs-property-line event "RRULE")))
  ;; dates by rule as the repeat menu of the date prompt writes them
  (flet ((rule-of (stamp)
           (let ((event (first (cs-sent (cs-entry (format nil "* Rule~%~a~%" stamp))))))
             (list (cs-property-line event "DTSTART") (cs-property-line event "RRULE")))))
    (is equal '("DTSTART;TZID=Europe/Stockholm:20261103T100000" "RRULE:FREQ=MONTHLY;INTERVAL=2;UNTIL=20261231T090000Z;BYDAY=1TU")
        (rule-of "<%%(and (diary-block 11 3 2026 12 31 2026) (diary-float '(1 3 5 7 9 11) 2 1)) 10:00>"))
    (is equal '("DTSTART;TZID=Europe/Stockholm:20261103T090000" "RRULE:FREQ=WEEKLY;INTERVAL=2;BYDAY=TU,TH")
        (rule-of "<%%(or (diary-cyclic 14 11 3 2026) (diary-cyclic 14 11 5 2026)) 09:00>"))
    (is equal '("DTSTART;VALUE=DATE:20261102" "RRULE:FREQ=DAILY;INTERVAL=2;BYDAY=MO,TU,WE,TH,FR")
        (rule-of "<%%(and (diary-block 11 2 2026 1 1 2126) (diary-cyclic 2 11 2 2026) (memq (calendar-day-of-week date) '(1 2 3 4 5)))>")))
  ;; one with a range of days and a time besides: an event each, the second's UID with its day
  (let ((events (cs-sent (cs-entry (format nil "* Trip~%:PROPERTIES:~%:UID: trip@example.org~%:END:~%<2026-12-01 Tue>--<2026-12-03 Thu>~%<2026-12-10 Thu 08:00>~%")))))
    (is equal '("UID:trip@example.org" "UID:trip@example.org-20261210") (mapcar (lambda (e) (cs-property-line e "UID")) events))
    (is equal '("DTSTART;VALUE=DATE:20261201" "DTEND;VALUE=DATE:20261204")
        (list (cs-property-line (first events) "DTSTART") (cs-property-line (first events) "DTEND")))))

(define-test (caldav entries-keep-what-calendars-said)
  ;; read from the calendar and sent back unchanged: its times as the calendar wrote them, its other properties kept
  (let* ((calendar (cs-calendar "BEGIN:VEVENT" "UID:weekly@example.org" "SUMMARY:Stand-up" "CATEGORIES:Public Holiday"
                                "DTSTART;TZID=Europe/Stockholm:20261103T090000" "DURATION:PT30M"
                                "RRULE:FREQ=WEEKLY;BYDAY=TU" "EXDATE;TZID=Europe/Stockholm:20261110T090000"
                                "SEQUENCE:4" "X-CLIENT-THING:kept" "END:VEVENT"
                                "BEGIN:VEVENT" "UID:pay@example.org" "SUMMARY:Payday" "DTSTART;VALUE=DATE:20261030"
                                "RRULE:FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1" "END:VEVENT"))
         (entries (nth-value 1 (cs-entries calendar)))
         (weekly (first (cs-sent (find "Stand-up" entries :key #'tuatara:agenda-entry-title :test #'string=) :agreed calendar)))
         (pay (first (cs-sent (find "Payday" entries :key #'tuatara:agenda-entry-title :test #'string=) :agreed calendar))))
    (is equal '("DTSTART;TZID=Europe/Stockholm:20261103T090000" "DURATION:PT30M" "RRULE:FREQ=WEEKLY;BYDAY=TU"
                "EXDATE;TZID=Europe/Stockholm:20261110T090000" "SEQUENCE:4" "X-CLIENT-THING:kept" "CATEGORIES:Public Holiday")
        (mapcar (lambda (name) (cs-property-line weekly name))
                '("DTSTART" "DURATION" "RRULE" "EXDATE" "SEQUENCE" "X-CLIENT-THING" "CATEGORIES")))
    (false (cs-property-line weekly "DTEND"))
    ;; times written out: the calendar's stand
    (is equal '("DTSTART;VALUE=DATE:20261030" "RRULE:FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-1")
        (list (cs-property-line pay "DTSTART") (cs-property-line pay "RRULE"))))
  ;; its time changed in Org: the entry's time, and the next SEQUENCE
  (let* ((calendar (cs-calendar "BEGIN:VEVENT" "UID:one@example.org" "SUMMARY:Lunch" "SEQUENCE:2"
                                "DTSTART;TZID=Europe/Stockholm:20261103T120000" "DTEND;TZID=Europe/Stockholm:20261103T130000" "END:VEVENT"))
         (event (first (cs-sent (cs-entry (format nil "* Lunch~%:PROPERTIES:~%:UID: one@example.org~%:END:~%<2026-11-04 Wed 12:30-13:30>~%"))
                                :agreed calendar))))
    (is string= "DTSTART;TZID=Europe/Stockholm:20261104T123000" (cs-property-line event "DTSTART"))
    (is string= "SEQUENCE:3" (cs-property-line event "SEQUENCE")))
  ;; lines of its own after its RRULE, read and sent back unchanged: each line once, and the
  ;; same event as the calendar has, so that nothing is sent
  (let* ((calendar (cs-calendar "BEGIN:VEVENT" "UID:birthday@example.org" "DTSTART;VALUE=DATE:20260702"
                                "DTEND;VALUE=DATE:20260703" "CREATED:20260615T125119Z" "RRULE:FREQ=YEARLY"
                                "SEQUENCE:1" "SUMMARY:Birthday" "TRANSP:TRANSPARENT" "X-MOZ-GENERATION:1" "END:VEVENT"))
         (entry (find "Birthday" (nth-value 1 (cs-entries calendar)) :key #'tuatara:agenda-entry-title :test #'string=))
         (sent (cs-sent entry :agreed calendar))
         (names (mapcar #'caldav::ical-property-name (caldav::ical-component-properties (first sent)))))
    (is equal (remove-duplicates names :test #'string-equal :from-end t) names)
    (true (caldav::same-events-p sent (caldav::calendar-events calendar)))))

;;; Syncing with a server: a CalDAV server of the tests' own, in memory

(defstruct (cs-server (:constructor make-cs-server ()))
  "A CalDAV server in memory: EVENTS by path, each (ETAG . TEXT); CHANGES,
each (TOKEN PATH), the newest first; TOKEN, the count of them; REQUESTS, each
\(METHOD PATH), the newest first; with EMPTY-TOKENS, a sync answered with
every event and an empty token, as etesync-dav answers."
  (events (make-hash-table :test 'equal)) (changes '()) (token 0) (requests '()) (refuse-puts nil) (empty-tokens nil))

(defun cs-server-put (server path text)
  "Keep TEXT at PATH on SERVER, as a client's PUT would; its new ETAG."
  (let ((etag (format nil "\"~d\"" (incf (cs-server-token server)))))
    (setf (gethash path (cs-server-events server)) (cons etag text))
    (push (list (cs-server-token server) path) (cs-server-changes server))
    etag))

(defun cs-server-delete (server path)
  "Delete the event at PATH on SERVER."
  (remhash path (cs-server-events server))
  (push (list (incf (cs-server-token server)) path) (cs-server-changes server)))

(defun cs-multistatus (&rest responses)
  "A multistatus answer of RESPONSES, each the XML inside a response."
  (format nil "<?xml version=\"1.0\"?><d:multistatus xmlns:d=\"DAV:\" xmlns:c=\"urn:ietf:params:xml:ns:caldav\">~{<d:response>~a</d:response>~}</d:multistatus>" responses))

(defun cs-ok (href props)
  "A response for HREF of PROPS, found."
  (format nil "<d:href>~a</d:href><d:propstat><d:prop>~a</d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat>" href props))

(defun cs-transport (server)
  "A transport (CALENDAR-SYNC::*TRANSPORT*) answering as SERVER does, for the
user kiwi with the password p\"w: one calendar, /kiwi/home/."
  (lambda (method url &key headers body user password)
    (let ((path (caldav::url-path url))
          (header (lambda (name) (cdr (assoc name headers :test #'string-equal)))))
      (push (list method path) (cs-server-requests server))
      (cond
        ((not (and (equal user "kiwi") (equal password "p\"w"))) (values 401 '() ""))
        ((string= method "PROPFIND")
         (values 207 '()
                 (cond ((search "current-user-principal" body) (cs-multistatus (cs-ok path "<d:current-user-principal><d:href>/kiwi/</d:href></d:current-user-principal>")))
                       ((search "calendar-home-set" body) (cs-multistatus (cs-ok path "<c:calendar-home-set><d:href>/kiwi/</d:href></c:calendar-home-set>")))
                       ((search "resourcetype/><d:displayname" body)
                        (cs-multistatus (cs-ok "/kiwi/" "<d:resourcetype><d:collection/></d:resourcetype>")
                                        (cs-ok "/kiwi/home/" "<d:resourcetype><d:collection/><c:calendar/></d:resourcetype><d:displayname>Home &amp; garden</d:displayname><c:supported-calendar-component-set><c:comp name=\"VEVENT\"/></c:supported-calendar-component-set>")))
                       (t (apply #'cs-multistatus (loop for p being the hash-keys of (cs-server-events server) using (hash-value v)
                                                        collect (cs-ok p (format nil "<d:getetag>~a</d:getetag>" (car v)))))))))
        ((and (string= method "REPORT") (search "sync-collection" body))
         (let* ((since (let ((m (nth-value 1 (cl-ppcre:scan-to-strings "token/(\\d+)<" body)))) (if m (parse-integer (aref m 0)) 0)))
                (paths (if (cs-server-empty-tokens server)
                           (loop for p being the hash-keys of (cs-server-events server) collect p)
                           (remove-duplicates (loop for (n p) in (cs-server-changes server) when (> n since) collect p) :test #'string=))))
           (values 207 '()
                   (format nil "<?xml version=\"1.0\"?><d:multistatus xmlns:d=\"DAV:\">~{<d:response>~a</d:response>~}<d:sync-token>~@[token/~d~]</d:sync-token></d:multistatus>"
                           (loop for p in paths
                                 for v = (gethash p (cs-server-events server))
                                 collect (if v
                                             (cs-ok p (format nil "<d:getetag>~a</d:getetag>" (car v)))
                                             (format nil "<d:href>~a</d:href><d:status>HTTP/1.1 404 Not Found</d:status>" p)))
                           (and (not (cs-server-empty-tokens server)) (cs-server-token server))))))
        ((string= method "REPORT")
         (values 207 '()
                 (apply #'cs-multistatus
                        (loop for p in (let ((paths '())) (cl-ppcre:do-register-groups (p) ("<d:href>([^<]*)</d:href>" body) (push p paths)) (nreverse paths))
                              for v = (gethash p (cs-server-events server))
                              when v collect (cs-ok p (format nil "<d:getetag>~a</d:getetag><c:calendar-data>~a</c:calendar-data>" (car v) (tuatara:xml-escape (cdr v))))))))
        ((string= method "PUT")
         (let ((was (gethash path (cs-server-events server))))
           (cond ((cs-server-refuse-puts server) (values 403 '() ""))
                 ((and (funcall header "If-Match") (not (and was (string= (car was) (funcall header "If-Match"))))) (values 412 '() ""))
                 ((and (funcall header "If-None-Match") was) (values 412 '() ""))
                 (t (values 201 (list (cons "etag" (cs-server-put server path body))) "")))))
        ((string= method "DELETE")
         (let ((was (gethash path (cs-server-events server))))
           (cond ((null was) (values 404 '() ""))
                 ((and (funcall header "If-Match") (string/= (car was) (funcall header "If-Match"))) (values 412 '() ""))
                 (t (cs-server-delete server path) (values 204 '() "")))))
        ((string= method "GET")
         (let ((v (gethash path (cs-server-events server)))) (if v (values 200 '() (cdr v)) (values 404 '() ""))))
        (t (values 405 '() ""))))))

(defun cs-event-text (&rest lines)
  "A calendar file of one event of LINES."
  (apply #'cs-ical-lines (append '("BEGIN:VCALENDAR" "VERSION:2.0" "BEGIN:VEVENT") lines '("END:VEVENT" "END:VCALENDAR"))))

(defmacro with-cs-state (() &body body)
  "Run BODY with the state and cache directories in a folder of its own, deleted after."
  `(let ((home (merge-pathnames (format nil "tuatara-cs-~d/" (sb-posix:getpid)) (uiop:temporary-directory)))
         (state (uiop:getenv "XDG_STATE_HOME")) (cache (uiop:getenv "XDG_CACHE_HOME")))
     (unwind-protect
          (progn (sb-posix:setenv "XDG_STATE_HOME" (namestring (merge-pathnames "state/" home)) 1)
                 (sb-posix:setenv "XDG_CACHE_HOME" (namestring (merge-pathnames "cache/" home)) 1)
                 ,@body)
       (if state (sb-posix:setenv "XDG_STATE_HOME" state 1) (sb-posix:unsetenv "XDG_STATE_HOME"))
       (if cache (sb-posix:setenv "XDG_CACHE_HOME" cache 1) (sb-posix:unsetenv "XDG_CACHE_HOME"))
       (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore))))

(defun cs-sync (server text)
  "TEXT, the calendars' file, synced with SERVER, as the command syncs it: its new text."
  (let* ((caldav::*transport* (cs-transport server))
         (account '(:name "Home" :url "https://cal.example.org/" :user "kiwi" :password "p\"w"))
         (sections (caldav::calendar-sections text (caldav::file-entries text)))
         (passes (loop for c in (caldav::account-calendars account)
                       collect (caldav::sync-calendar account c (caldav::section-of sections (getf c :url))
                                                             :zone (cs-zone) :now (cs-now)))))
    (dolist (p passes)
      (caldav::write-calendar-state (caldav::calendar-pass-folder p) (caldav::calendar-pass-token p) (caldav::calendar-pass-events p)))
    (caldav::merged-text text passes)))

(defun cs-puts (server)
  "How many events SERVER was sent, and forget them."
  (prog1 (count "PUT" (cs-server-requests server) :key #'first :test #'string=)
    (setf (cs-server-requests server) '())))

(define-test (caldav addresses-and-requests)
  ;; https anywhere, http to this machine alone
  (true (caldav::url-allowed-p "https://cal.example.org/dav/"))
  (true (caldav::url-allowed-p "http://localhost:37358/"))
  (true (caldav::url-allowed-p "http://127.0.0.1:5232/kiwi/"))
  (true (caldav::url-allowed-p "http://[::1]:5232/"))
  (false (caldav::url-allowed-p "http://cal.example.org/"))
  (false (caldav::url-allowed-p "http://localhost.example.org/"))
  (false (caldav::url-allowed-p "file:///etc/passwd"))
  (is string= "https://h.example/a/b.ics" (caldav::url-joined "https://h.example/a/" "b.ics"))
  (is string= "https://h.example/x/" (caldav::url-joined "https://h.example/a/b" "/x/"))
  ;; the password goes to curl quoted, never on its command line
  (is string= "\"kiwi:p\\\"w\\\\\"" (caldav::curl-quoted "kiwi:p\"w\\"))
  ;; the headers of the last answer, after a redirect's
  (is equal '(("etag" . "\"7\"") ("content-type" . "text/calendar"))
      (caldav::curl-headers (format nil "HTTP/1.1 301 Moved~c~%Location: /x~c~%~c~%HTTP/1.1 200 OK~c~%ETag: \"7\"~c~%Content-Type: text/calendar~c~%~c~%"
                                           #\Return #\Return #\Return #\Return #\Return #\Return #\Return)))
  ;; a redirect to an address that may not be reached is not followed
  (let ((caldav::*transport* (lambda (method url &rest r) (declare (ignore method r))
                                      (if (search "example.org" url) (values 301 '(("location" . "http://elsewhere.example/")) "") (values 200 '() "")))))
    (fail (caldav::request '() "GET" "https://cal.example.org/") 'caldav::caldav-error)
    ;; nor is an account's password sent to another host, whatever an answer names
    (fail (caldav::request '(:name "Home" :url "https://cal.example.org/" :user "kiwi" :password "x") "GET" "https://other.example.net/kiwi/")
          'caldav::caldav-error)))

(define-test (caldav calendars-synced-both-ways)
  (with-cs-state ()
    (let ((server (make-cs-server)))
      (cs-server-put server "/kiwi/home/dentist.ics"
                     (cs-event-text "UID:dentist@example.org" "SUMMARY:Dentist" "LOCATION:Nelson" "X-THEIRS:kept"
                                    "DTSTART;TZID=Europe/Stockholm:20261105T140000" "DTEND;TZID=Europe/Stockholm:20261105T150000"))
      (cs-server-put server "/kiwi/home/board.ics" (cs-event-text "UID:board@example.org" "SUMMARY:Board" "DTSTART;VALUE=DATE:20261120"))
      ;; a rule Org cannot say, all of its times before the days written out: an entry with none
      (cs-server-put server "/kiwi/home/ended.ics"
                     (cs-event-text "UID:ended@example.org" "SUMMARY:Upgrade" "DTSTART;VALUE=DATE:20250601"
                                    "RRULE:FREQ=MONTHLY;BYDAY=SU;BYSETPOS=1;UNTIL=20250705"))
      ;; fetched: a heading for the calendar, an entry each
      (let ((text (cs-sync server (format nil "#+title: Calendars~%"))))
        (is = 0 (cs-puts server))
        (true (search (format nil "* Home & garden~%:PROPERTIES:~%:CALENDAR: https://cal.example.org/kiwi/home/~%:END:~%") text))
        (true (search (format nil "** Dentist~%:PROPERTIES:~%:UID: dentist@example.org~%:LOCATION: Nelson~%:END:~%<2026-11-05 Thu 14:00-15:00>~%") text))
        ;; again: nothing sent, nothing changed; the entry with no times keeps the calendar's
        (is string= text (cs-sync server text))
        (is = 0 (cs-puts server))
        ;; changed here: the title, a line of your own, a new entry, and a heading of no time, which is not sent
        (let* ((edited (concatenate 'string (cl-ppcre:regex-replace "\\*\\* Dentist" text "** Dentist, the check-up")
                                    (format nil "Bring the card.~%** Lunch with Aroha~%<2026-11-06 Fri 12:00-13:00>~%** Some day~%")))
               (synced (cs-sync server edited))
               (dentist (cdr (gethash "/kiwi/home/dentist.ics" (cs-server-events server)))))
          (is = 2 (cs-puts server))
          (true (search "SUMMARY:Dentist\\, the check-up" dentist))
          (true (search "X-THEIRS:kept" dentist) "what the calendar keeps of its own is sent back")
          ;; the new entry is an event of the server's, its UID written into it
          (let ((uid (nth-value 1 (cl-ppcre:scan-to-strings "\\*\\* Lunch with Aroha\\n:PROPERTIES:\\n:UID: (\\S+@tuatara)\\n" synced))))
            (true uid "the new entry has its UID: ~a" synced)
            (true (find-if (lambda (v) (search (aref uid 0) (cdr v))) (loop for v being the hash-values of (cs-server-events server) collect v))))
          (true (search "Bring the card." synced))
          ;; changed there: a time; deleted there: the board; changed on both sides: the dentist
          (cs-server-put server "/kiwi/home/dentist.ics"
                         (cs-event-text "UID:dentist@example.org" "SUMMARY:Dentist" "LOCATION:Nelson"
                                        "DTSTART;TZID=Europe/Stockholm:20261105T160000" "DTEND;TZID=Europe/Stockholm:20261105T170000"))
          (cs-server-delete server "/kiwi/home/board.ics")
          (let ((both (cs-sync server (cl-ppcre:regex-replace "the check-up" synced "the check-up, early"))))
            (is = 0 (cs-puts server) "a change made on both sides is not sent")
            ;; the server's stands, your lines in it kept, your copy beside it, tagged
            (true (search (format nil "** Dentist~%:PROPERTIES:~%:UID: dentist@example.org~%:LOCATION: Nelson~%:END:~%<2026-11-05 Thu 16:00-17:00>~%Bring the card.~%") both)
                  "the server's version: ~a" both)
            (true (search (format nil "** Dentist, the check-up, early :conflict:~%:PROPERTIES:~%:LOCATION: Nelson~%:END:~%<2026-11-05 Thu 14:00-15:00>~%Bring the card.~%") both)
                  "your copy, no UID: ~a" both)
            (false (search "** Board" both) "the board, deleted there and nothing of yours in it, is gone")
            ;; and once more: nothing sent, the copy and the removed entry left alone
            (is string= both (cs-sync server both))
            (is = 0 (cs-puts server))))))))

(define-test (caldav deleted-where-tokens-are-empty)
  ;; a server answering a sync with every event and an empty token: an event deleted there is seen gone
  (with-cs-state ()
    (let ((server (make-cs-server)))
      (setf (cs-server-empty-tokens server) t)
      (cs-server-put server "/kiwi/home/walk.ics" (cs-event-text "UID:walk@example.org" "SUMMARY:Walk" "DTSTART;VALUE=DATE:20261121"))
      (cs-server-put server "/kiwi/home/swim.ics" (cs-event-text "UID:swim@example.org" "SUMMARY:Swim" "DTSTART;VALUE=DATE:20261122"))
      (cs-server-put server "/kiwi/home/run.ics" (cs-event-text "UID:run@example.org" "SUMMARY:Run" "DTSTART;VALUE=DATE:20261123"))
      (let ((text (cl-ppcre:regex-replace "(<2026-11-23 Mon>\\n)" (cs-sync server (format nil "#+title: Calendars~%"))
                                          (format nil "\\1Bring water.~%"))))
        (true (search "** Walk" text))
        (true (search "Bring water." text))
        (cs-server-delete server "/kiwi/home/walk.ics")
        (cs-server-delete server "/kiwi/home/run.ics")
        (let ((synced (cs-sync server text)))
          ;; nothing of yours in it: gone
          (false (search "** Walk" synced) "the walk deleted there: ~a" synced)
          ;; a line of yours: kept, tagged, its time inactive, so that the agenda shows it no more
          (true (search (format nil "** Run :removed:~%:PROPERTIES:~%:UID: run@example.org~%:END:~%[2026-11-23 Mon]~%Bring water.~%") synced)
                "the run kept: ~a" synced)
          (true (search "<2026-11-22 Sun>" synced))
          (false (search "** Swim :removed:" synced))
          (is = 0 (cs-puts server)))))))

(define-test (caldav cancelled-here-deleted-there)
  ;; an entry cancelled here: its event deleted on the server, the entry kept, tagged, its time still shown
  (with-cs-state ()
    (let ((server (make-cs-server)))
      (cs-server-put server "/kiwi/home/yoga.ics"
                     (cs-event-text "UID:yoga@example.org" "SUMMARY:Yoga" "DTSTART;TZID=Europe/Stockholm:20261110T180000"
                                    "DTEND;TZID=Europe/Stockholm:20261110T190000"))
      (let* ((text (cs-sync server (format nil "#+title: Calendars~%")))
             (cancelled (concatenate 'string
                                     (cl-ppcre:regex-replace "\\*\\* Yoga" text
                                                             (format nil "** CANCELLED Yoga~%CLOSED: [2026-10-28 Wed 12:00]"))
                                     ;; cancelled before it was ever sent
                                     (format nil "** CANCELLED Pottery~%CLOSED: [2026-10-28 Wed 12:00]~%<2026-11-12 Thu 18:00-19:00>~%")))
             (synced (cs-sync server cancelled)))
        (false (gethash "/kiwi/home/yoga.ics" (cs-server-events server)) "the yoga deleted there")
        (true (find "DELETE" (cs-server-requests server) :key #'first :test #'string=))
        (is = 0 (cs-puts server) "the pottery, never sent, is not sent now")
        (true (search (format nil "** CANCELLED Yoga :removed:~%CLOSED: [2026-10-28 Wed 12:00]") synced) "~a" synced)
        (true (search "<2026-11-10 Tue 18:00-19:00>" synced) "its time still active")
        (true (search "** CANCELLED Pottery" synced))
        ;; and again: nothing sent, nothing deleted
        (setf (cs-server-requests server) '())
        (is string= synced (cs-sync server synced))
        (false (find-if (lambda (r) (member (first r) '("PUT" "DELETE") :test #'string=)) (cs-server-requests server)))))))

(define-test (caldav your-lines-kept)
  ;; the lines of an entry that are yours, kept when its event is written again
  (let ((own (format nil "** Dentist :health:~%:PROPERTIES:~%:UID: d@x~%:LOCATION: Nelson~%:MINE: yes~%:END:~%:LOGBOOK:~%- State \"CANCELLED\"  for [2026-11-12 Thu 14:00] [2026-10-28 Wed 13:00]~%CLOCK: [2026-11-05 Thu 14:00]--[2026-11-05 Thu 14:40] =>  0:40~%:END:~%<2026-11-05 Thu 14:00-15:00 +1w>~%Bring the card.~%#+name: description~%#+begin_example~%From the clinic~%#+end_example~%- [ ] floss~%")))
    (multiple-value-bind (props logbook body) (caldav::own-lines own)
      (is equal '(":MINE: yes") props)
      (is equal '("CLOCK: [2026-11-05 Thu 14:00]--[2026-11-05 Thu 14:40] =>  0:40") logbook)
      (is equal '("Bring the card." "- [ ] floss") body)
      (is string= (format nil "** Dentist~%:PROPERTIES:~%:UID: d@x~%:MINE: yes~%:END:~%:LOGBOOK:~%CLOCK: [2026-11-05 Thu 14:00]--[2026-11-05 Thu 14:40] =>  0:40~%:END:~%<2026-11-05 Thu 16:00>~%Bring the card.~%- [ ] floss~%#+name: description~%#+begin_example~%New~%#+end_example~%")
          (caldav::with-own-lines (format nil "** Dentist~%:PROPERTIES:~%:UID: d@x~%:END:~%<2026-11-05 Thu 16:00>~%#+name: description~%#+begin_example~%New~%#+end_example~%")
                                         props logbook body))))
  (is string= "** Board :work:removed:" (caldav::heading-tagged "** Board :work:" "removed"))
  (is string= "** Board :removed:" (caldav::heading-tagged "** Board" "removed"))
  (is string= "** Board :removed:" (caldav::heading-tagged "** Board :removed:" "removed")))

(defun cs-count-lines (regex text)
  "How many lines of TEXT REGEX matches."
  (/ (length (cl-ppcre:all-matches (concatenate 'string "(?m)" regex) text)) 2))

(define-test (caldav untrusted-calendars-bounded)
  ;; a rule's lists: each value once, no more of them than there are values
  (is equal '(1 2) (caldav::ical-recur-by-hour (caldav::read-ical-recur "FREQ=DAILY;BYHOUR=2,1,2,1")))
  (false (caldav::read-ical-recur (format nil "FREQ=DAILY;BYHOUR=~{~a~^,~}" (loop repeat 20000 collect 0))))
  (false (caldav::read-ical-recur (format nil "FREQ=DAILY;BYDAY=~{~a~^,~}" (loop repeat 20000 collect "MO"))))
  (is = 1 (length (caldav::ical-recur-by-day (caldav::read-ical-recur "FREQ=WEEKLY;BYDAY=MO,MO,MO"))))
  ;; a time zone's name of a megabyte is no zone's, at once
  (let ((zones (caldav::calendar-zones (cs-calendar)))
        (start (get-internal-real-time)))
    (false (funcall zones (format nil "~{~a~^/~}" (loop repeat 100000 collect "Europe"))))
    (true (< (- (get-internal-real-time) start) (* 1/2 internal-time-units-per-second))))
  ;; a VTIMEZONE whose only change is past the years worked out: no zone, not an error
  (false (caldav::vtimezone-zone
          (first (caldav::ical-components-of
                  (cs-calendar "BEGIN:VTIMEZONE" "TZID:Far" "BEGIN:STANDARD" "DTSTART:99990101T000000"
                               "TZOFFSETFROM:+0100" "TZOFFSETTO:+0100" "END:STANDARD" "END:VTIMEZONE")
                  "VTIMEZONE"))))
  ;; one UID of many changes, grouped in order, at once
  (let* ((calendar (caldav::make-ical-component
                    "VCALENDAR" '()
                    (cons (caldav::make-ical-component "VEVENT" (list (caldav::make-ical-property "UID" "u")))
                          (loop for i below 60000
                                collect (caldav::make-ical-component
                                         "VEVENT" (list (caldav::make-ical-property "UID" "u")
                                                        (caldav::make-ical-property "RECURRENCE-ID" (format nil "~d" i))))))))
         (start (get-internal-real-time))
         (groups (caldav::uid-groups calendar)))
    (true (< (- (get-internal-real-time) start) internal-time-units-per-second))
    (is = 1 (length groups))
    (false (caldav::ical-property-of (first (first groups)) "RECURRENCE-ID"))
    (is string= "59999" (caldav::ical-value-of (car (last (first groups))) "RECURRENCE-ID")))
  ;; many times left out of a rule cost no time each against the others
  (let* ((calendar (cs-calendar "BEGIN:VEVENT" "UID:x" "DTSTART:20260101T090000Z" "RRULE:FREQ=DAILY;COUNT=5000"
                                (format nil "EXDATE:~{~a~^,~}" (loop for d from 0 below 5000 by 2
                                                                     collect (cs-ical-written (+ (cs-ical-local "20260101T090000") (* d 86400)))))
                                "END:VEVENT"))
         (start (get-internal-real-time))
         (found (caldav::icalendar-occurrences calendar (* 86400 (tuatara:day-number 2026 1 1)) (* 86400 (tuatara:day-number 2041 1 1))
                                                      :zone (tuatara:fixed-time-zone 0 "UTC"))))
    (is = 2500 (length found))
    (true (< (- (get-internal-real-time) start) (* 2 internal-time-units-per-second))))
  ;; a rule of hours written as its first time alone, not a timestamp an hour
  (let ((text (cs-entries (cs-calendar "BEGIN:VEVENT" "UID:h" "SUMMARY:Ping" "DTSTART:20261029T090000Z" "RRULE:FREQ=HOURLY" "END:VEVENT"))))
    (is = 1 (count #\< text)))
  ;; the state of two long addresses alike for 100 characters kept apart
  (let ((a (format nil "https://cal.example.org/~a/a/" (make-string 150 :initial-element #\x)))
        (b (format nil "https://cal.example.org/~a/b/" (make-string 150 :initial-element #\x))))
    (false (string= (caldav::safe-name a) (caldav::safe-name b))))
  ;; words with no mark come back as they are
  (let ((words "plain words"))
    (true (eq words (caldav::agenda-unguarded words))))
  ;; a line break in a name or password never reaches curl's configuration
  (true (search "line break"
                (handler-case (progn (caldav::curl-request "GET" "https://cal.example.invalid/" :user (format nil "kiwi~%url = https://elsewhere.invalid/") :password "x") "sent")
                  (caldav::caldav-error (e) (princ-to-string e))))))

(define-test (caldav refusals-and-limits)
  (with-cs-state ()
    (let ((server (make-cs-server)))
      (dotimes (i 3)
        (cs-server-put server (format nil "/kiwi/home/e~d.ics" i)
                       (cs-event-text (format nil "UID:e~d@example.org" i) (format nil "SUMMARY:Event ~d" i) "DTSTART;VALUE=DATE:20261120")))
      ;; a sync fetches so many at most; the next fetches the rest
      (let ((text (let ((caldav::*most-fetched* 2)) (cs-sync server ""))))
        (is = 2 (cs-count-lines "^\\*\\* Event" text))
        (let ((text (let ((caldav::*most-fetched* 2)) (cs-sync server text))))
          (is = 3 (cs-count-lines "^\\*\\* Event" text))
          ;; a server refusing a change: noted, the entry kept, what changed there fetched all the same
          (setf (cs-server-refuse-puts server) t)
          (cs-server-put server "/kiwi/home/e0.ics"
                         (cs-event-text "UID:e0@example.org" "SUMMARY:Event zero" "DTSTART;VALUE=DATE:20261120"))
          (let* ((edited (cl-ppcre:regex-replace "\\*\\* Event 1" text "** Event one"))
                 (caldav::*transport* (cs-transport server))
                 (account '(:name "Home" :url "https://cal.example.org/" :user "kiwi" :password "p\"w"))
                 (sections (caldav::calendar-sections edited (caldav::file-entries edited)))
                 (calendar (first (caldav::account-calendars account)))
                 (pass (caldav::sync-calendar account calendar (caldav::section-of sections (getf calendar :url))
                                                     :zone (cs-zone) :now (cs-now))))
            (is = 1 (length (caldav::calendar-pass-failed pass)))
            (true (search "refused the change of Event one" (first (caldav::calendar-pass-failed pass))))
            (is = 1 (caldav::calendar-pass-fetched pass))
            (true (search "** Event zero" (caldav::merged-text edited (list pass))))))))))

(define-test (caldav failures-told)
  ;; a server not reached says why, as curl found it
  (let ((said (handler-case (progn (caldav::curl-request "GET" "http://127.0.0.1:9/") "reached")
                (caldav::caldav-error (e) (princ-to-string e)))))
    (true (search "http://127.0.0.1:9/ not reached: " said) "said: ~a" said)
    (true (search "connect" said) "said: ~a" said))
  ;; failures are told as a notification too, nothing when there are none
  (let ((told '()))
    (sb-int:encapsulate 'tuatara:notify 'cs-told
                        (lambda (function title words &rest keys)
                          (declare (ignore function))
                          (push (list title words (getf keys :source)) told)))
    (unwind-protect
         (progn
           (caldav::tell-failures "Calendars not all synced" '())
           (is equal '() told)
           (caldav::tell-failures "Calendars not all synced" '("EteSync: http://localhost:37358/ not reached: Could not connect" "Holidays: bad"))
           (is equal '(("Calendars not all synced" "EteSync: http://localhost:37358/ not reached: Could not connect; Holidays: bad" "Calendars"))
               told))
      (sb-int:unencapsulate 'tuatara:notify 'cs-told))))

(define-test (caldav calendars-chosen)
  ;; every calendar when the account names none, else those it names, in any case
  (let ((calendars '((:name "Personal" :url "a") (:name "Family" :url "b") (:name "Work" :url "c"))))
    (is equal '("a" "b" "c") (mapcar (lambda (c) (getf c :url)) (caldav::chosen-calendars '(:name "E") calendars)))
    (is equal '("a" "c") (mapcar (lambda (c) (getf c :url))
                                 (caldav::chosen-calendars '(:name "E" :calendars ("work" "Personal")) calendars)))
    (is equal '() (caldav::chosen-calendars '(:name "E" :calendars ("Holidays")) calendars))))

(define-test (caldav password-commands)
  (flet ((password (command) (caldav::command-password (list :name "EteSync" :password-command command))))
    ;; the first line printed, quotes kept as a shell keeps them, no shell run
    (is string= "p\"w x" (password "printf '%s\\nsecond line\\n' 'p\"w x'"))
    (is string= "$HOME;echo" (password "printf %s '$HOME;echo'"))
    ;; a command failing, or printing nothing, says so and never what it printed
    (let ((said (handler-case (password "sh -c 'echo secret; exit 3'") (caldav::caldav-error (e) (princ-to-string e)))))
      (true (search "gave no password" said))
      (false (search "secret" said)))
    (fail (password "true") 'caldav::caldav-error)
    (fail (password "no-such-program-here x") 'caldav::caldav-error)
    (fail (password "printf 'open") 'caldav::caldav-error)
    ;; a GnuPG file's passphrase, the file decrypted, in a GnuPG home of the test's own
    (when (tuatara:find-program "gpg")
      (let ((home (merge-pathnames (format nil "tuatara-gpg-~d/" (sb-posix:getpid)) (uiop:temporary-directory)))
            (was (uiop:getenv "GNUPGHOME")))
        (unwind-protect
             (progn
               (ensure-directories-exist home)
               (sb-posix:chmod (namestring home) #o700)
               (sb-posix:setenv "GNUPGHOME" (namestring home) 1)
               (let ((clear (namestring (merge-pathnames "pass" home)))
                     (sealed (namestring (merge-pathnames "pass.gpg" home))))
                 (with-open-file (out clear :direction :output) (format out "dav-password~%"))
                 (true (nth-value 1 (tuatara:program-output (list (tuatara:find-program "gpg") "--batch" "--quiet" "--pinentry-mode" "loopback"
                                                          "--passphrase" "kiwi" "--symmetric" "--output" sealed clear)
                                                    :seconds 30)))
                 (is string= "dav-password"
                     (password (format nil "gpg --quiet --batch --pinentry-mode loopback --passphrase kiwi --for-your-eyes-only --no-tty --decrypt ~a" sealed)))))
          (if was (sb-posix:setenv "GNUPGHOME" was 1) (sb-posix:unsetenv "GNUPGHOME"))
          (ignore-errors (uiop:run-program (list "gpgconf" "--homedir" (namestring home) "--kill" "gpg-agent")))
          (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore))))))

(define-test (caldav password-wiped)
  ;; the password is in memory while the work runs, and wiped when it ends, an error too
  (let ((kept nil))
    (caldav::with-signed-in (signed '(:name "E" :password-command "printf dav-password"))
      (setf kept (getf signed :password))
      (is string= "dav-password" kept))
    (true (every (lambda (c) (char= c (code-char 0))) kept))
    (ignore-errors
     (caldav::with-signed-in (signed '(:name "E" :password-command "printf other"))
       (setf kept (getf signed :password))
       (error "midway")))
    (true (every (lambda (c) (char= c (code-char 0))) kept))))

(define-test (caldav syncs-on-their-own)
  (let ((started '())
        (caldav::*next-sync* nil)
        (caldav::*sync-job* nil))
    (sb-int:encapsulate 'caldav::start-sync 'cs-started (lambda (function quiet) (declare (ignore function)) (push quiet started) nil))
    (unwind-protect
         (let ((tuatara-user::*caldav-accounts* '((:name "E" :url "http://localhost:37358/" :user "u")))
               (tuatara-user::*caldav-minutes* 1))
           ;; the first a few seconds after starting, the editor sleeping until then
           (is = caldav::*first-sync-seconds* (caldav::sync-tick nil))
           (true (<= 1 (caldav::sync-tick nil) caldav::*first-sync-seconds*))
           (is equal '() started)
           ;; due: one sync, quiet, and the next a minute on
           (setf caldav::*next-sync* (- (get-universal-time) 1))
           (true (<= 59 (caldav::sync-tick nil) 60))
           (is equal '(t) started)
           ;; none when not set, or with no accounts
           (let ((tuatara-user::*caldav-minutes* nil)) (false (caldav::sync-tick nil)))
           (let ((tuatara-user::*caldav-accounts* '())) (false (caldav::sync-tick nil))))
      (sb-int:unencapsulate 'caldav::start-sync 'cs-started)))
  ;; a sync on its own tells a failure once, not again at each sync while it lasts
  (let ((told '()) (caldav::*told-last* nil))
    (sb-int:encapsulate 'tuatara:notify 'cs-told (lambda (function title words &rest keys) (declare (ignore function keys)) (push (list title words) told)))
    (unwind-protect
         (flet ((sync-failing (why quiet)
                  (let ((job (caldav::make-caldav-job nil quiet)))
                    (setf (caldav::caldav-job-failed job) (and why (list why)))
                    (caldav::job-take-in job))))
           (sync-failing "EteSync: not reached" t)
           (sync-failing "EteSync: not reached" t)
           (is = 1 (length told))
           (sync-failing "EteSync: refused the name and password" t)
           (is = 2 (length told))
           ;; asked for, it always tells
           (sync-failing "EteSync: refused the name and password" nil)
           (is = 3 (length told)))
      (sb-int:unencapsulate 'tuatara:notify 'cs-told))))

(define-test (caldav new-event-in-a-form)
  ;; C-c s n: the calendar chosen, the time asked, the title typed, filed under the calendar's heading
  (with-cs-state ()
    (let ((file (namestring (merge-pathnames (format nil "tuatara-cs-new-~d.org" (sb-posix:getpid)) (uiop:temporary-directory)))))
      (unwind-protect
           (let ((caldav::*caldav-file* file))
             (alexandria:write-string-into-file
              (format nil "* Home~%:PROPERTIES:~%:CALENDAR: https://cal.example.org/kiwi/home/~%:END:~%* Work~%:PROPERTIES:~%:CALENDAR: https://cal.example.org/kiwi/work/~%:END:~%")
              file :if-exists :supersede)
             (run-keys "M-x c a l d a v - e v e n t - n e w RET W o r k RET D e n t i s t RET N e l s o n RET 2 0 2 6 - 1 1 - 0 5 SPC 1 4 : 0 0 RET B r i n g SPC t h e SPC c a r d C-c C-c"
                       :text "x" :width 120 :height 16)
             (let ((text (file-text file)))
               (true (search (format nil "* Work~%:PROPERTIES:~%:CALENDAR: https://cal.example.org/kiwi/work/~%:END:~%** Dentist~%:PROPERTIES:~%:LOCATION: Nelson~%:END:~%<2026-11-05 Thu 14:00>~%#+name: description~%#+begin_example~%Bring the card~%#+end_example")
                             text)
                     "~a" text)
               ;; an entry the next sync sends as an event
               (let ((entry (find "Dentist" (tuatara.defaults::org-agenda-file-entries file text) :key #'tuatara:agenda-entry-title :test #'string=)))
                 (true entry)
                 ;; sent with its place and description
                 (let ((event (first (cs-sent entry))))
                   (is string= "Dentist" (caldav::ical-value-of event "SUMMARY"))
                   (is string= "Nelson" (caldav::ical-value-of event "LOCATION"))
                   (is string= "Bring the card" (caldav::ical-value-of event "DESCRIPTION")))))
             ;; no place and no description: neither sent
             (run-keys "M-x c a l d a v - e v e n t - n e w RET H o m e RET L u n c h RET RET 2 0 2 6 - 1 1 - 0 6 SPC 1 2 : 0 0 RET C-c C-c"
                       :text "x" :width 120 :height 16)
             (let* ((text (file-text file))
                    (entry (find "Lunch" (tuatara.defaults::org-agenda-file-entries file text) :key #'tuatara:agenda-entry-title :test #'string=))
                    (event (first (cs-sent entry))))
               (true (search "<2026-11-06 Fri 12:00>" text) "~a" text)
               (is string= "Lunch" (caldav::ical-value-of event "SUMMARY"))
               (false (caldav::ical-value-of event "LOCATION"))
               (false (caldav::ical-value-of event "DESCRIPTION"))))
        (let ((b (tuatara.buffer:find-buffer (file-namestring file)))) (when b (tuatara.buffer:buffer-mark-saved b) (tuatara.buffer:kill-buffer b)))
        (uiop:delete-file-if-exists file)))))
