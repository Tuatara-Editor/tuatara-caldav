;;;; occurrences.lisp --- The occurrences of a calendar file's events and tasks within the times asked for.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; RFC 5545, sections 3.8.5 and 3.8.4.4. An event happens at its DTSTART and
;;;; at the times its RRULE and RDATE give, less those EXDATE takes away; a
;;;; component of the same UID with a RECURRENCE-ID changes the one
;;;; occurrence it names. Each occurrence lasts what DTEND or DURATION says:
;;;; a day for a date alone without either, nothing for a time.

(in-package #:caldav)

(defstruct (ical-occurrence (:constructor make-ical-occurrence (start end whole-day component &optional recurrence-id)))
  "An occurrence of an event or a task: its START and END, universal times,
or for WHOLE-DAY days (day numbers, END the day after the last); the
COMPONENT it comes from (the one changing it, for an occurrence changed);
and RECURRENCE-ID, the ICAL-TIME of the occurrence as its rule gave it, NIL
for a component that does not recur."
  (start 0 :type integer)
  (end 0 :type integer)
  (whole-day nil)
  (component nil)
  (recurrence-id nil))

(defun time-zone-of (time zones floating)
  "The time zone TIME, an ICAL-TIME, is in: UTC, the zone ZONES gives for its TZID, or FLOATING for a floating time or a TZID of no zone."
  (let ((zone (ical-time-zone time)))
    (cond ((eq zone :utc) (fixed-time-zone 0 "UTC"))
          ((stringp zone) (or (funcall zones zone) floating))
          (t floating))))

(defun local-in (time zone zones floating)
  "TIME, an ICAL-TIME, as a local time on ZONE's clock: by its own zone's
universal time, or as it stands when it is a date or in ZONE already."
  (let ((own (time-zone-of time zones floating)))
    (if (or (ical-time-date-p time) (eq own zone))
        (ical-local time)
        (zone-local-time zone (zone-universal-time own (ical-local time))))))

(defun component-span (component zones floating)
  "COMPONENT's start, an ICAL-TIME, its zone, and how long it lasts as local
seconds (whole days for a date): three values; NIL when it has no start."
  (let* ((start (or (ical-property-time (ical-property-of component "DTSTART"))
                    (and (string= (ical-component-name component) "VTODO")
                         (ical-property-time (ical-property-of component "DUE"))))))
    (when start
      (let* ((zone (time-zone-of start zones floating))
             (end (or (ical-property-time (ical-property-of component "DTEND"))
                      (and (string= (ical-component-name component) "VTODO")
                           (ical-property-of component "DTSTART")
                           (ical-property-time (ical-property-of component "DUE")))))
             (duration (let ((v (ical-value-of component "DURATION")))
                         (and v (multiple-value-bind (days seconds) (read-ical-duration v)
                                  (and days (+ (* 86400 days) seconds))))))
             (length (cond (end (if (ical-time-date-p start)
                                    (* 86400 (- (ical-time-day end) (ical-time-day start)))
                                    (- (local-in end zone zones floating) (ical-local start))))
                           (duration duration)
                           ((ical-time-date-p start) 86400)
                           (t 0))))
        (values start zone (max 0 length))))))

(defparameter *event-kinds* '("VEVENT" "VTODO")
  "The components of a calendar that are its events and tasks.")

(defun uid-groups (calendar &optional (kinds *event-kinds*))
  "The components of CALENDAR of KINDS by UID, in the order first met: a list
of lists, each the one recurring first and those changing its occurrences after."
  ;; each UID's (MASTERS . CHANGES), both in reverse, put in order once at the end
  (let ((groups (make-hash-table :test 'equal)) (order '()))
    (dolist (c (ical-components-of calendar))
      (when (member (ical-component-name c) kinds :test #'string-equal)
        (let* ((uid (or (ical-value-of c "UID") c))
               (group (or (gethash uid groups)
                          (progn (push uid order) (setf (gethash uid groups) (cons '() '()))))))
          (if (ical-property-of c "RECURRENCE-ID")
              (push c (cdr group))
              (push c (car group))))))
    (loop for uid in (nreverse order)
          for (masters . changes) = (gethash uid groups)
          collect (append masters (nreverse changes)))))

(defun group-of-uid (calendar uid)
  "The components of CALENDAR of UID (UID-GROUPS), or NIL."
  (find uid (uid-groups calendar) :key (lambda (members) (ical-value-of (first members) "UID")) :test #'equal))

(defun calendar-events (calendar)
  "The events and tasks of CALENDAR (*EVENT-KINDS*), in the order written."
  (remove-if-not (lambda (c) (member (ical-component-name c) *event-kinds* :test #'string-equal))
                 (ical-components-of calendar)))

(defun icalendar-occurrences (calendars from to &key (zone (system-time-zone)) (kinds *event-kinds*))
  "The occurrences of the components of CALENDARS (a VCALENDAR or a list of
them) of KINDS that overlap the universal times FROM to TO, as
ICAL-OCCURRENCE structures in order of their start. Floating times and
dates alone are counted on ZONE's clock. A component cancelled is given too;
its STATUS says so."
  (let ((found '())
        (local-from (zone-local-time zone from))
        (local-to (zone-local-time zone to)))
    (dolist (calendar (if (listp calendars) calendars (list calendars)))
      (let ((zones (calendar-zones calendar)))
        (dolist (members (uid-groups calendar kinds))
          (let* ((master (and (not (ical-property-of (first members) "RECURRENCE-ID")) (first members)))
                 (changes (if master (rest members) members)))
            (labels ((add (component start-local length whole-day zone recurrence-id)
                       ;; an occurrence of COMPONENT at START-LOCAL on ZONE's clock, if it overlaps
                       (if whole-day
                           (let ((first (floor start-local 86400)) (days (max 1 (ceiling length 86400))))
                             (when (and (< (* 86400 first) local-to) (> (* 86400 (+ first days)) local-from))
                               (push (make-ical-occurrence first (+ first days) t component recurrence-id) found)))
                           (let ((start (zone-universal-time zone start-local))
                                 (end (zone-universal-time zone (+ start-local length))))
                             (when (and (< start to) (or (> end from) (and (= end start) (>= start from))))
                               (push (make-ical-occurrence start end nil component recurrence-id) found)))))
                     (single (component recurrence-id)
                       (multiple-value-bind (start own-zone length) (component-span component zones zone)
                         (when start
                           (add component (ical-local start) length (ical-time-date-p start) own-zone recurrence-id)))))
              (if (null master)
                  (dolist (c changes) (single c (ical-property-time (ical-property-of c "RECURRENCE-ID"))))
                  (multiple-value-bind (start start-zone length) (component-span master zones zone)
                    (when start
                      (let* ((date (ical-time-date-p start))
                             (start-local (ical-local start))
                             (universal (if date #'identity (lambda (local) (zone-universal-time start-zone local))))
                             (window-from (if date (- local-from length) (- from length 1)))
                             (window-to (if date local-to to))
                             (rules (loop for p in (ical-properties-of master "RRULE")
                                          for r = (read-ical-recur (ical-property-value p) (ical-time-zone start))
                                          when r collect r))
                             (local-of (lambda (time) (local-in time start-zone zones zone)))
                             ;; the times left out, a table to look them up in
                             (excluded (let ((table (make-hash-table)))
                                         (dolist (p (ical-properties-of master "EXDATE") table)
                                           (dolist (time (ical-property-times p))
                                             (setf (gethash (funcall local-of time) table) t)))))
                             (changed (make-hash-table))
                             (instances '()))
                        (dolist (c changes)
                          (let ((id (ical-property-time (ical-property-of c "RECURRENCE-ID"))))
                            (when id (setf (gethash (funcall local-of id) changed) c))))
                        (setf instances
                              (if rules
                                  (loop for r in rules nconc (recur-expand r start-local window-from window-to :universal universal :date date))
                                  (let ((u (funcall universal start-local)))
                                    (and (>= u window-from) (< u window-to) (list start-local)))))
                        (dolist (p (ical-properties-of master "RDATE"))
                          (dolist (time (ical-property-times p))
                            (let* ((local (funcall local-of time)) (u (funcall universal local)))
                              (when (and (>= u window-from) (< u window-to)) (push local instances)))))
                        ;; in order, each once, those left out taken out: by sorting and a table, never list against list
                        (setf instances (loop for (local . more) on (sort instances #'<)
                                              unless (or (gethash local excluded) (eql local (first more)))
                                                collect local))
                        (dolist (local instances)
                          (let ((id (make-ical-time (floor local 86400) (and (not date) (mod local 86400))
                                                    (and (not date) (ical-time-zone start))))
                                (change (gethash local changed)))
                            (if change
                                (progn (single change (and (or rules (ical-property-of master "RDATE")) id))
                                       (remhash local changed))
                                (add master local length date start-zone (and (or rules (ical-property-of master "RDATE")) id)))))
                        ;; changes moved here from occurrences outside the times asked for
                        (maphash (lambda (local c)
                                   (unless (gethash local excluded)
                                     (single c (ical-property-time (ical-property-of c "RECURRENCE-ID")))))
                                 changed))))))))))
    (sort found #'< :key (lambda (o) (if (ical-occurrence-whole-day o)
                                         (zone-universal-time zone (* 86400 (ical-occurrence-start o)))
                                         (ical-occurrence-start o))))))

(setf (documentation 'ical-occurrence-p 'function) "True when the object given is an ICAL-OCCURRENCE."
      (documentation 'ical-occurrence-start 'function) "When an occurrence begins: a universal time, or a day number for one of whole days."
      (documentation 'ical-occurrence-end 'function) "When an occurrence ends: a universal time, or for one of whole days the day after its last."
      (documentation 'ical-occurrence-whole-day 'function) "True when an occurrence is of whole days."
      (documentation 'ical-occurrence-component 'function) "The component an occurrence comes from: the one changing it, for an occurrence changed."
      (documentation 'ical-occurrence-recurrence-id 'function) "An occurrence's time as its rule gave it, an ICAL-TIME, or NIL for a component that does not recur.")
