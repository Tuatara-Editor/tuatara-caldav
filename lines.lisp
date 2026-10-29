;;;; lines.lisp --- Calendar files read into components and properties, and written back.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; RFC 5545, section 3.1: a calendar file is lines, a long one folded onto
;;;; the next lines each beginning with a space or a tab. A line is a name,
;;;; parameters after semicolons, a colon and the value. BEGIN and END lines
;;;; nest components. Parameter values carry ^n, ^' and ^^ as RFC 6868 says.
;;;; What is read is kept as written: values are read when asked for (values.lisp).

(in-package #:caldav)

(define-condition icalendar-error (error)
  ((text :initarg :text :reader icalendar-error-text)
   (line :initarg :line :initform nil :reader icalendar-error-line))
  (:report (lambda (condition stream)
             (format stream "Not a calendar file: ~a~@[, at line ~d~]" (icalendar-error-text condition) (icalendar-error-line condition))))
  (:documentation "Text read as a calendar file that is not one, or is past the bounds: what was wrong, and on which line."))

(setf (documentation 'icalendar-error-text 'function) "What was wrong with the text read as a calendar file."
      (documentation 'icalendar-error-line 'function) "The line of the text read as a calendar file where it went wrong, counting from 1, or NIL.")

(defparameter *icalendar-largest* (* 16 1024 1024)
  "The most characters of a calendar file read: a larger one is refused.")

(defparameter *icalendar-deepest* 20
  "The most components of a calendar file read one within another: deeper is refused.")

(defstruct (ical-property (:constructor make-ical-property (name value &optional parameters)))
  "A property of a calendar file's component: its NAME in capitals (DTSTART),
its VALUE as written (a string, escapes kept), and its PARAMETERS, a list of
\(NAME . VALUES), the name in capitals and VALUES a list of strings."
  (name "" :type string)
  (value "" :type string)
  (parameters nil :type list))

(defstruct (ical-component (:constructor make-ical-component (name &optional properties components)))
  "A component of a calendar file: its NAME in capitals (VCALENDAR, VEVENT),
its PROPERTIES in the order written, and the COMPONENTS within it."
  (name "" :type string)
  (properties nil :type list)
  (components nil :type list))

(defun ical-property-of (component name)
  "COMPONENT's first property named NAME (DTSTART), or NIL."
  (find name (ical-component-properties component) :key #'ical-property-name :test #'string-equal))

(defun ical-properties-of (component name)
  "COMPONENT's properties named NAME, in the order written."
  (remove name (ical-component-properties component) :key #'ical-property-name :test-not #'string-equal))

(defun ical-value-of (component name)
  "The value of COMPONENT's first property named NAME as written, or NIL."
  (let ((property (ical-property-of component name)))
    (and property (ical-property-value property))))

(defun ical-parameter-values-of (property name)
  "The values of PROPERTY's parameter NAME (TZID), a list of strings; NIL when it has none."
  (cdr (assoc name (ical-property-parameters property) :test #'string-equal)))

(defun ical-parameter-of (property name)
  "The first value of PROPERTY's parameter NAME (TZID), or NIL."
  (first (ical-parameter-values-of property name)))

(defun ical-components-of (component &optional name)
  "The components within COMPONENT, only those named NAME when it is given."
  (if name
      (remove name (ical-component-components component) :key #'ical-component-name :test-not #'string-equal)
      (ical-component-components component)))

;;; Reading

(defun name-char-p (c)
  "True when C may be in a property's or a parameter's name: a letter, a digit or -."
  (or (char<= #\a c #\z) (char<= #\A c #\Z) (char<= #\0 c #\9) (char= c #\-)))

(defun parameter-value-decoded (text)
  "TEXT, a parameter's value, with ^n read as a line break, ^' as \" and ^^ as ^."
  (if (not (find #\^ text))
      text
      (with-output-to-string (out)
        (loop with i = 0
              while (< i (length text))
              do (let ((c (char text i)))
                   (if (and (char= c #\^) (< (1+ i) (length text)) (find (char text (1+ i)) "n'^"))
                       (progn (write-char (case (char text (1+ i)) (#\n #\Newline) (#\' #\") (t #\^)) out)
                              (incf i 2))
                       (progn (write-char c out) (incf i))))))))

(defun read-content-line (text number)
  "The property TEXT, one unfolded line, writes, read at line NUMBER: an ICAL-PROPERTY."
  (let ((at 0) (end (length text)))
    (labels ((fail (what) (error 'icalendar-error :text what :line number))
             (name ()
               (let ((stop (or (position-if-not #'name-char-p text :start at) end)))
                 (when (= stop at) (fail "a line with no name"))
                 (prog1 (string-upcase (subseq text at stop)) (setf at stop))))
             (parameter-value ()
               (if (and (< at end) (char= (char text at) #\"))
                   (let ((close (position #\" text :start (1+ at))))
                     (unless close (fail "a quoted parameter value not closed"))
                     (prog1 (parameter-value-decoded (subseq text (1+ at) close)) (setf at (1+ close))))
                   (let ((stop (or (position-if (lambda (c) (find c ";:,\"")) text :start at) end)))
                     (prog1 (parameter-value-decoded (subseq text at stop)) (setf at stop))))))
      (let ((name (name)) (parameters '()))
        (loop while (and (< at end) (char= (char text at) #\;))
              do (incf at)
                 (let ((parameter (name)))
                   (unless (and (< at end) (char= (char text at) #\=)) (fail (format nil "parameter ~a with no =" parameter)))
                   (incf at)
                   (let ((values (list (parameter-value))))
                     (loop while (and (< at end) (char= (char text at) #\,))
                           do (incf at) (push (parameter-value) values))
                     (push (cons parameter (nreverse values)) parameters))))
        (unless (and (< at end) (char= (char text at) #\:)) (fail (format nil "~a with no : before its value" name)))
        (make-ical-property name (subseq text (1+ at)) (nreverse parameters))))))

(defun map-unfolded-lines (function text)
  "Call FUNCTION with each line of TEXT, unfolded, and the number of the line
it begins on; empty lines left out. Lines may end in CR LF or LF alone."
  (let ((end (length text)) (at 0) (number 1)
        (line (make-string-output-stream)) (line-number 1) (has nil))
    (flet ((flush ()
             (let ((s (get-output-stream-string line)))
               (when (plusp (length s)) (funcall function s line-number)))
             (setf has nil)))
      (loop while (< at end)
            do (let* ((newline (or (position #\Newline text :start at) end))
                      (stop (if (and (> newline at) (char= (char text (1- newline)) #\Return)) (1- newline) newline)))
                 (if (and has (< at stop) (member (char text at) '(#\Space #\Tab)))
                     ;; a folded line goes on the one before
                     (write-string text line :start (1+ at) :end stop)
                     (progn (flush)
                            (setf line-number number has t)
                            (write-string text line :start at :end stop)))
                 (setf at (1+ newline))
                 (incf number)))
      (flush))))

(defun icalendar-read (text)
  "The components TEXT, a calendar file's text, holds, in order (usually one
VCALENDAR), as ICAL-COMPONENT structures. An ICALENDAR-ERROR when TEXT is
not a calendar file, holds more than *ICALENDAR-LARGEST* characters or
components deeper than *ICALENDAR-DEEPEST*."
  (when (> (length text) *icalendar-largest*)
    (error 'icalendar-error :text (format nil "larger than ~:d characters" *icalendar-largest*)))
  (let ((text (if (and (plusp (length text)) (char= (char text 0) (code-char #xFEFF))) (subseq text 1) text))
        (open '()) (top '()) (last-line 1))
    (map-unfolded-lines
     (lambda (line number)
       (setf last-line number)
       (let ((property (read-content-line line number)))
         (cond ((string= (ical-property-name property) "BEGIN")
                (when (>= (length open) *icalendar-deepest*)
                  (error 'icalendar-error :text (format nil "components deeper than ~d" *icalendar-deepest*) :line number))
                (push (make-ical-component (string-upcase (ical-property-value property))) open))
               ((string= (ical-property-name property) "END")
                (let ((component (pop open)))
                  (unless (and component (string-equal (ical-component-name component) (ical-property-value property)))
                    (error 'icalendar-error :text (format nil "END:~a closes ~:[nothing~;~:*~a~]" (ical-property-value property)
                                                          (and component (ical-component-name component)))
                                            :line number))
                  (setf (ical-component-properties component) (nreverse (ical-component-properties component))
                        (ical-component-components component) (nreverse (ical-component-components component)))
                  (if open
                      (push component (ical-component-components (first open)))
                      (push component top))))
               (open (push property (ical-component-properties (first open))))
               (t (error 'icalendar-error :text (format nil "~a outside any component" (ical-property-name property)) :line number)))))
     text)
    (when open
      (error 'icalendar-error :text (format nil "~a not ended" (ical-component-name (first open))) :line last-line))
    (nreverse top)))

;;; Writing

(defun parameter-value-text (value)
  "VALUE, a parameter's value, as written: ^ and line breaks and \" as RFC
6868 writes them, quoted when it holds ; : or ,."
  (let ((encoded (if (find-if (lambda (c) (find c (load-time-value (coerce (list #\^ #\Newline #\") 'string) t))) value)
                     (with-output-to-string (out)
                       (loop for c across value
                             do (case c (#\^ (write-string "^^" out)) (#\Newline (write-string "^n" out))
                                  (#\" (write-string "^'" out)) (t (write-char c out)))))
                     value)))
    (if (find-if (lambda (c) (find c ";:,")) encoded)
        (concatenate 'string "\"" encoded "\"")
        encoded)))

(defun char-octets (c)
  "How many octets C takes in UTF-8."
  (let ((code (char-code c)))
    (cond ((< code #x80) 1) ((< code #x800) 2) ((< code #x10000) 3) (t 4))))

(defun write-folded (line out)
  "Write LINE to OUT folded at 75 octets, never inside a character, each line ending CR LF."
  (let ((octets 0))
    (loop for c across line
          do (let ((size (char-octets c)))
               (when (> (+ octets size) 75)
                 (write-char #\Return out) (write-char #\Newline out) (write-char #\Space out)
                 (setf octets 1))
               (write-char c out)
               (incf octets size)))
    (write-char #\Return out) (write-char #\Newline out)))

(defun property-line (property)
  "PROPERTY as one unfolded line."
  (with-output-to-string (out)
    (write-string (ical-property-name property) out)
    (loop for (name . values) in (ical-property-parameters property)
          do (write-char #\; out) (write-string name out) (write-char #\= out)
             (loop for (value . more) on values
                   do (write-string (parameter-value-text value) out)
                      (when more (write-char #\, out))))
    (write-char #\: out)
    (write-string (ical-property-value property) out)))

(defun icalendar-write (components &optional stream)
  "COMPONENTS, a list of ICAL-COMPONENT structures or one, written as a
calendar file: to STREAM, or as a string when STREAM is NIL. Lines end CR LF
and are folded at 75 octets."
  (labels ((component (c out)
             (write-folded (concatenate 'string "BEGIN:" (ical-component-name c)) out)
             (dolist (p (ical-component-properties c)) (write-folded (property-line p) out))
             (dolist (inner (ical-component-components c)) (component inner out))
             (write-folded (concatenate 'string "END:" (ical-component-name c)) out))
           (all (out) (dolist (c (if (listp components) components (list components))) (component c out))))
    (if stream
        (all stream)
        (with-output-to-string (out) (all out)))))

(setf (documentation 'make-ical-property 'function) "A property of a component named NAME, its VALUE as written and its PARAMETERS, a list of (NAME . VALUES)."
      (documentation 'ical-property-p 'function) "True when the object given is an ICAL-PROPERTY."
      (documentation 'ical-property-name 'function) "A property's name in capitals (DTSTART)."
      (documentation 'ical-property-value 'function) "A property's value as written, escapes kept."
      (documentation 'ical-property-parameters 'function) "A property's parameters, a list of (NAME . VALUES), VALUES a list of strings."
      (documentation 'make-ical-component 'function) "A component named NAME (VEVENT) of PROPERTIES and the COMPONENTS within it."
      (documentation 'ical-component-p 'function) "True when the object given is an ICAL-COMPONENT."
      (documentation 'ical-component-name 'function) "A component's name in capitals (VCALENDAR, VEVENT)."
      (documentation 'ical-component-properties 'function) "A component's properties, in the order written."
      (documentation 'ical-component-components 'function) "The components within a component, in the order written.")
