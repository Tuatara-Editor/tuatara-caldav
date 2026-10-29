;;;; test/run.lisp --- Command-line test runner: sbcl --non-interactive --load test/run.lisp
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Loads Tuatara and its tests from the source tree $TUATARA_SOURCE names
;;;; (tools/test.sh sets it), then this repository's package, compiled as
;;;; load-package compiles it, and its tests (test.lisp). The package is
;;;; checked to use only Tuatara's API and the library names it allows
;;;; (OUTSIDE-API), and refused by Tuatara when it uses a name Tuatara lacks.
;;;; Exits 0 when the package loads, keeps to the API and passes its tests, 1
;;;; otherwise.

(defvar *package-name* "caldav"
  "The package this repository holds: its entry is NAME.lisp at the top.")

(defvar *package-root* (merge-pathnames "../" (make-pathname :name nil :type nil :defaults *load-truename*))
  "The repository's top folder, where the package's files are.")

(defvar *tuatara-source*
  (let ((given (sb-ext:posix-getenv "TUATARA_SOURCE")))
    (if (and given (plusp (length given)))
        (pathname (concatenate 'string (string-right-trim "/" given) "/"))
        (or (find-if #'probe-file (list (merge-pathnames "../tuatara/" *package-root*)
                                        (merge-pathnames "../../tuatara/" *package-root*)))
            (error "No Tuatara source tree beside this repository: set TUATARA_SOURCE"))))
  "The Tuatara source tree the tests run against: $TUATARA_SOURCE, else
tuatara beside this repository or beside the folder it is in.")

(load (merge-pathnames "tools/quicklisp.lisp" *tuatara-source*))
(push *tuatara-source* asdf:*central-registry*)
(ql:quickload :tuatara/tests :silent t)

(defvar *package-names* (list *package-name*)
  "The packages the runner loads and tests: this repository's one.")

(defvar *refused* '()
  "The packages Tuatara refused to load, each (NAME . WHY): one uses a name
this Tuatara lacks.")

(dolist (name *package-names*)
  (let ((folder *package-root*))
    ;; compiled afresh, as load-package compiles a package fetched anew, its names checked
    (dolist (file (uiop:directory-files folder "*.lisp"))
      (uiop:delete-file-if-exists (tuatara.defaults::package-fasl file)))
    (handler-case
        (let ((tuatara.defaults::*package-folder* folder)
              (tuatara.defaults::*loading-package-name* name))
          (tuatara-user:load-package-file (concatenate 'string name ".lisp"))
          (let ((tests (merge-pathnames "test.lisp" folder)))
            (when (probe-file tests) (load tests))))
      (tuatara.defaults::package-lacks-names (refusal)
        (push (cons name (princ-to-string refusal)) *refused*)))))

(defun tuatara-name-p (symbol)
  "True when SYMBOL is one of Tuatara's own, of a package named TUATARA...."
  (let ((home (symbol-package symbol)))
    (and home (uiop:string-prefix-p "TUATARA" (package-name home)))))

(defun outside-library-p (symbol own)
  "True when SYMBOL is of a library: a package that is not OWN, the
package's own, nor Common Lisp's, the keywords' or Tuatara's, and SYMBOL is
not one of the library names Tuatara lets packages use (*API-FROM-LIBRARIES*)."
  (let ((home (symbol-package symbol)))
    (and home
         (not (member home (list own (find-package :cl) (find-package :keyword))))
         (not (tuatara-name-p symbol))
         (not (eq symbol 'sb-int:quasiquote))
         (not (member symbol tuatara.defaults::*api-from-libraries*)))))

(defun public-p (symbol)
  "True when a package may use SYMBOL: exported from TUATARA, or a setting
TUATARA-USER exports."
  (some (lambda (package) (eq (nth-value 1 (find-symbol (symbol-name symbol) package)) :external))
        '(:tuatara :tuatara-user)))

(defun defined-p (symbol)
  "True when SYMBOL names a function, a variable or a class."
  (or (fboundp symbol) (boundp symbol) (find-class symbol nil)))

(defun tuatara-defines-p (symbol)
  "True when Tuatara's defaults define a function, variable or class of
SYMBOL's name."
  (let ((tuatara (find-symbol (symbol-name symbol) :tuatara.defaults)))
    (and tuatara (defined-p tuatara))))

(defvar *tuatara-keys*
  (let ((keys (make-hash-table :test 'equal)))
    (dolist (folder '("src/" "lisp/") keys)
      (dolist (file (directory (merge-pathnames (concatenate 'string folder "**/*.lisp") *tuatara-source*)))
        (cl-ppcre:do-register-groups (name) ("buffer-local [^()']*'([a-z0-9*+%-]+)" (uiop:read-file-string file))
          (setf (gethash (string-upcase name) keys) t)))))
  "The names Tuatara keeps buffers' values under, as its source writes them:
(buffer-local BUFFER 'NAME).")

(defvar *tuatara-keywords*
  (let ((keys (make-hash-table :test 'equal)))
    (dolist (folder '("src/" "lisp/") keys)
      (dolist (file (directory (merge-pathnames (concatenate 'string folder "**/*.lisp") *tuatara-source*)))
        (cl-ppcre:do-register-groups (name) ("buffer-local\\s+(?:[^\\s()']+|\\([^()]*\\))\\s+:([a-z0-9*+%-]+)" (uiop:read-file-string file))
          (setf (gethash (string-upcase name) keys) t)))))
  "The keywords Tuatara keeps buffers' values under, as its source writes
them: (buffer-local BUFFER :NAME).")

(defun shared-key-p (symbol)
  "True when Tuatara keeps a buffer's value under SYMBOL's name: a keyword
of *TUATARA-KEYWORDS*, another symbol of *TUATARA-KEYS*."
  (gethash (symbol-name symbol) (if (keywordp symbol) *tuatara-keywords* *tuatara-keys*)))

(defun buffer-keyword (form)
  "The keyword FORM keeps a buffer's value under, (buffer-local BUFFER :NAME); else NIL."
  (and (eq (car form) 'tuatara.buffer:buffer-local) (consp (cdr form)) (consp (cddr form))
       (keywordp (third form)) (third form)))

(defun named-symbol (form)
  "The symbol FORM names as data, 'NAME, or as the key of (buffer-local
BUFFER 'NAME); else NIL."
  (cond ((and (eq (car form) 'quote) (consp (cdr form)) (symbolp (second form))) (second form))
        ((and (eq (car form) 'tuatara.buffer:buffer-local) (consp (cdr form)) (consp (cddr form))
              (consp (third form)) (eq (car (third form)) 'quote) (symbolp (second (third form))))
         (second (third form)))))

(defun outside-api (name)
  "The names the package NAME uses that are not Tuatara's API, written with
Tuatara's package in front (read from its files but test.lisp), each with how;
one called or read though defined nowhere is refused as it loads (*REFUSED*)."
  (let ((own (find-package (string-upcase name)))
        (found '()))
    (dolist (file (uiop:directory-files *package-root* "*.lisp"))
      (unless (string= (pathname-name file) "test")
        (with-open-file (in file)
          (let ((*package* own) (*read-eval* t))
            (loop for form = (read in nil in) until (eq form in)
                  do (when (and (consp form) (eq (car form) 'in-package)) (setf *package* (find-package (second form))))
                  (labels ((note (format symbol)
                             (pushnew (format nil format symbol) found :test #'string=))
                           (walk (x)
                             (cond ((and (symbolp x) (tuatara-name-p x) (not (public-p x)))
                                    (note "~(~s~), written with its package" x))
                                   ((and (symbolp x) (outside-library-p x own))
                                    (note "~(~s~), a library's name Tuatara does not let packages use" x))
                                   ((consp x)
                                    (let ((key (buffer-keyword x)))
                                      (when (and key (shared-key-p key))
                                        (note "~(~s~), a buffer's value kept by Tuatara under that key, not in its API" key)))
                                    (let ((named (named-symbol x)))
                                      (when (and named (eq (symbol-package named) own))
                                        (cond ((and (eq (car x) 'quote) (tuatara-defines-p named) (not (defined-p named)))
                                               (note "~(~a~), Tuatara's own named as data, not in its API" named))
                                              ((and (consp (cdr x)) (eq (car x) 'tuatara.buffer:buffer-local) (shared-key-p named))
                                               (note "~(~a~), a buffer's value kept by Tuatara under that name, not in its API" named)))))
                                    (walk (car x)) (walk (cdr x))))))
                    (walk form)))))))
    (sort found #'string<)))

(let ((failed nil))
  (loop for (name . why) in (reverse *refused*)
        do (format t "~&~a is refused: ~a~%" name why)
           (setf failed t))
  (dolist (name (set-difference *package-names* (mapcar #'car *refused*) :test #'string=))
    (dolist (use (outside-api name))
      (format t "~&~a uses ~a~%" name use)
      (setf failed t)))
  (dolist (name *package-names*)
    (let ((test (find-symbol (string-upcase name) :tuatara.tests)))
      (when test
        (unless (eql (parachute:status (parachute:test test :report 'parachute:plain)) :passed)
          (setf failed t)))))
  (tuatara.tests::remove-test-directories)
  (sb-ext:exit :code (if failed 1 0)))
