;;;; test/run.lisp --- Command-line test runner: sbcl --non-interactive --load test/run.lisp
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Loads Tuatara and its tests from the source tree $TUATARA_SOURCE names
;;;; (tools/test.sh sets it), then has Tuatara test this repository's package
;;;; (TUATARA.TESTS:TEST-PACKAGE): compiled as load-package compiles it,
;;;; checked to use only Tuatara's API and the library names it allows, and
;;;; its tests (test.lisp) run. Exits 0 when the package loads, keeps to the
;;;; API and passes its tests, 1 otherwise.

(defvar *package-name* "caldav"
  "The package this repository holds: its entry is NAME.lisp at the top.")

(defvar *package-root* (merge-pathnames "../" (make-pathname :name nil :type nil :defaults *load-truename*))
  "The repository's top directory, where the package's files are.")

(defvar *tuatara-source*
  (let ((given (sb-ext:posix-getenv "TUATARA_SOURCE")))
    (if (and given (plusp (length given)))
        (pathname (concatenate 'string (string-right-trim "/" given) "/"))
        (or (find-if #'probe-file (list (merge-pathnames "../tuatara/" *package-root*)
                                        (merge-pathnames "../../tuatara/" *package-root*)))
            (error "No Tuatara source tree beside this repository: set TUATARA_SOURCE"))))
  "The Tuatara source tree the tests run against: $TUATARA_SOURCE, else
tuatara beside this repository or beside the directory it is in.")

(load (merge-pathnames "tools/quicklisp.lisp" *tuatara-source*))
(push *tuatara-source* asdf:*central-registry*)
(ql:quickload :tuatara/tests :silent t)

(sb-ext:exit :code (tuatara.tests:test-package *package-name* *package-root*))
