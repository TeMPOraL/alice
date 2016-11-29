(in-package #:alice-tests)

(defparameter *suites* '())

(defun run-all ()
  (mapc 'fiveam:run! *suites*)
  (format t "DONE!~%"))

(defun add-suite (suite)
  (setf *suites* (remove-duplicates (append (list suite) *suites*))))
