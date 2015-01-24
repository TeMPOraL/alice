(in-package #:alice)

(defparameter *answers* '())

(defun provide-output (name list)
  "Register a new output definition `LIST' under the `NAME'."
  (setf *answers* (cons (cons name list) (remove-if (lambda (output) (eql (car output) name)) *answers*))))
