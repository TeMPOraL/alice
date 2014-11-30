(in-package #:alice)

(defun fix-drakma-output (output)
  "Takes `OUTPUT' from drakma:http-request and coerces it into a string."
  (if (stringp output)
      output
      (map 'string #'code-char output)))
