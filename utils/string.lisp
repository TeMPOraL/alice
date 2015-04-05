(in-package #:alice)

(defun fix-drakma-output (output)
  "Takes `OUTPUT' from drakma:http-request and coerces it into a string."
  (if (stringp output)
      output
      (map 'string #'code-char output)))

(defun limit-string-length (string max-length &optional (ending ""))
  "Makes sure `STRING' (with `ENDING' appended) is no longer than `MAX-LENGTH'."
  (let ((current-length (length string))
        (max-length (- max-length (length ending))))
    (if (>= max-length 0)
        (if (> current-length max-length)
            (concatenate 'string
                         (subseq string 0 max-length)
                         ending)
            string)
        "")))

(defun implode (list &optional (delimiter #\Space))
  "Concatenates `LIST' into a single string, with elements separated by `DELIMITER', which can be basically anything."
  (format nil
          (concatenate 'string
                       "~{~A~^"
                       (format nil "~A" delimiter)
                       "~}")
          list))
