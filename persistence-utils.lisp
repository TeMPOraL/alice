;; persistence primitives
(in-package #:alice)

(defun dump-hashtable (hashtable filename)
  "Serializes all `HASHTABLE' data into a file `FILENAME'. Such file can be read back by `READ-BACK-INTO-HASHTABLE'."
  (let ((entries '()))
    (maphash (lambda (k v) (push (cons k v) entries)) hashtable)
    (with-open-file (out filename :if-exists :supersede :direction :output)
      (with-standard-io-syntax
        (print entries out)))
    entries))

(defun read-back-into-hashtable (hashtable filename)
  "Reads into `HASHTABLE' data from `FILENAME' that was dumped by `DUMP-HASHTABLE' and returns it value."
  (let ((data '()))
    (with-open-file (in filename)
      (with-standard-io-syntax
        (setf datpa (read in))))
    (mapc (lambda (entry) (setf (gethash (car entry) hashtable)
                                (cdr entry)))
          data)))
