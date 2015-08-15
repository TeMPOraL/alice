;; persistence primitives
(in-package #:alice)

(defmethod marshal:class-persistant-slots ((timestamp local-time:timestamp))
  (mapcar #'closer-mop:slot-definition-name (closer-mop:class-direct-slots (class-of timestamp))))

(defun dump-list (list filename)
  "Serializes entire `LIST' into `FILENAME'."
  (with-open-file (out filename :if-exists :supersede :direction :output)
    (with-standard-io-syntax
      (print (ms:marshal list) out))))

(defun read-back-list (filename)
  "Read back list from file named `FILENAME'."
  (when (probe-file filename)
   (with-open-file (in filename)
     (with-standard-io-syntax
       (ms:unmarshal (read in))))))

(defun serialize-hashtable-to-file (hashtable filename)
  "Serializes all `HASHTABLE' data into a file `FILENAME' using cl-marshal. Read byc using `DESERIALIZE-HASHTABLE-FROM-FILE'."
  (with-open-file (out filename :if-exists :supersede :direction :output)
    (print (ms:marshal hashtable) out)))

(defun deserialize-hashtable-from-file (filename)
  "Reads back and deserializes hashtable from `FILENAME' saved by `SERIALIZE-HASHTABLE-TO-FILE'."
  (when (probe-file filename)
    (with-open-file (in filename)
      (ms:unmarshal (read in)))))

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
    (when (probe-file filename)
      (with-open-file (in filename)
        (with-standard-io-syntax
          (setf data (read in)))))
    (mapc (lambda (entry) (setf (gethash (car entry) hashtable)
                           (cdr entry)))
          data)))
