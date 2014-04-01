(defpackage #:alice-debug
  (:use #:cl #:alice)
  (:nicknames #:aldbg)
  (:export #:dump-memos
           #:read-back-memos
           #:dump-hashtable
           #:read-back-into-hashtable
           #:get-background-handler-instance
           #:attach-standard-output-to-slime
           #:detach-standard-output-from-slime))

(in-package #:alice-debug)


(defun deconstruct-memo (memo)
  (print memo)
  (if (= 5 (length memo))
      (list (first memo)
            (second memo)
            (third memo)
            (fourth memo)
            (local-time:timestamp-to-unix (fifth memo)))
      memo))

(defun reconstruct-memo (memo-data)
  (list (first memo-data)
        (second memo-data)
        (third memo-data)
        (fourth memo-data)
        (local-time:unix-to-timestamp (fifth memo-data))))

(defun dump-memos (&key (source-table alice::*memos*) (destination-file "memos.se"))
  (dump-hashtable source-table destination-file :value-deconstructor #'deconstruct-memo))

(defun read-back-memos (&key (destination-table alice::*memos*) (source-file "memos.se"))
  (read-back-into-hashtable destination-table source-file :value-constructor #'reconstruct-memo))

(defun dump-hashtable (hashtable filename &key (value-deconstructor #'identity))
  "Serializes all `HASHTABLE' data into a file `FILENAME'. Such file can be read back by `READ-BACK-INTO-HASHTABLE'."
  (let ((entries '()))
    (maphash (lambda (k v) (push (cons k (mapcar value-deconstructor v)) entries)) hashtable)
    (with-open-file (out filename :if-exists :supersede :direction :output)
      (with-standard-io-syntax
        (print entries out)))
    entries))


(defun read-back-into-hashtable (hashtable filename &key (value-constructor #'identity))
  "Reads into `HASHTABLE' data from `FILENAME' that was dumped by `DUMP-HASHTABLE' and returns it value."
  (let ((data '()))
    (with-open-file (in filename)
      (with-standard-io-syntax
        (setf data (read in))))
    (mapc (lambda (entry) (setf (gethash (car entry) hashtable)
                                (mapcar value-constructor (cdr entry))))
          data)))

(defun get-background-handler-instance ()
  "Get instance of the (CCL) process of the message loop."
  #-openmcl (error "This function works only in Clozure CL (which I happen to use on my VPS)")
  #+openmcl (first (ccl:all-processes)))

(defun attach-standard-output-to-slime (handler-process)
  "Rebind `HANDLER-PROCESS''s `*STANDARD-OUTPUT' to the one of current context; effectively making it output into SLIME's REPL."
  #-openmcl (error "This function works only in Clozure CL (which I happen to use on my VPS)")
  #+openmcl (let ((so *standard-output*))
              (ccl:process-interrupt handler-process (lambda ()
                                                       (setf *backup-so* *standard-output*
                                                             *standard-output* so)))))

(defun detach-standard-output-from-slime (handler-process)
  "Reverse the effects of `ATTACH-STANDARD-OUTPUT-TO-SLIME', restoring the original value of `HANDLER-PROCESS''s `*STANDARD-OUTPUT*'."
  #-openmcl (error "This function works only in Clozure CL (which I happen to use on my VPS)")
  #+openmcl (ccl:process-interrupt handler-process (lambda ()
                                                     (setf *standard-output* *backup-so*))))
