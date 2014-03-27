(defpackage #:alice-debug
  (:use #:cl #:alice)
  (:nicknames #:aldbg)
  (:export #:dump-hashtable
           #:read-back-into-hashtable
           #:get-background-handler-instance
           #:attach-standard-output-to-slime
           #:detach-standard-output-from-slime))

(in-package #:alice-debug)

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
        (setf data (read in))))
    (mapc (lambda (entry) (setf (gethash (car entry) hashtable)
                                (cdr entry)))
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
