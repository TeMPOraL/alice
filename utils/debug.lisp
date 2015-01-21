(defpackage #:alice-debug
  (:use #:cl #:alice)
  (:nicknames #:aldbg)
  (:export #:get-background-handler-instance
           #:attach-standard-output-to-slime
           #:detach-standard-output-from-slime
           #:restart-IRC-thread
           #:start-swank
           #:stop-swank))

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

;; (defun dump-memos (&key (source-table alice::*memos*) (destination-file "memos.se"))
;;   (dump-hashtable source-table destination-file :value-deconstructor #'deconstruct-memo))

;; (defun read-back-memos (&key (destination-table alice::*memos*) (source-file "memos.se"))
;;   (read-back-into-hashtable destination-table source-file :value-constructor #'reconstruct-memo))

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

(defun restart-IRC-thread ()
  "Restart IRC connection after crash."
  (irc:start-background-message-handler alice::*connection*))

(defun start-swank (&optional (port 4005))
  "Start SWANK listening on a provided `PORT'."
  (unless swank::*servers*              ;FIXME a bit hackish.
    (swank:create-server :port port :dont-close t)))

(defun stop-swank (&optional (port 4005))
  "Stop SWANK listening on a provided `PORT'."
  (swank:stop-server port))
