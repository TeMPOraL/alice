(in-package #:alice)

(defvar *event-queue-lock* (bordeaux-threads:make-lock))
(defvar *event-queue* '())

(defvar *event-loop-timer* (trivial-timers:make-timer (lambda () (funcall 'process-event-queue))))

(defun process-event-queue ()
  "Process event in global event queue."
  (let ((to-execute '()))
    (bordeaux-threads:with-lock-held (*event-queue-lock*)
      (setf *event-queue* (remove-if #'null (mapcar (lambda (event)
                                                      (if (funcall (car event))
                                                          (progn (push (cdr event) to-execute)
                                                                 nil)
                                                          event))
                                                    *event-queue*))))
    (mapc #'funcall to-execute)))

(defun add-event (condition action)
  "Register an event made out of `CONDITION' and `ACTION' for single execution."
  (bordeaux-threads:with-lock-held (*event-queue-lock*)
    (push (cons condition action)  *event-queue*)))

(defun run-event-loop ()
  (unless (trivial-timers:timer-scheduled-p *event-loop-timer*)
    (trivial-timers:schedule-timer *event-loop-timer* 0 :repeat-interval 1)))

(defun stop-event-loop ()
  (when (trivial-timers:timer-scheduled-p *event-loop-timer*)
    (trivial-timers:unschedule-timer *event-loop-timer*)))

;;; https://trac.common-lisp.net/bordeaux-threads/wiki/ApiDocumentation
;;; #'condition-wait and #'condition-notify could be used to sync the threads on event loop.

;;; also, consider moving timer for delayed actions into a separate
;;; unit with separate thread, that feeds events back to the primary event loop.

;;; utility event condition functions

(defun milisec->nanosec (value)
  (* value 1000000))

(defun run-asap ()
  (lambda () t))

(defun run-after-delay-msec (delay)
  (run-after-time (local-time:adjust-timestamp (local-time:now)
                    (:offset :nsec (milisec->nanosec delay)))))

(defun run-after-time (time)
  (lambda ()
    (local-time:timestamp>= (local-time:now) time)))
