(in-package #:alice)

(defvar *channels* '())

(defclass channel ()
  ((name :initarg :name
         :type string
         :accessor name
         :documentation "Channel name, eg. #foo.")

   (last-messages :accessor last-messages
                  :initform '()
                  :documentation "A few last messages sent on that channel.")

   (last-URLs :accessor last-URLs
              :initform '()
              :documentation "Last URLs sent on that channel.")

   (throttle :accessor channel-throttle ;FIXME rename to just `THROTTLE' after full refactor
             :initform '()
             :documentation "Channel-local throttle (i.e. additional output).")))

(defun ensure-get-channel (name)
  "Get channel object named `NAME', creating one if it does not exist."
  (let ((channel (find-if (lambda (ch) (string= (name channel) name)) *channels*)))
    (unless channel
      (setf channel (make-instance 'channel :name name))
      (push channel *channels*))
    channel))
