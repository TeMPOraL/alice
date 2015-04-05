(in-package #:alice)

(defparameter *pushover-token* "")
(defparameter *pushover-admin-user* "")

(defun send-pushover (what to-token from)
  (if (ignore-errors (drakma:http-request "https://api.pushover.net/1/messages.json"
                                          :method :post
                                          :external-format-out :UTF-8
                                          :parameters `(("token" . ,*pushover-token*)
                                                        ("user" . ,to-token)
                                                        ("title" . ,*full-name*)
                                                        ("message" . ,(concatenate 'string "<" from "> " what)))
                                          :content "hack"
                                          :content-length 4))
      :notification-sent
      :failed-in-sending-notification))

;;; FIXME maybe move output message determination somewhere else

;;; notifications plug-in
(defun make-pushover-notifier (pushover-key)
  (lambda (memo)
    (send-pushover (text memo) pushover-key (author memo))))
