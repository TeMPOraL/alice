(in-package #:alice)

(defparameter *mailgun-domain* "")
(defparameter *mailgun-key* "")

(defun send-email (where-to text)
  (if (ignore-errors (drakma:http-request (concatenate 'string "https://api.mailgun.net/v2/" *mailgun-domain* "/messages")
                                          :method :post
                                          :basic-authorization `("api" ,*mailgun-key*)
                                          :parameters `(("from" . ,(concatenate 'string "Alice Margatroid <alice.margatroid@" *mailgun-domain* ">"))
                                                        ("to" . ,where-to)
                                                        ("subject" . "Alice Margatroid here; got a notification for you.")
                                                        ("text" . ,text))
                                          :external-format-out :UTF-8))
      ;; 
      :notification-sent
      :failed-in-sending-notification))

;;; FIXME maybe move output message determination somewhere elsew

;;; notifications plug-in
(defun make-email-notifier (email)
  (lambda (channel who what from-who is-private)
    (declare (ignore channel who from-who is-private))
    (send-email email what)))
