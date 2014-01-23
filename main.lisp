(ql:quickload :cl-irc)

(defpackage :alice
  (:use :common-lisp
        :irc)
  (:export :start-alice
           :stop-alice
           :mute
           :unmute))

(in-package :alice)

(defvar *connection*)
(defvar *nick* "")
(defvar *connected-channels*)

(defconstant +nickserv+ "NickServ")
(defconstant +nickserv-identify-msg-template+ "IDENTIFY ~a")

(defvar *msg-introduction* "Alice Margatroid, do usług.")


;;; speech related
(defun public-message-p (message)
  (and
   (not (string-equal *nick* (first (arguments message)))) ; search message
   (not (equal 0
               (search *nick* (second (arguments message))))))) ; search message target
       

(defun private-message-p (message)
  (or (string-equal (first (arguments message))
                    *nick*)
      (equal 0 (search *nick* (second (arguments message))))))

;;; utils

(defun concat-strings (list)
  (format nil "~{~a, ~}" list))

(defun mentions (what string)
  (search what string))

(defun mentions-name (name string)
  (mentions name string))

;;; handling
(defun directed-message-p (message)
  (or (string-equal (first (arguments message))
                    *nick*)
      (mentions-name *nick* (second (arguments message)))))

(defun msg-hook (message)
  (let ((destination (if (string-equal (first (arguments message)) *nick*)
                         (source message)
                         (first (arguments message)))))
    ;; TODO match commands, questions, etc.

    (if (and (search "fail" (second (arguments message)))
             (= 0 (random 5)))
        (privmsg *connection* destination "..."))

    (if (public-message-p message)
        (privmsg *connection* destination "public!"))

    (if (private-message-p message)
        (privmsg *connection* destination "private!"))


    ;; default autoresponder
    (if (directed-message-p message)
        (cond
          ((mentions "przedstaw sie" (second (arguments message))) (privmsg *connection* destination *msg-introduction*))
          (t (privmsg *connection* destination (concatenate 'string (source message) " :P")))))))


(defun start-alice (server nick pass &rest channels)
  (setf *nick* nick)
  (setf *connection* (connect :nickname *nick*
                              :server server))
  (setf *connected-channels* channels)

  (privmsg *connection* +nickserv+ (format nil +nickserv-identify-msg-template+ pass))

  (mapcar (lambda (channel) (join *connection* channel)) channels)

  (add-hook *connection* 'irc::irc-privmsg-message 'msg-hook)

  #+(or sbcl
        openmcl)
  (start-background-message-handler *connection*))

(defun stop-alice (&optional (msg "Goodbye!"))
      (quit *connection* msg))

(defun mute ()
  ;; TODO
  )

(defun unmute ()
  ;; TODO
  )

(defun impersonate-say (destination what)
  (privmsg *connection* destination what))
