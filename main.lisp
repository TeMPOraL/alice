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

;;; utils

(defun concat-strings (list)
  (format nil "~{~a, ~}" list))

(defun mentions (what string)
  (search what string))

(defun mentions-name (name string)
  (mentions name string))

;;; handling
(defun private-message-p (message)
  (or (string-equal (first (arguments message))
                    *nick*)
      (mentions-name *nick* (second (arguments message)))))

(defun msg-hook (message)
  (let ((destination (if (string-equal (first (arguments message)) *nick*)
                         (source message)
                         (first (arguments message)))))
    ;; TODO match commands, questions, etc.

    ;; default autoresponder
    (if (private-message-p message)
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
