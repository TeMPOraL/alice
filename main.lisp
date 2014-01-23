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



;; phrases

(defvar *msg-introduction* "Alice Margatroid, do usług.")
(defvar *friendly-smiles-list* '(":)" ":)" ":)" ":)" ":)" ":)" ":)" ":)" ":)" ":)" ; yeah, a cheap trick to fake probability distribution
                                 ";)" ";)" ";)"";)" ";)" ";)"
                                 ":P" ":P" ":P" ":P" ":P"
                                 ":>" ":>" ":>"
                                 "ta da!"
                                 "maka paka!"))


(defun get-random-friendly-smile ()
  (elt *friendly-smiles-list*
       (random (length *friendly-smiles-list*))))


;;; speech related

;; types of message
(defun public-message-p (message)
  (and
   (not (string-equal *nick* (first (arguments message)))) ; search message
   (not (equal 0
               (search *nick* (second (arguments message))))))) ; search message target
       

(defun private-message-p (message)
  (or (string-equal (first (arguments message))
                    *nick*)
      (equal 0 (search *nick* (second (arguments message))))))

(defun directed-message-p (message)
  (or (string-equal (first (arguments message))
                    *nick*)
      (mentions-name *nick* (second (arguments message)))))

;;; utils

(defun concat-strings (list)
  (format nil "~{~a, ~}" list))

(defun mentions (what string)
  (search what string))

(defun mentions-name (name string)
  (mentions name string))

;;; handling

(defun msg-hook (message)
  (let ((destination (if (string-equal (first (arguments message)) *nick*)
                         (source message)
                         (first (arguments message)))))
    ;; TODO match commands, questions, etc.

    (let ((is-private (private-message-p message))
          (is-public (public-message-p message))
          (is-directed (directed-message-p message)))

      (cond ((and is-directed
                  (mentions "przedstaw sie" (second (arguments message))))
             (privmsg *connection* destination *msg-introduction*))


            ((and is-public
                  (search "fail" (second (arguments message)))
                  (/= 0 (random 3)))
             (privmsg *connection* destination "..."))


            ;; default responder
            (is-directed
             (if (/= 0 (random 5))
                 (privmsg *connection* destination (concatenate 'string (source message) " " (get-random-friendly-smile)))))))))




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
