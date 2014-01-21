(ql:quickload :cl-irc)

(defpackage :alice
  (:use :common-lisp
        :irc)
  (:export :start-alice
           :stop-alice
           :mute
           :unmute))

(defun run-test (nick pass)
  (let ((connection (irc:connect :nickname nick
                                 :server "irc.freenode.net")))
    (irc:read-message-loop connection)
    (irc:join connection "#trc")
    (irc:privmsg connection "#trc" "Ta Da!")))


(defun start-alice (server nick pass &rest channels)
  (setf *))

(defun stop-alice ()
  (format t "Not yet implemented.~%"))

(defun mute () )

(defun unmute () )
