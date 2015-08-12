(in-package #:alice)
;;; "alice" goes here. Hacks and glory await! ;-)

(defparameter *full-name* "Alice Margatroid")  ;FIXME not exactly sure where it belongs...

(defvar *connection*)

;; configurables
(defparameter *server* "")
(defvar *nick* "")
(defparameter *password* "")


(defparameter *autojoin-channels* '())

(defparameter *muted* nil)

(defparameter +nickserv+ "NickServ")
(defparameter +nickserv-identify-msg-template+ "IDENTIFY ~a")


(defparameter *debug-last-message* nil)

(defparameter *excluded-from-replying-to* '() "List of users that the bot won't reply to for unrecognized queries.")

(defparameter *throttled-output* nil "A buffer for throttling the output to avoid flooding the channel.")

(defparameter *max-output-sequence-length* 4)
(define-constant +maximum-line-length+ 400)

(defparameter *default-phrase* "Nie wiem co powiedzieć...")


;; functions
;; tools
(defun throttle (messages)
  (let ((len (length messages)))
    (if (> len *max-output-sequence-length*)
        (let* ((split-point (min *max-output-sequence-length*
                                 len))
               (to-say (subseq messages 0 split-point))
               (to-buffer (subseq messages split-point)))
          (setf *throttled-output* (and (> (length to-buffer) 0) to-buffer))
          (concatenate 'vector to-say #(:throttled-message)))
        (progn
          (setf *throttled-output* nil)
          messages))))

(defun say (to-where what &key to)
  (unless *muted*
    (typecase what
      (null t)

      (keyword (say to-where (cdr (assoc what *answers*)) :to to))

      (list (say to-where
                 (random-elt what)
                 :to to))

      (string
       (if (null to)
           (irc:privmsg *connection* to-where what)
           (irc:privmsg *connection* to-where (concatenate 'string to ": " what))))

      (vector
       (let ((tosay (throttle what)))
         (map 'nil
              (lambda (msg)
                (say to-where msg :to to))
              tosay)))

      (t (irc:privmsg *connection* to-where *default-phrase*)))))

;;; handling

(defun msg-hook (message)
    (let ((destination (if (string-equal (first (irc:arguments message)) *nick*)
                         (irc:source message)
                         (first (irc:arguments message))))
          (is-private (private-message-p message))
          (is-public (public-message-p message))
          (is-directed (directed-message-p message))
          (from-who (irc:source message))
          (message-body (second (irc:arguments message))))

      (check-for-memos destination from-who)

      (handle-specials destination is-private is-public is-directed from-who message-body)

      (execute-match message)

      (setf *debug-last-message* (extract-message-features message))))

;; those hooks handle world state
(defun join-hook (message)
  (let ((who (irc:source message))
        (where (first (irc:arguments message))))
    (store-joining-name where who)))

(defun part-hook (message)
  (let ((who (irc:source message))
        (where (first (irc:arguments message))))
    (when (string= who "kdbot")
      (say where :kdbot-down)
      (notify-kdbot-down))
    (store-parting-name where who)))

(defun names-hook (message)
  (let ((channel (third (irc:arguments message)))
        (nicks (fourth (irc:arguments message))))
    (store-names channel (split-sequence:split-sequence #\Space nicks))))

(defun nick-hook (message)
  (let ((from (irc:source message))
        (to (first (irc:arguments message))))
    (register-nick-change from to)))

(defun invite-hook (message)
  (let ((where (second (irc:arguments message))))
    (join-channel where)))

;; entry point

(defun start-alice (&key (server *server*) (nick *nick*) (password *password*) (channels *autojoin-channels*))
  (clear-nonpersistent-worldstate)
  (load-persistent-world-model-data)

  (setf *nick* nick)
  (setf *connection* (irc:connect :nickname *nick*
                                  :server server))

  (irc:privmsg *connection* +nickserv+ (format nil +nickserv-identify-msg-template+ password))

  (mapcar (lambda (channel) (join-channel channel)) channels)

  (irc:add-hook *connection* 'irc:irc-privmsg-message 'msg-hook)
  (irc:add-hook *connection* 'irc:irc-join-message 'join-hook)
  (irc:add-hook *connection* 'irc:irc-part-message 'part-hook)
  (irc:add-hook *connection* 'irc:irc-rpl_namreply-message 'names-hook)
  (irc:add-hook *connection* 'irc:irc-nick-message 'nick-hook)
  (irc:add-hook *connection* 'irc:irc-invite-message 'invite-hook)

  #+(or sbcl
        openmcl)
  (irc:start-background-message-handler *connection*)

  (start-delayed-notification-timer))

(defun stop-alice (&optional (msg "Goodbye!"))
      (irc:quit *connection* msg))

(defun mute ()
  (setf *muted* t))

(defun unmute ()
  (setf *muted* nil))

;; impersonate function

(defun impersonate-say (destination what)
  (irc:privmsg *connection* destination what))

(defun impersonate-join (channel &key password)
  (join-channel channel :password password))

(defun impersonate-part (channel)
  (part-channel channel))

(defun impersonate-slap (channel user)
  (irc::action alice::*connection* channel (concatenate 'string "slaps " user " with a Shanghai doll.")))
