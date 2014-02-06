(in-package #:alice)
;;; "alice" goes here. Hacks and glory await! ;-)

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
  (if (not *muted*)
      (cond ((null what)
             t)

            ((keywordp what)
             (say to-where (cdr (assoc what *answers*)) :to to))
            
            ((listp what)
             (say to-where
                  (elt what
                       (random (length what)))
                  :to to))

            ((stringp what)
             (if (null to)
                 (irc:privmsg *connection* to-where what)
                 (irc:privmsg *connection* to-where (concatenate 'string to ": " what))))

            ((vectorp what)
             (let ((tosay (throttle what)))
               (map 'nil
                    (lambda (msg)
                      (say to-where msg :to to))
                    tosay)))

            (t (irc:privmsg *connection* to-where "I just don't know what to say...")))))

;;; utils
(defun mentions (what string)
  (search (string-downcase what) (string-downcase string)))

(defun mentions-name (name string)
  (mentions name string))

;; types of message
(defun public-message-p (message)
  (and
   (not (string-equal *nick* (first (irc:arguments message)))) ; search message
   (not (equal 0
               (search *nick* (second (irc:arguments message))))))) ; search message target
       

(defun private-message-p (message)
  (or (string-equal (first (irc:arguments message))
                    *nick*)
      (equal 0 (search *nick* (second (irc:arguments message))))))

(defun directed-message-p (message)
  (or (string-equal (first (irc:arguments message))
                    *nick*)
      (mentions-name *nick* (second (irc:arguments message)))))

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
      (declare (ignore is-private))
      (cond
        ;; introductions
        ((and is-directed
              (or (mentions "poznaj" message-body)
                  (mentions "przedstaw się" message-body)
                  (mentions "przedstaw sie" message-body)
                  (mentions "przedstawisz"  message-body)))

         (say destination :introduction))

        ;; version number
        ((and is-directed
              (or (mentions "numer wersji" message-body)
                  (mentions "wersje" message-body)
                  (mentions "wersja" message-body)
                  (mentions "wersją" message-body)
                  (mentions "wersję" message-body)))
         (say destination :version))

        ;; be nice to thanks
        ((and is-directed
              (or (mentions "thx" message-body)
                  (mentions "thanks" message-body)
                  (mentions "thank you" message-body)
                  (mentions "dzieki" message-body)
                  (mentions "dzięki" message-body)
                  (mentions "dziekuje" message-body)
                  (mentions "dziękuje" message-body)
                  (mentions "dziękuję" message-body)))
         (say destination :thanks-reply))

        ;; temp check
        ((and is-directed
              (or (mentions "temperatur" message-body)
                  (mentions "zimno" message-body)
                  (mentions "cieplo" message-body)
                  (mentions "ciepło" message-body)))
         (say destination :temperature))


        ;; save
        ((and is-directed
              (or (mentions "pisz" message-body)
                  (mentions "notuj" message-body)))
         (say destination :save))
                  

        ;; anyone in HS?
        ((and is-directed
              (mentions "kto" message-body)
              (mentions "jest w HS" message-body))
         (say destination :who-in-hs))

        ;; sing
        ((and is-directed
              (or (mentions "spiew" message-body)
                  (mentions "śpiew" message-body)))
         (say destination :songs))

        ;; TCP handshake for Bambucha
        ((and is-directed
              (mentions "SYN" message-body))
         (say destination :tcp :to from-who))

        ;; URL advanced shortener (not yet implemented)
        ((and is-directed
              (and (or (mentions "skró" message-body)
                       (mentions "skracaj" message-body))
                   (or (mentions "poprzedni" message-body)
                       (mentions "ostatni" message-body))))
         (say destination :not-yet-implemented))

        ;; URL shortener
        ((and is-directed
              (or (mentions "skró" message-body)
                  (mentions "skracaj" message-body)))
         (say destination (shorten-url (parse-message-for-url-shortening message-body))))

        ;; Wolfram|Alpha
        ((and is-directed
              (or (mentions "licz" message-body)
                  (mentions "compute" message-body)))
         (say destination (do-wolfram-computation (parse-message-for-wolfram-computation message-body))))


        ;; continue throttled output
        ((and is-directed
              (or (mentions "tak" message-body)
                  (mentions "yes" message-body)
                  (mentions "dawaj" message-body)
                  (mentions "pros" message-body))
              (not (null *throttled-output*)))
         (say destination *throttled-output*))


        ;; ping temporal
        ((and is-directed
              (and (or (mentions "TeMPOraL" message-body)
                       (mentions "temporal" message-body))
                   (or (mentions "zawiadom" message-body)
                       (mentions "powiadom" message-body)
                       (mentions "przeka" message-body)
                       (mentions "pingnij" message-body))))
         (progn (say destination :notification-sent)
                (send-notification message-body from-who)))

        ((and is-directed
              (and (or (mentions "Wiktor" message-body)
                       (mentions "wiktor" message-body))
                   (or (mentions "zawiadom" message-body)
                       (mentions "powiadom" message-body)
                       (mentions "przeka" message-body)
                       (mentions "pingnij" message-body))))
         (progn (say destination :notification-sent)
                (send-email *wiktor-email* (concatenate 'string "<" from-who "> - " message-body))))

        ;; say hi!
        ((and is-directed
              (or (mentions "czesc" message-body)
                  (mentions "cześć" message-body)
                  (mentions "hi" message-body)
                  (mentions "hej" message-body)
                  (mentions "hey" message-body)
                  (mentions "yo" message-body)
                  (mentions "joł" message-body)
                  (mentions "hello" message-body)))
         (say destination :hello :to from-who))

        ;; kdbot is a doll
        ((and is-directed
              (mentions "kdbot" message-body))
         (say destination :kdbot))

        ((and is-directed
              (mentions "cycki" message-body))
         (say destination :notitsforyou :to from-who))

        ;; is this an accident?
        ((and (or is-public
                  is-directed)
              (mentions "przypadek?" message-body))
         (say destination "nie sądzę."))

        ;; default responder
        (is-directed
         (if (and (/= 0 (random 5))
                  (not (position from-who *excluded-from-replying-to* :test #'equal)))
             (say destination :smiles :to from-who))))))

;; those hooks handle world state
(defun join-hook (message)
  (let ((who (irc:source message))
        (where (first (irc:arguments message))))
    (store-joining-name where who)))

(defun part-hook (message)
  (let ((who (irc:source message))
        (where (first (irc:arguments message))))
    (store-parting-name where who)))

(defun names-hook (message)
  (let ((channel (third (irc:arguments message)))
        (nicks (fourth (irc:arguments message))))
    (store-names channel nicks)))

(defun nick-hook (message)
  (let ((from (irc:source message))
        (to (first (irc:arguments message))))
    (register-nick-change from to)))

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

  #+(or sbcl
        openmcl)
  (irc:start-background-message-handler *connection*))

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
