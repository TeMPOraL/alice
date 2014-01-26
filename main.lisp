(ql:quickload :cl-irc)
(ql:quickload :drakma)

(defpackage :alice
  (:use :common-lisp
        :irc)
  (:export :start-alice
           :stop-alice
           :impersonate-say
           :impersonate-join
           :impersonate-part
           :mute
           :unmute))

(in-package :alice)

(defvar *connection*)
(defvar *connected-channels*)

;; configurables
(defparameter *server* "")
(defvar *nick* "")
(defparameter *password* "")

(defparameter *pushover-token* "")
(defparameter *pushover-user* "")

(defparameter *autojoin-channels* '())



(defparameter *muted* nil)

(defconstant +nickserv+ "NickServ")
(defconstant +nickserv-identify-msg-template+ "IDENTIFY ~a")

;; phrases
(defparameter *answers* 
  '((:introduction . ("Alice Margatroid, do usług."
                       "Alice Margatroid, kłaniam się ;)."
                       "Mów mi Alice Margatroid."))

    (:version . "0.0.17. (ta z pushoverem)")

    (:smiles . (":)" ":)" ":)" ":)" ":)" ":)" ":)" ":)" ":)" ":)" ; yeah, a cheap trick to fake probability distribution
                ";)" ";)" ";)"";)" ";)" ";)"
                ":P" ":P" ":P" ":P" ":P"
                ":>" ":>" ":>"
                "ta da!"
                "maka paka!"))

    (:who-in-hs . ("A skąd mam wiedzieć? Spytaj kdbot." "!at"
                   #("Czy wyglądam Ci na odźwierną?.." "!at")
                   "Nie wiem, spytaj kdbot."
                   #("kdbot jest od tego." "!at")
                   "!at"))

    (:songs . #("♩♫♪♬ http://youtube.com/watch?v=O7SNIeyKbxI ♫♭♪𝅘𝅥𝅯"
                "Z dedykacją dla Bambuchy :P"))

    (:thanks-reply . ("you're welcome"
                      "nie ma za co"
                      "sure, np."
                      "np."
                      "no problem"
                      ":)"
                      "spoko :)"))

    (:tcp . "SYN-ACK")

    (:temperature . #("pozwól, że spytam kdbot" "!temp"))
    
    (:save . (#("pewnie ;)" "!save")
              #("jasne :)" "!save")
              "!save"))
     

    (:hello . ("czeeeeeeeeeść"
               "oh hai!"
               "hej"
               "helloł"))))

(defparameter *at-ers* '("lenwe"
                        "lenwe|bb"
                        "marchewa"
                        "rafalt"
                        "bambucha|tiny"))

(defparameter *excluded-from-replying-to* '("kdbot"))

;; LOAD LOCAL CONFIG
(load "local-config.lisp")

;; functions


(defun utf-encoder (what ignore)
  (drakma:url-encode what :utf-8))

(defun send-notification (what)
  (drakma:http-request "https://api.pushover.net/1/messages.json"
                       :method :post
                       :url-encoder #'utf-encoder
                       :parameters `(("token" . ,*pushover-token*)
                                     ("user" . ,*pushover-user*)
                                     ("message" . ,what))))
;; tools
(defun say (to-where what &key to)
  (if (not *muted*)
      (cond ((keywordp what)
             (say to-where (cdr (assoc what *answers*)) :to to))
            
            ((listp what)
             (say to-where
                  (elt what
                       (random (length what)))
                  :to to))

            ((stringp what)
             (if (null to)
                 (privmsg *connection* to-where what)
                 (privmsg *connection* to-where (concatenate 'string to ": " what))))

            ((vectorp what)
             (map 'nil
                  (lambda (msg)
                    (say to-where msg :to to))
                  what))

            (t (privmsg *connection* to-where "I just don't know what to say...")))))

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

(defun mentions (what string)
  (search what string))

(defun mentions-name (name string)
  (mentions name string))

;;; handling

(defun msg-hook (message)
    (let ((destination (if (string-equal (first (arguments message)) *nick*)
                         (source message)
                         (first (arguments message))))
          (is-private (private-message-p message))
          (is-public (public-message-p message))
          (is-directed (directed-message-p message))
          (from-who (source message))
          (message-body (second (arguments message))))

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
              (or (mentions "temperatura" message-body)
                  (mentions "temperature" message-body)
                  (mentions "temperaturę" message-body)
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
         (progn (say destination :songs)))

        ;; TCP handshake for Bambucha
        ((and is-directed
              (mentions "SYN" message-body))
         (say destination :tcp :to from-who))
         
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

        ;; ping temporal
        ((and is-directed
              (and (or (mentions "TeMPOraL" message-body)
                       (mentions "temporal" message-body))
                   (or (mentions "powiadom" message-body)
                       (mentions "pingnij" message-body))))
         (progn (say destination "ok, przekazałam")
                (send-notification message-body)))

        ;; is this an accident?
        ((and (or is-public
                  is-directed)
              (mentions "przypadek?" message-body))
         (say destination "nie sądzę."))

        ;; fail -> ... - trolling
        ((and is-public
              (search "fail" message-body)
              (= 0 (random 4)))
         (say destination "..."))

        ;; default responder
        (is-directed
         (if (and (/= 0 (random 5))
                  (not (position from-who *excluded-from-replying-to* :test #'equal)))
             (say destination :smiles :to from-who))))))

(defun join-hook (message)
  (let ((who (source message))
        (where (first (arguments message))))
    (if (position who *at-ers* :test #'equal)
        (say where "!at"))))

(defun start-alice (&key (server *server*) (nick *nick*) (password *password*) (channels *autojoin-channels*))
  (setf *nick* nick)
  (setf *connection* (connect :nickname *nick*
                              :server server))
  (setf *connected-channels* channels)

  (privmsg *connection* +nickserv+ (format nil +nickserv-identify-msg-template+ password))

  (mapcar (lambda (channel) (join *connection* channel)) channels)

  (add-hook *connection* 'irc::irc-privmsg-message 'msg-hook)
  (add-hook *connection* 'irc::irc-join-message 'join-hook)

  #+(or sbcl
        openmcl)
  (start-background-message-handler *connection*))

(defun stop-alice (&optional (msg "Goodbye!"))
      (quit *connection* msg))

(defun mute ()
  (setf *muted* t))

(defun unmute ()
  (setf *muted* nil))

;; impersonate function

(defun impersonate-say (destination what)
  (privmsg *connection* destination what))

(defun impersonate-join (channel &key password)
  (join *connection* channel :password password))

(defun impersonate-part (channel)
  (part *connection* channel))
