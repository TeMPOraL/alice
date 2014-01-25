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

(defparameter *msg-introduction* '("Alice Margatroid, do usług."
                                   "Alice Margatroid, kłaniam się ;)."
                                   "Mów mi Alice Margatroid."))

(defparameter *msg-version* "0.0.13. (ta porządnie zrefactorowana")
(defparameter *friendly-smiles-list* '(":)" ":)" ":)" ":)" ":)" ":)" ":)" ":)" ":)" ":)" ; yeah, a cheap trick to fake probability distribution
                                       ";)" ";)" ";)"";)" ";)" ";)"
                                       ":P" ":P" ":P" ":P" ":P"
                                       ":>" ":>" ":>"
                                       "ta da!"
                                       "maka paka!"))

(defparameter *who-in-HS* '("A skąd mam wiedzieć? Spytaj kdbot." "!at"
                            #("Czy wyglądam Ci na odźwierną?.." "!at")
                            "Nie wiem, spytaj kdbot."
                            #("kdbot jest od tego." "!at")
                            "!at"))

(defparameter *hatsune-miku* "♩♫♪♬ http://youtube.com/watch?v=O7SNIeyKbxI ♫♭♪𝅘𝅥𝅯")

(defparameter *yourewelcome* '("you're welcome"
                               "nie ma za co"
                               "sure, np."
                               "np."
                               "no problem"
                               ":)"
                               "spoko :)"))

(defparameter *at-ers* '("lenwe"
                         "lenwe|bb"
                         "marchewa"
                         "rafalt"
                         "bambucha|tiny"))

(defparameter *hello* '("czeeeeeeeeeść"
                        "oh hai!"
                        "hej"
                        "helloł"))

(defparameter *excluded-from-replying-to* '("kdbot"))

;; tools
(defun say (to-where what &key to)
  (cond ((listp what)
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

        (t (privmsg *connection* to-where "I just don't know what to say..."))))

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

         (say destination *msg-introduction*))

        ;; version number
        ((and is-directed
              (or (mentions "numer wersji" message-body)
                  (mentions "wersje" message-body)
                  (mentions "wersja" message-body)
                  (mentions "wersją" message-body)
                  (mentions "wersję" message-body)))
         (say destination *msg-version*))

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
         (say destination *yourewelcome*))
        ;; anyone in HS?
        ((and is-directed
              (mentions "jest w HS" message-body))
         (say destination *who-in-HS*))

        ;; sing
        ((and is-directed
              (or (mentions "spiew" message-body)
                  (mentions "śpiew" message-body)))
         (progn (say destination *hatsune-miku*)
                (say destination "Z dedykacją dla Bambuchy :P")))

        ;; TCP handshake for Bambucha
        ((and is-directed
              (mentions "SYN" message-body))
         (say destination "SYN-ACK" :to from-who))
         
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
         (say destination *hello* :to from-who))

        ;; fail -> ... - trolling
        ((and is-public
              (search "fail" message-body)
              (= 0 (random 4)))
         (say destination "..."))


        ;; default responder
        (is-directed
         (if (and (/= 0 (random 5))
                  (not (position from-who *excluded-from-replying-to* :test #'equal)))
             (say destination *friendly-smiles-list* :to from-who))))))


(defun join-hook (message)
  (let ((who (source message))
        (where (first (arguments message))))
    (if (position who *at-ers* :test #'equal)
        (say where "!at"))))

(defun start-alice (server nick pass &rest channels)
  (setf *nick* nick)
  (setf *connection* (connect :nickname *nick*
                              :server server))
  (setf *connected-channels* channels)

  (privmsg *connection* +nickserv+ (format nil +nickserv-identify-msg-template+ pass))

  (mapcar (lambda (channel) (join *connection* channel)) channels)

  (add-hook *connection* 'irc::irc-privmsg-message 'msg-hook)
  (add-hook *connection* 'irc::irc-join-message 'join-hook)

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
