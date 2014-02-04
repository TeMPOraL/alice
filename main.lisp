(ql:quickload :cl-irc)
(ql:quickload :alexandria)
(ql:quickload :drakma)
(ql:quickload :cl-unicode)
(ql:quickload :cl-ppcre)
(ql:quickload :cxml)

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

(defparameter *wolfram-app-id* "")

(defparameter *autojoin-channels* '())

(defparameter *muted* nil)

(defconstant +nickserv+ "NickServ")
(defconstant +nickserv-identify-msg-template+ "IDENTIFY ~a")

(defparameter *full-name* "Alice Margatroid")

;;; phrases
;; phrases are defined as an alist of label and possible phrases to speak
;; plain-text value will be said directly
;; list of values -> bot will pick one at random
;; vector of values -> bot will output all of them sequencially, possibly throttling output to avoid flooding the channel
;; 
;; values are read recursively, i.e. encoding a symbol will cause bot to find the proper phrase in
;; this alist, and e.g. list of vector means "pick a sequence of things to say at random"
(defparameter *answers* 
  '((:introduction . (#("Alice Margatroid."
                        "You mustn't consider me a normal human. I'm normal, just not human!")

                      "Alice Margatroid, w czym mogę pomóc?."
                      "Mów mi Alice Margatroid."
                      "Alice Margatroid, the Seven-Colored Puppeteer."
                      "Pozornie Zapracowana Youkai, Alice Margatroid."))

    (:version . "0.0.31. (The Girl Who Played With People's Shapes and could also hear their names)")

    (:smiles . (":)" ":)" ":)" ":)" ":)" ":)" ":)" ":)" ":)" ":)" ; yeah, a cheap trick to fake probability distribution
                ";)" ";)" ";)"";)" ";)" ";)"
                ":P" ":P" ":P" ":P" ":P"
                ":>" ":>" ":>"
                "*sigh*" "*sigh*" "*sigh*"
                "Yukkuri shiteitte ne!" "Yukkuri shiteitte ne!" "Yukkuri shiteitte ne!"
                "maka paka!"))

    (:who-in-hs . ("A skąd mam wiedzieć? Spytaj kdbot."
                   #("Czy wyglądam Ci na odźwierną?.." "!at")
                   "Nie wiem, spytaj kdbot."
                   #("kdbot jest od tego." "!at")
                   "!at"))

    (:songs . (#("♩♫♪♬ http://youtube.com/watch?v=O7SNIeyKbxI ♫♭♪𝅘𝅥𝅯"
                 "Z dedykacją dla Bambuchy :P")
               "♫♭ http://www.youtube.com/watch?v=mN8JTgTs1i4 ♩♫"
               "http://www.youtube.com/watch?v=26Mat6YDpHE ♫♪"
               "♫♪ http://www.youtube.com/watch?v=W5ESyEzS1tA ♪𝅘𝅥𝅯"

               #("http://www.youtube.com/watch?v=rAbhJk4YJns"
                 ("*sigh*"
                  "*sob*"
                  "btw. jak ktoś widział Marisę, to niech da znać..."
                  "true story *sigh*"
                  "\"Shanghai Shanghai Shanghai Shanghai Hourai Hourai Hourai Hourai! ♫♪♬\""
                  "Why-why-why-why-why don't I miss you a lot forever? ♩♫♪...  *sigh*"))))

    (:thanks-reply . ("you're welcome"
                      "nie ma za co"
                      "sure, np."
                      "np."
                      "no problem"
                      ":)"
                      "spoko :)"))

    (:tcp . "SYN-ACK")

    (:temperature . #("pozwól, że spytam kdbot" "!temp"))
    
    (:save . (#("mhm" "!save")
              #("jasne :)" "!save")
              "!save"))

    (:notification-sent . ("ok, przekazałam"
                         "jasne, przekazuję"
                         "sure, już przekazuję"
                         "przekazane"))

    (:nothing-to-compute . ("Ale co mam obliczyć? Umieść to w cudzysłowiach."
                            "Co mam przeliczyć? Umieść to w cudzysłowiach."))

    (:nothing-computed . ("Nic się nie policzyło :(."
                          "Brak wyniku; spytaj o coś innego."
                          "Nope, nic nie ma."))

    (:throttled-message . ("... jest tego więcej, wyświetlić?"
                           "... wyświetlać dalej?"))

    (:kdbot . ("kdbot? jest moją ulubioną lalką."
               "kdbot to bardzo umiejęŧna lalka."
               "kdbot to świetna lalka"))

    (:notitsforyou . ("Chyba żartujesz."
                      "Nie pozwalaj sobie na za dużo."
                      "Może sam pokaż swoje najpierw."
                      "Troszkę przesadzasz."))

    (:hello . ("czeeeeeeeeeść"
               "oh hai!"
               "hej"
               "helloł"))))

(defparameter *excluded-from-replying-to* '("kdbot") "List of users that the bot won't reply to for unrecognized queries.")

(defparameter *wolfram-query-regexp* "\"(.*)\"" "A regexp to extract question part when performing Wolfram|Alpha search.")

(defparameter *throttled-output* nil "A buffer for throttling the output to avoid flooding the channel.")

(defparameter *max-output-sequence-length* 4)

;; LOAD LOCAL CONFIG
(load "local-config.lisp")

;; functions
(defun do-google-search (query)
  )

(defun do-wolfram-computation (query)
  (flet ((xml-response-to-speechstrings (xml)
           (coerce (alexandria:flatten (map 'list
                                            (lambda (el)
                                              (let ((val (dom:first-child el)))
                                                (if val
                                                    (split-sequence:split-sequence #\Newline (dom:data val)))))
                                            (dom:get-elements-by-tag-name xml "plaintext")))
                   'vector))

         (get-xml-response (query)
           (let ((response (drakma:http-request "http://api.wolframalpha.com/v2/query"
                                                :external-format-out :UTF-8
                                                :parameters `(("appid" . ,*wolfram-app-id*)
                                                              ("input" . ,query)
                                                              ("format" . "plaintext")))))
             (cxml:parse-rod response
                             (cxml-dom:make-dom-builder))))
         (clean-up (response)
           (let ((cleaned-up (remove nil response)))
             (if (= (length cleaned-up) 0)
                 :nothing-computed
                 cleaned-up))))

    ;; code
    (if query
        (clean-up (xml-response-to-speechstrings (get-xml-response query)))
        :nothing-to-compute)))

(defun parse-message-for-wolfram-computation (text)
  (cl-ppcre:scan-to-strings *wolfram-query-regexp* text))

(defun send-notification (what &optional (from ""))
  (drakma:http-request "https://api.pushover.net/1/messages.json"
                       :method :post
                       :external-format-out :UTF-8
                       :parameters `(("token" . ,*pushover-token*)
                                     ("user" . ,*pushover-user*)
                                     ("title" . ,*full-name*)
                                     ("message" . ,(concatenate 'string "<" from "> " what)))
                       :content "hack"
                       :content-length 4))
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
                 (privmsg *connection* to-where what)
                 (privmsg *connection* to-where (concatenate 'string to ": " what))))

            ((vectorp what)
             (let ((tosay (throttle what)))
               (map 'nil
                    (lambda (msg)
                      (say to-where msg :to to))
                    tosay)))

            (t (privmsg *connection* to-where "I just don't know what to say...")))))



;; WORLD STATE
;; (todo move elsewhere)
(defun store-joining-name (channel name)
  (say "TeMPOraL" (concatenate 'string "JOINING: " name " (" channel ")")))

(defun store-parting-name (channel name)
  (say "TeMPOraL" (concatenate 'string "LEAVING: " name " (" channel ")")))

(defun store-names (channel names)
  (say "TeMPOraL" (concatenate 'string "NAMES: " names " (" channel ")")))

;;; utils
(defun mentions (what string)
  (search what string))

(defun mentions-name (name string)
  (mentions name string))

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
              (mentions "names" message-body))
         (names *connection* destination))
         
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
  (let ((who (source message))
        (where (first (arguments message))))
    (store-joining-name where who)))

(defun part-hook (message)
  (let ((who (source message))
        (where (first (arguments message))))
    (store-parting-name where who)))

(defun names-hook (message)
  (let ((channel (third (arguments message)))
        (nicks (fourth (arguments message))))
    (store-names channel nicks)))

;; entry point

(defun start-alice (&key (server *server*) (nick *nick*) (password *password*) (channels *autojoin-channels*))
  (setf *nick* nick)
  (setf *connection* (connect :nickname *nick*
                              :server server))
  (setf *connected-channels* channels)

  (privmsg *connection* +nickserv+ (format nil +nickserv-identify-msg-template+ password))

  (mapcar (lambda (channel) (join *connection* channel)) channels)

  (add-hook *connection* 'irc::irc-privmsg-message 'msg-hook)
  (add-hook *connection* 'irc::irc-join-message 'join-hook)
  (add-hook *connection* 'irc::irc-part-message 'part-hook)
  (add-hook *connection* 'irc::irc-rpl_namreply-message 'names-hook)

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
