(in-package #:alice)

(defvar *connection*)

;; configurables
(defparameter *server* "")
(defvar *nick* "")
(defparameter *password* "")

(defparameter *pushover-token* "")
(defparameter *pushover-user* "")

(defparameter *wolfram-app-id* "")

(defparameter *autojoin-channels* '())

(defparameter *muted* nil)

(defparameter +nickserv+ "NickServ")
(defparameter +nickserv-identify-msg-template+ "IDENTIFY ~a")

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

    (:version . "0.0.32. (The Girl with Grimoire)")

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

