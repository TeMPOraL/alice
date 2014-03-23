(in-package #:alice)

(defvar *connection*)

;; configurables
(defparameter *server* "")
(defvar *nick* "")
(defparameter *password* "")

(defparameter *pushover-token* "")
(defparameter *pushover-admin-user* "")

(defparameter *wolfram-app-id* "")

(defparameter *mailgun-domain* "")
(defparameter *mailgun-key* "")

(defparameter *autojoin-channels* '())

(defparameter *muted* nil)

(defparameter +nickserv+ "NickServ")
(defparameter +nickserv-identify-msg-template+ "IDENTIFY ~a")

(defparameter *full-name* "Alice Margatroid")

(defparameter *default-phrase* "Nie wiem co powiedzieć...")

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

    (:version . "0.0.49. (Ta bez głupiego regexp-injection w parserze do memos)")

    (:smiles . (":)" ":)" ":)" ":)" ":)" ":)" ":)" ":)" ":)" ":)" ; yeah, a cheap trick to fake probability distribution
                ";)" ";)" ";)"";)" ";)" ";)"
                ":P" ":P" ":P" ":P" ":P"
                ":>" ":>" ":>"
                "*sigh*" "*sigh*" "*sigh*"
                "Yukkuri shiteitte ne!" "Yukkuri shiteitte ne!"
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
                           "jasne, przekazane"
                           "sure, już przekazuję"
                           "Shanghai posłana z wiadomością"
                           "przekazane"))

    (:memo-saved . ("zapisałam jako memo"
                    "memo zapisane"
                    "zapisane; przekażę jak zobaczę"
                    "jasne, przekażę jak zobaczę"))

    (:memo-failed . ("Nie umiem wysłać tego memo. Chyba nie wiem o kogo Ci chodzi."
                     "Nie wiem komu co mamy wysłać."
                     "Shanghai wróciła i mówi, że nie wie komu przekazać..."))

    
    (:failed-in-sending-notification . ("Coś się spsuło :(."
                                        "Coś nie działa. *sigh*"
                                        "Nie umiem w notyfikacje. *sob*"))

    (:more-memos . ("Są też kolejne powiadomienia."
                    "Są kolejne mema! :)"
                    "Mam Ci coś więcej do przekazania."
                    "Shanghai mówi, że jest do Ciebie więcej powiadomień."
                    "Shanghai przyniosła więcej powiadomień."))

    (:not-yet-implemented . ("Not in my Grimoire yet."
                             "Jeszcze nie. Sprawdź pojutrze."
                             "Nie dzisiaj."
                             "Nope. Może następnym razem."
                             "Zawsze masz takie dziwne wymagania?"))

    (:nothing-to-shorten . #(("Jak mi nie powiesz co, to nic nie skrócę."
                              "Ta, a niby co?"
                              "*sigh*")
                             ("pro tip: URLe do skrócenia zaczynają się od http://"
                              "Jak mi napiszesz ładny URL zaczynający się od http:// to pomyślimy.")))

    (:failed-in-shortening . ("Nie skróciło się... *sigh*"
                              "http://cos.nie.dzia.la :/"
                              "Nie umiem w skracanie linków. *sob*"))

    (:nothing-to-compute . ("Ale co mam obliczyć? Umieść to w cudzysłowiach."
                            "Co mam przeliczyć? Umieść to w cudzysłowiach."))

    (:failed-in-computing . ("Nie umiem w Wolframa *sob*"
                             "Musisz zawsze pytać o takie dziwne rzeczy? *sigh*"
                             "Sorry, nie wyszło. *sigh*"))

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

    (:point-out-making-sense . ("!!! szybko !!!"
                                "!! ↑↑"
                                "↑↑↑↑"))
    

    (:hello . ("czeeeeeeeeeść"
               "oh hai!"
               "hej"
               "helloł"))))

(defparameter *excluded-from-replying-to* '("kdbot") "List of users that the bot won't reply to for unrecognized queries.")

(defparameter *url-regexp* "((^https?\\:.*)|(www\\..*))")
(defparameter *url-shortening-regexp* "(http.*)")
(defparameter *wolfram-query-regexp* "\"(.*)\"" "A regexp to extract question part when performing Wolfram|Alpha search.")

(defparameter *throttled-output* nil "A buffer for throttling the output to avoid flooding the channel.")

(defparameter *max-output-sequence-length* 4)

(defparameter *user-notification-medium* (make-hash-table :test 'equalp))
