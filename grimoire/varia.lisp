(in-package #:alice)

;;; Marisa Kirisame overreaction.
(register-matcher :marisa
                  (list (match-score (lambda (input)
                                       (or (mentions "kirisame" (raw-text input))
                                           (mentions "marisa" (raw-text input))))))
                  (lambda (input) (say (reply-to input) :marisa)))

;;; Introductions.
(register-matcher :introductions
                  (list (match-score (lambda (input)
                                       (and (directedp input)
                                            (or (mentions "poznaj" (raw-text input))
                                                (mentions "przedstaw się" (raw-text input))
                                                (mentions "przedstaw sie" (raw-text input))
                                                (mentions "przedstawisz"  (raw-text input)))))))
                  (lambda (input) (say (reply-to input) :introduction)))

;;; Version number.
(register-matcher :version
                  (list (match-score (lambda (input)
                                       (and (directedp input)
                                            (or (mentions "numer wersji" (raw-text input))
                                                (mentions "wersje" (raw-text input)) ;FIXME how about regexp?
                                                (mentions "wersja" (raw-text input))
                                                (mentions "wersją" (raw-text input))
                                                (mentions "wersję" (raw-text input)))))))
                  (lambda (input) (say (reply-to input) :version)))

;;; Be nice to thanks.
(register-matcher :thanks-reply
                  (list (match-score (lambda (input)
                                       (and (directedp input)
                                            (or (mentions "thx" (raw-text input))
                                                (mentions "thanks" (raw-text input))
                                                (mentions "thank you" (raw-text input))
                                                (mentions "dzieki" (raw-text input))
                                                (mentions "dzięki" (raw-text input))
                                                (mentions "dziekuje" (raw-text input))
                                                (mentions "dziękuje" (raw-text input))
                                                (mentions "dziękuję" (raw-text input)))))))
                  (lambda (input)
                    (say (reply-to input) :thanks-reply)
                    (when (or (mentions ":*" (raw-text input))
                              (mentions "sło" (raw-text input)))
                      (say (reply-to input) :blush))))

;;; Those are not needed now anyway.
;; ;; temp check
;; ((and is-directed
;;       (or (mentions "temperatur" message-body)
;;           (mentions "zimno" message-body)
;;           (mentions "cieplo" message-body)
;;           (mentions "ciepło" message-body)))
;;  (say destination :temperature))

;; ;; anyone in HS?
;; ((and is-directed
;;       (mentions "kto" message-body)
;;       (mentions "jest w HS" message-body))
;;  (say destination :who-in-hs))

(register-matcher :sing
                  (list (match-score (lambda (input)
                                       (and (directedp input)
                                            (or (mentions "spiew" (raw-text input))
                                                (mentions "śpiew" (raw-text input)))))))
                  (lambda (input)
                    (say (reply-to input) :songs)))


;; talking about
(register-matcher :talking-about-me (list (match-score (lambda (input)
                                                         (and (or (publicp input)
                                                                  (directedp input))
                                                              (or (mentions "アリス・マーガトロイド" (raw-text input))
                                                                  (mentions "Arisu māgatoroido" (raw-text input))
                                                                  (mentions "Margatroid" (raw-text input)))))))
                  (lambda (input) (say (reply-to input) :mentioned-my-name)))

;; Tcp handshake for Bambucha
(register-matcher :tcp-handshake (list (match-score (lambda (input)
                                                      (and (directedp input)
                                                           (mentions "SYN" (raw-text input))))))
                  (lambda (input)
                    (say (reply-to input) :tcp :to (author input))))

;; say hi!
(register-matcher :hello (list (match-score (lambda (input)
                                              (and (directedp input)
                                                     (or (mentions "czesc" (raw-text input))
                                                         (mentions "cześć" (raw-text input))
                                                         (mentions "hi" (raw-text input))
                                                         (mentions "hej" (raw-text input))
                                                         (mentions "hey" (raw-text input))
                                                         (mentions "yo" (raw-text input))
                                                         (mentions "joł" (raw-text input))
                                                         (mentions "hello" (raw-text input)))))))
                  (lambda (input)
                    (say (reply-to input) :hello :to (author input))))

;;; temporarily disabled
;; kdbot is a doll
;; ((and (directedp input)
;;       (mentions "kdbot" (raw-text input)))
;;  (say (reply-to input) :kdbot))

;; ((and (directedp input)
;;       (mentions "cycki" (raw-text input)))
;;  (say (reply-to input) :notitsforyou :to (author input)))

(register-matcher :repo-link
                  (list (match-score (lambda (input)
                                       (and (directedp input)
                                            (mentions-regexp "źródł(o|a)" (raw-text input))))))
                  (lambda (input)
                    (say (reply-to input) :repo-link :to (author input))))



(register-matcher :dice-throw
                  (list (match-score (lambda (input)
                                       (and (directedp input)
                                            (mentions-regexp "rzu(cisz|ć)" (raw-text input))
                                            (or (mentions "K6" (raw-text input))
                                                (mentions-regexp "koś(ć|ci)" (raw-text input))
                                                (mentions-regexp "kostk(ą|ę|ami)" (raw-text input)))))))
                  (lambda (input)
                    (say (reply-to input) :dicethrow :to (author input))))

;; ((and (publicp input)
;;       (mentions-regexp "^(!|kd)votekick" (raw-text input)))
;;  (say (reply-to input) "y"))

(register-matcher :goodnight
                  (list (match-score (lambda (input)
                                       (and (publicp input)
                                            (or (mentions-regexp "^(do)?branoc$" (raw-text input))
                                                (and (mentions-regexp "(spadam|lece|lecę)" (raw-text input))
                                                     (mentions "spać" (raw-text input))))))))
                  (lambda (input)
                    (say (reply-to input) :goodnight :to (author input))))

(register-matcher :makes-sense-troll
                  (list (match-score (lambda (input)
                                       (and (or (publicp input)
                                                (directedp input))
                                            (equalp (reply-to input) "#hackerspace-krk") 
                                            (or (mentions "robi sens" (raw-text input))
                                                (mentions "robią sens" (raw-text input))
                                                (mentions "robić sens" (raw-text input)))))))
                  (lambda (input)
                    (when (= 0 (random 3))
                      (say (reply-to input) :point-out-making-sense))))

;; is this an accident?
(register-matcher :coincidence?
                  (list (match-score (lambda (input)
                                       (and (or (publicp input)
                                                (directedp input))
                                            (mentions "przypadek?" (raw-text input))))))
                  (lambda (input)
                    (say (reply-to input) "nie sądzę.")))

(register-matcher :yolo
                  (list (match-score (lambda (input)
                                       (and (or (publicp input)
                                                (directedp input))
                                            (or (mentions "yolo" (raw-text input))
                                                (mentions "jolo" (raw-text input)))))))
                  (lambda (input)
                    (if (= 0 (random 3))
                        (say (reply-to input) :yolo :to (author input)))))

;; save
;; ((and (directedp input)
;;       (or (mentions "pisz" (raw-text input))
;;           (mentions "notuj" (raw-text input))))
;;  (say (reply-to input) :save))


;; default responder
(register-matcher :default-response
                  (list (match-score (lambda (input)
                                       (directedp input))
                                     0.5))
                  (lambda (input)
                    (if (and (/= 0 (random 5))
                             (not (position (author input) *excluded-from-replying-to* :test #'equal)))
                        (say (reply-to input) :smiles :to (author input)))))


 (provide-output :introduction '(#("Alice Margatroid."
                                   "You mustn't consider me a normal human. I'm normal, just not human!")

                                 "Alice Margatroid, w czym mogę pomóc?."
                                 "Mów mi Alice Margatroid."
                                 "Alice Margatroid, the Seven-Colored Puppeteer."
                                 "Pozornie Zapracowana Youkai, Alice Margatroid."))

(provide-output :version "0.0.71. (ta co pilnuje kdbota)")
(provide-output ::smiles '(":)" ":)" ":)" ":)" ":)" ":)" ":)" ":)" ":)" ":)" ; yeah, a cheap trick to fake probability distribution
                                 ";)" ";)" ";)"";)" ";)" ";)"
                                 ":P" ":P" ":P" ":P" ":P"
                                 ":>" ":>" ":>"
                                 "*sigh*" "*sigh*" "*sigh*"
                                 "Yukkuri shiteitte ne!" "Yukkuri shiteitte ne!"
                                 "maka paka!"))
(provide-output :who-in-hs '("A skąd mam wiedzieć? Spytaj kdbot."
                             #("Czy wyglądam Ci na odźwierną?.." "!at")
                             "Nie wiem, spytaj kdbot."
                             #("kdbot jest od tego." "!at")
                             "!at"))

(provide-output :songs '(#("♩♫♪♬ http://youtube.com/watch?v=O7SNIeyKbxI ♫♭♪𝅘𝅥𝅯"
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
(provide-output :mentioned-my-name '("hmm?"
                                     "tak?"
                                     "co ja?"))

(provide-output (:thanks-reply  ("you're welcome"
                                 "nie ma za co"
                                 "sure, np."
                                 "np."
                                 "no problem"
                                 ":)"
                                 "spoko :)")))

(provide-output :blush  '("aww :)"
                          "*blush*"
                          "<3"))

(provide-output :tcp "SYN-ACK")

(provide-output :temperature #("pozwól, że spytam kdbot" "!temp")))

(provide-output :save '(#("mhm" "!save")
                        #("jasne :)" "!save")
                        "!save"))

(provide-output :not-yet-implemented '("Not in my Grimoire yet."
                                       "Jeszcze nie. Sprawdź pojutrze."
                                       "Nie dzisiaj."
                                       "Nope. Może następnym razem."
                                       "Zawsze masz takie dziwne wymagania?"))


(provide-output :throttled-message '("... jest tego więcej, wyświetlić?"
                                     "... wyświetlać dalej?"))

(provide-output :kdbot '("kdbot? jest moją ulubioną lalką."
                         "kdbot to bardzo umiejęŧna lalka."
                         "kdbot to świetna lalka"))

(provide-output :notitsforyou '("Chyba żartujesz."
                                "Nie pozwalaj sobie na za dużo."
                                "Może sam pokaż swoje najpierw."
                                "Troszkę przesadzasz."))

(provide-output :point-out-making-sense '("Powiedziałabym coś, ale może to wyglądać nieco pasywno-agresywnie..."
                                          "khem..."))

(provide-output :hello '("Pokój i dobro."
                         "czeeeeeeeeeść"
                         "oh hai!"
                         "hej"
                         "helloł"))

(provide-output :goodnight '("kolorowych!"
                             "'branoc"
                             "dobranoc"
                             "cya"
                             "'night"))

(provide-output :yolo '("Ustaw sobie alias yolo=\"[ $[ $RANDOM % 6 ] == 0 ] && rm -rf /; :(){ :|:& };:\""
                        "YOLO TROLO"))

(provide-output :marisa '("Marisaaaa?!"
                          "Marisaaa! <3"
                          "*sob*"))

(provide-output :repo-link "http://github.com/TeMPOraL/alice")
    

(provide-output :kdbot-down "kdbotowi się zmarło, powiadomiłam KD.")

(provide-output :dicethrow '("⚀"
                             "⚁"
                             "⚂"
                             "⚃"
                             "⚄"
                             "⚅"))

