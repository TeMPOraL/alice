(in-package #:alice)


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

    (:wolfram-turned-off . ("Skoro nie chcecie, żebym cokolwiek liczyła to o to nie proście."
                            "Takiego wała."
                            "http://wolframalpha.com, samemu sobie policz."))

    (:version . "0.0.71. (ta co pilnuje kdbota)")

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

    (:mentioned-my-name . ("hmm?"
                           "tak?"
                           "co ja?"))

    (:thanks-reply . ("you're welcome"
                      "nie ma za co"
                      "sure, np."
                      "np."
                      "no problem"
                      ":)"
                      "spoko :)"))

    (:blush . ("aww :)"
               "*blush*"
               "<3"))

    (:tcp . "SYN-ACK")

    (:temperature . #("pozwól, że spytam kdbot" "!temp"))
    
    (:save . (#("mhm" "!save")
              #("jasne :)" "!save")
              "!save"))




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
                          "Nope, nic nie ma."
                          "Nie pykło."))

    (:failed-to-track-package . ("Nie pykło :(."
                                 "Nie udało mi sie sprawdzić :(."
                                 "Coś nie działa."))

    (:no-package-to-track . ("Podaj poprawny numer trackingowy paczki. Póki co, umiem śledzić tylko te w bazie Poczty Polskiej."))

    (:throttled-message . ("... jest tego więcej, wyświetlić?"
                           "... wyświetlać dalej?"))

    (:issue-added . ("Zapisane."
                     "Dodane."
                     "Zanotowane."))

    (:failed-to-add-issue . "Chyba nie umiem w Githuba...")

    (:no-issue-to-add . ("Ale co mam dodać? Umieść opis w cudzysłowiach."
                         "Opis w cudzysłowiach proszę."))

    (:kdbot . ("kdbot? jest moją ulubioną lalką."
               "kdbot to bardzo umiejęŧna lalka."
               "kdbot to świetna lalka"))

    (:notitsforyou . ("Chyba żartujesz."
                      "Nie pozwalaj sobie na za dużo."
                      "Może sam pokaż swoje najpierw."
                      "Troszkę przesadzasz."))

    (:point-out-making-sense . ("Powiedziałabym coś, ale może to wyglądać nieco pasywno-agresywnie..."
                                "khem..."))
    
    (:hello . ("Pokój i dobro."
               "czeeeeeeeeeść"
               "oh hai!"
               "hej"
               "helloł"))

    (:goodnight . ("kolorowych!"
                   "'branoc"
                   "dobranoc"
                   "cya"
                   "'night"))

    (:yolo . ("Ustaw sobie alias yolo=\"[ $[ $RANDOM % 6 ] == 0 ] && rm -rf /; :(){ :|:& };:\""
              "YOLO TROLO"))

    (:marisa . ("Marisaaaa?!"
                "Marisaaa! <3"
                "*sob*"))

    (:repo-link . "http://github.com/TeMPOraL/alice")
    

    (:kdbot-down . "kdbotowi się zmarło, powiadomiłam KD.")

    (:dicethrow . ("⚀"
                   "⚁"
                   "⚂"
                   "⚃"
                   "⚄"
                   "⚅"))))



