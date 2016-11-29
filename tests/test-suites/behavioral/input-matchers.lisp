(in-package #:alice-tests)

(def-suite input-matchers :description "Test how various inputs match in Alice.")

(in-suite input-matchers)

(defmethod print-object ((message alice::message) stream)
  (print-unreadable-object (message stream :type t :identity nil)
    (format stream
            "(~A ~A ~A) ~A@~A: ~A"
            (alice::publicp message)
            (alice::privatep message)
            (alice::directedp message)
            (alice::author message)
            (alice::reply-to message)
            (alice::raw-text message))))

;;TODO eventually replace with `DEF-FIXTURE'/`WITH-FIXTURE' from FiveAM.
(defmacro with-temporary-environment (bindings &body code)
  `(let ((alice::*nick* "Alice_M"))
     (let ,bindings
       ,@code)))

;;; helpers
(defun make-input (raw-text &key (publicp t) (privatep nil) (directedp t) (channel "#test") (author "TestUser"))
  (make-instance 'alice::message
                 :raw-text raw-text
                 :publicp publicp
                 :privatep privatep
                 :directedp directedp
                 :author author
                 :channel (unless privatep channel)
                 :reply-to (if privatep
                               author
                               channel)))

(defun best-match (match input)
  "Test if `MATCH' is the best match for `INPUT'."
  (eql match
       (car (first (alice::best-matchers input)))))

(defun only-primary-match (match input)
  "Test if `MATCH' is the best match for `INPUT' and that there are no other matches that tied."
  (flet ((remove-non-equally-scored-matches (score matches)
           (remove-if-not (lambda (match)
                            (= (cdr match) score))
                          matches)))
   (let ((best-matches (alice::best-matchers input)))
     (and (= 1 (length (remove-non-equally-scored-matches (cdr (first best-matches))
                                                          best-matches)))
          (eql match
               (car (first best-matches)))))))

(defun no-match (input)
  "Test if `INPUT' does not match to anything specific."
  (= 0 (length (alice::best-matchers input))))

(defmacro test-no-match (name &body inputs)
  `(test ,name
     (mapc (lambda (input)
             (is (no-match (apply 'make-input input))))
           ',inputs)))

;;; TODO add bindings to pass to `DEF-FIXTURE' / `WITH-TEMPORARY-ENVIRONMENT'.
(defmacro test-is-primary-match (name matchspec bindings &body inputs)
  "Create test named `NAME' which ensures that each of `INPUTS' strings generates an distinct match to `MATCHSPEC'."
  `(test ,name
     (with-temporary-environment ,bindings
       (mapc (lambda (input)
               (is (only-primary-match ,matchspec (apply 'make-input input))))
             ',inputs))))

;;; tests

(test-is-primary-match primary-match-version :version ()
  ;; pl_PL
  ("Alice_M, wersja")
  ("Alice_M, jaką masz wersję?")
  ("Jaki masz numer wersji, Alice_M?")
  ("Alice_M: podaj numer wersji!")

  ;; en_US
  ("Alice_M, version")
  ("Alice_M, version number please!"))

(test-is-primary-match primary-match-wolfram-alpha-query-full :wolfram-alpha-query-full ()
  ("Alice_M, oblicz proszę \"2+2\", pełny wynik")
  ("Alice_M, policz proszę \"2+2\", pokaż wszystko"))

(test-is-primary-match primary-match-wolfram-alpha-query :wolfram-alpha-query ()
  ("Alice_M policz \"2+2\""))

(test-is-primary-match primary-match-whois :whois ()
  ;; pl_PL
  ("Kim jestem, Alice_M?")
  ("Alice_M, kim jest ShanghaiDoll?")
  ("Alice_M, kto to jest HouraiDoll?"))

(test-is-primary-match primary-match-assign-name-alias :assign-name-alias ()
  ("Alice_M, zapamiętaj: hourai shanghai" :publicp nil))

(test-is-primary-match primary-match-shorten-url :shorten-url ()
  ("Skróć http://google.pl/")
  ("Alice_M, skróć proszę http://google.com/"))

(test-is-primary-match primary-match-notify-user :notify-user ()
  ("Przekaż tacie test."))

(test-is-primary-match primary-match-delayed-notify-user :delayed-notify-user ()
  ("Przypomnij tacie pojutrze test."))

(test-is-primary-match primary-match-track-package :track-package ()
  ("Gdzie jest XXX?"))

(test-is-primary-match primary-match-github-issues-link :github-issues-link ()
  ;; pl_PL
  ("Pokaż issues.")

  ;; en_US
  ("Show issues."))

(test-is-primary-match primary-match-add-github-issue :add-github-issue ()
  ;; pl_PL
  ("Dodaj issue \"XXX\".")
  ("dodaj issue \"XXX\".")
  ("nowy issue \"XXX\".")             ;uh oh, a case sensitivity bug lurking inside...

  ;; en_US
  ("add issue \"XXX\".")
  ("new issue \"XXX\"."))

(test-is-primary-match primary-match-random-frequency :random-frequency ()
  ("Alice_M, częstotliwość na dziś to...?"))

(test-is-primary-match primary-match-whats-the-frequency :whats-the-frequency ()
  ("Alice_M, co jest na częstotliwości 123.456"))

(test-is-primary-match primary-match-events-list :events-list () ;FIXME this is not active yet anyway
  ("Alice_M, jest coś ciekawego w tym tygodniu?"))

(test-is-primary-match primary-match-marisa :marisa ()
  ("Marisa" :directedp nil)
  ("Kirisame" :directedp nil))

(test-is-primary-match primary-match-introductions :introductions ()
  ("Alice_M, przedstaw się!"))

(test-is-primary-match primary-match-thanks-reply :thanks-reply ()
  ("Dzięki, Alice_M!")
  ("thx, Alice_M!"))

(test-is-primary-match primary-match-sing :sing ()
  ("Alice_M, zaśpiewaj coś!"))

(test-is-primary-match primary-match-talking-about-me :talking-about-me ()
  ("Alice Margatroid" :directedp nil))

(test-is-primary-match primary-match-tcp-handshake :tcp-handshake ()
  ("Alice_M: SYN"))

(test-is-primary-match primary-match-hello :hello ()
  ("Alice_M: hej")
  ("Alice_M: witaj")
  ("witaj, Alice_M"))

(test-is-primary-match primary-match-repo-link :repo-link ()
  ("Alice_M, pokaż źródła")
  ("źródła, Alice_M"))

(test-is-primary-match primary-match-dice-throw :dice-throw ()
  ("Alice_M, rzuć K6."))

(test-is-primary-match primary-match-goodnight :goodnight ()
  ("dobranoc" :directedp nil))

(test-is-primary-match primary-match-coincidence? :coincidence? ()
  ("przypadek?" :directedp nil)
  ("pszypadeg?":directedp nil))

(test-is-primary-match primary-match-default-response :default-response ()
  ("Foo")
  ("Bar"))

;;; tests requiring special environment

(test-is-primary-match primary-match-throttle-continue :throttle-continue ((alice::*throttled-output* '("foo" "bar"))) ;FIXME, need to bind alice::*throttled-output* somehow.
  ("Alice_M, tak.")
  ("Alice_M, poproszę."))

(test-is-primary-match primary-match-makes-sense-troll :makes-sense-troll () ;FIXME need to bind message's reply-to to "#hackerspace-krk" as it's channel-specific
  ("robi sens":directedp nil :channel "#hackerspace-krk"))

;;; no match

(test-no-match yolo-does-not-match-to-anything ;YOLO disabled by popular request
  ("YOLO" :directedp nil))

(add-suite 'input-matchers)
