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
(defmacro with-temporary-environment (&body code)
  `(let ((alice::*nick* "Alice_M"))
     ,@code))

;;; helpers
(defun make-input (raw-text &key (publicp t) (privatep nil) (directedp t))
  (make-instance 'alice::message
                 :raw-text raw-text
                 :publicp publicp
                 :privatep privatep
                 :directedp directedp
                 :author "TestUser"
                 :channel (when privatep "#test")
                 :reply-to (if privatep
                               "TestUser"
                               "#test")))

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

(defmacro test-is-primary-match (name matchspec &body inputs)
  "Create test named `NAME' which ensures that each of `INPUTS' strings generates an distinct match to `MATCHSPEC'."
  `(test ,name
     (with-temporary-environment
       (mapc (lambda (input)
               (is (only-primary-match ,matchspec (apply 'make-input input))))
             ',inputs))))

;;; tests

(test-is-primary-match primary-match-version :version
  ;; pl_PL
  ("Alice_M, wersja")
  ("Alice_M, jaką masz wersję?")
  ("Jaki masz numer wersji, Alice_M?")
  ("Alice_M: podaj numer wersji!")

  ;; en_US
  ("Alice_M, version")
  ("Alice_M, version number please!"))

(test-is-primary-match primary-match-wolfram-alpha-query-full :wolfram-alpha-query-full
  ("Foo"))

(test-is-primary-match primary-match-wolfram-alpha-query :wolfram-alpha-query
  ("Foo"))

(test-is-primary-match primary-match-whois :whois
  ("Foo"))

(test-is-primary-match primary-match-assign-name-alias :assign-name-alias
  ("Foo"))

(test-is-primary-match primary-match-shorten-url :shorten-url
  ("Foo"))

(test-is-primary-match primary-match-notify-user :notify-user
  ("Foo"))

(test-is-primary-match primary-match-delayed-notify-user :delayed-notify-user
  ("Foo"))

(test-is-primary-match primary-match-track-package :track-package
  ("Foo"))

(test-is-primary-match primary-match-github-issues-link :github-issues-link
  ("Foo"))

(test-is-primary-match primary-match-add-github-issue :add-github-issue
  ("Foo"))

(test-is-primary-match primary-match-random-frequency :random-frequency
  ("Foo"))

(test-is-primary-match primary-match-whats-the-frequency :whats-the-frequency
  ("Foo"))

(test-is-primary-match primary-match-events-list :events-list
  ("Foo"))

(test-is-primary-match primary-match-marisa :marisa
  ("Foo"))

(test-is-primary-match primary-match-introductions :introductions
  ("Foo"))

(test-is-primary-match primary-match-thanks-reply :thanks-reply
  ("Foo"))

(test-is-primary-match primary-match-sing :sing
  ("Foo"))

(test-is-primary-match primary-match-talking-about-me :talking-about-me
  ("Foo"))

(test-is-primary-match primary-match-tcp-handshake :tcp-handshake
  ("Foo"))

(test-is-primary-match primary-match-hello :hello
  ("Foo"))

(test-is-primary-match primary-match-repo-link :repo-link
  ("Foo"))

(test-is-primary-match primary-match-dice-throw :dice-throw
  ("Foo"))

(test-is-primary-match primary-match-goodnight :goodnight
  ("Foo"))

(test-is-primary-match primary-match-makes-sense-troll :makes-sense-troll
  ("Foo"))

(test-is-primary-match primary-match-coincidence? :coincidence?
  ("Foo"))

(test-is-primary-match primary-match-yolo :yolo
  ("Foo"))

(test-is-primary-match primary-match-throttle-continue :throttle-continue
  ("Foo"))

(test-is-primary-match primary-match-default-response :default-response
  ("Foo"))



(add-suite 'input-matchers)
