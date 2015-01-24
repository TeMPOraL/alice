(in-package #:alice)

(defvar *frequencies* '(("467.550" . "Taxi Barbakan, to z Jedyneczką :).")
                        ("145.650" . "Pokój i dobro, wita przemiennik na Koskowej Górze.")
                        ("145.550" . "Krakowska wywoławcza.")))

(define-constant +freq-extraction-regexp+ "(\\d{3}\\.\\d{3})" :test #'string=)

(register-matcher :random-frequency
                  (list (match-score (lambda (input)
                                       (and (directedp input)
                                            (or (mentions "ciekaw" (unquoted-part input))
                                                (mentions "fajn" (unquoted-part input))
                                                (mentions "dawaj jakąś" (unquoted-part input)))))
                                     0.4)
                        (match-score (lambda (input)
                                       (and (directedp input)
                                            (mentions "częstotliwość" (unquoted-part input))))
                                     0.75)
                        (match-score (lambda (input)
                                       (and (directedp input)
                                            (mentions +freq-extraction-regexp+ (unquoted-part input))))
                                     -1))
                  (lambda (input)
                    (say (reply-to input) (describe-random-frequency) :to (author input))))

(register-matcher :whats-the-frequency
                  (list (match-score (lambda (input)
                                       (and (directedp input)
                                            (or (mentions "co to" (unquoted-part input))
                                                (mentions "jaka" (unquoted-part input))
                                                (mentions "co jest" (unquoted-part input)))))
                                     0.5)
                        (match-score (lambda (input)
                                       (and (directedp input)
                                            (mentions "częstotliwość" (unquoted-part input))))
                                     0.7)
                        (match-score (lambda (input)
                                       (and (directedp input)
                                            (mentions +freq-extraction-regexp+ (unquoted-part input))))
                                     1.25))
                  (lambda (input)
                    (say (reply-to input) (describe-frequency (extract-frequency (unquoted-part input))) :to (author input))))

(provide-output :unknown-frequency '("Nie znam tej częstotliwości."
                                     "Nie kojarzę..."
                                     "Nie wiem."
                                     "No clue."))

(defun extract-frequency (text)
  (cl-ppcre:scan-to-strings +freq-extraction-regexp+ text))

(defun describe-frequency (freq)
  (if-let ((freq-data (assoc freq *frequencies* :test #'string=)))
    (cdr freq-data)
    :unknown-frequency))

(defun describe-random-frequency ()
  (let ((freq (random-elt *frequencies*)))
    (concatenate 'string (car freq) " → " (cdr freq))))
