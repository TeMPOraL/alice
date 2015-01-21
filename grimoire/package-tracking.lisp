(in-package #:alice)

(defparameter *gdziepaczka-token* "")
(defparameter *tracking-number-regexp* "([A-Z]{2}[0-9]+[A-Z]{2})")

(register-matcher :track-package
                  (list (match-score (lambda (input)
                                       (and (directedp input)
                                            (or (mentions "gdzie jest" (raw-text input))
                                                (mentions "śledź" (raw-text input))
                                                (mentions "track" (raw-text input)))))))
                  (lambda (input)
                    (say (reply-to input) (track-package (parse-message-for-package-tracking-number (raw-text input))))))

(provide-output :failed-to-track-package '("Nie pykło :(."
                                            "Nie udało mi sie sprawdzić :(."
                                            "Coś nie działa."))

(provide-output :no-package-to-track '("Podaj poprawny numer trackingowy paczki. Póki co, umiem śledzić tylko te w bazie Poczty Polskiej."))

(defun parse-message-for-package-tracking-number (text)
  (cl-ppcre:scan-to-strings *tracking-number-regexp* text))

(defun track-package (package-id)
  (if package-id
      (let ((result (json:decode-json-from-string (ignore-errors (fix-drakma-output (drakma:http-request "http://api.gdziepaczka.pl"
                                                                                                         :external-format-out :UTF-8
                                                                                                         :parameters `(("mode" . "package")
                                                                                                                       ("courier" . "pocztapolska")
                                                                                                                       ("package" . ,package-id)
                                                                                                                       ("token" . ,*gdziepaczka-token*)
                                                                                                                       ("output" . "json")
                                                                                                                       ("detailed" . "false"))))))))
        (if result
            (let* ((last-step (car (last (cdadr (assoc :steps result)))))
                   (date (assoc :date last-step))
                   (time (assoc :time last-step))
                   (location (assoc :place last-step))
                   (status (assoc :status last-step))
                   (uri (assoc :uri result)))
              (format nil "~A ~A @~A: ~A. (szczegóły: ~A)." (cdr date) (cdr time) (cdr location) (cdr status) (cdr uri)))
            :failed-to-track-package))
      :no-package-to-track))
