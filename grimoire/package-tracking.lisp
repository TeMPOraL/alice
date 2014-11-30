(in-package #:alice)

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
