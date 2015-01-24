(in-package #:alice)

(defparameter *url-regexp* "((^https?\\:.*)|(www\\..*))")
(defparameter *url-shortening-regexp* "(http.*)")

(register-matcher :shorten-url
                  (list (match-score (lambda (input)
                                       (and (directedp input)
                                            (or (mentions "skró" (unquoted-part input))
                                                (mentions "skracaj" (unquoted-part input)))))))
                  (lambda (input)
                    (say (reply-to input) (shorten-url (parse-message-for-url-shortening (unquoted-part input))))))

(provide-output :nothing-to-shorten  #(("Jak mi nie powiesz co, to nic nie skrócę."
                                        "Ta, a niby co?"
                                        "*sigh*")
                                       ("pro tip: URLe do skrócenia zaczynają się od http://"
                                        "Jak mi napiszesz ładny URL zaczynający się od http:// to pomyślimy.")))

(provide-output :failed-in-shortening '("Nie skróciło się... *sigh*"
                                        "http://cos.nie.dzia.la :/"
                                        "Nie umiem w skracanie linków. *sob*"))

(defun shorten-url (url)
  (if url
      (or (ignore-errors (drakma::http-request "http://tinyurl.com/api-create.php"
                                               :external-format-out :UTF-8
                                               :parameters `(("url" . ,url))))
          :failed-in-shortening)
      :nothing-to-shorten))

(defun extract-urls-from-message (message-body)
  (remove nil (mapcar (lambda (str)(cl-ppcre::scan-to-strings *url-regexp* str))
                      (split-sequence:split-sequence #\Space message-body))))

(defun parse-message-for-url-shortening (text)
  (cl-ppcre:scan-to-strings *url-shortening-regexp* text))
