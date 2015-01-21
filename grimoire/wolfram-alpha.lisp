(in-package #:alice)

(defparameter *wolfram-app-id* "")

(defparameter *wolfram-query-regexp* "\"(.*)\"" "A regexp to extract question part when performing Wolfram|Alpha search.")

(register-matcher :wolfram-alpha-query
                  (list (match-score (lambda (input)
                                       (and (directedp input)
                                              (or (mentions "licz" (raw-text input))
                                                  (mentions "compute" (raw-text input)))))))
                  (lambda (input)
                    (say (reply-to input) (do-wolfram-computation (parse-message-for-wolfram-computation (raw-text input))))))

(provide-output :wolfram-turned-off '("Skoro nie chcecie, żebym cokolwiek liczyła to o to nie proście."
                                      "Takiego wała."
                                      "http://wolframalpha.com, samemu sobie policz."))

(provide-output :nothing-to-compute '("Ale co mam obliczyć? Umieść to w cudzysłowiach."
                                      "Co mam przeliczyć? Umieść to w cudzysłowiach."))

(provide-output :failed-in-computing '("Nie umiem w Wolframa *sob*"
                                       "Musisz zawsze pytać o takie dziwne rzeczy? *sigh*"
                                       "Sorry, nie wyszło. *sigh*"))

(provide-output :nothing-computed '("Nic się nie policzyło :(."
                                    "Brak wyniku; spytaj o coś innego."
                                    "Nope, nic nie ma."
                                    "Nie pykło."))



(defun do-wolfram-computation (query)
  (flet ((xml-response-to-speechstrings (xml)
           (coerce (alexandria:flatten (map 'list
                                            (lambda (el)
                                              (let ((val (dom:first-child el)))
                                                (when val
                                                  (split-sequence:split-sequence #\Newline (dom:data val)))))
                                            (dom:get-elements-by-tag-name xml "plaintext")))
                   'vector))

         (get-xml-response (query)
           (let ((response (drakma:http-request "http://api.wolframalpha.com/v2/query"
                                                :external-format-out :UTF-8
                                                :parameters `(("appid" . ,*wolfram-app-id*)
                                                              ("input" . ,query)
                                                              ("format" . "plaintext")))))
             (cxml:parse-rod response
                             (cxml-dom:make-dom-builder))))
         (clean-up (response)
           (let ((cleaned-up (remove nil response)))
             (if (= (length cleaned-up) 0)
                 :nothing-computed
                 cleaned-up))))

    ;; code
    (if query
        (or (ignore-errors (clean-up (xml-response-to-speechstrings (get-xml-response query))))
            :failed-in-computing)
        :nothing-to-compute)))

(defun parse-message-for-wolfram-computation (text)
  (cl-ppcre:scan-to-strings *wolfram-query-regexp* text))
