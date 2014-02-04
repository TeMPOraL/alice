;;;; Alice's Grimoire, the source of her more powerful magic.
(in-package #:alice)

(defun do-google-search (query)
  (declare (ignore query))
  )

(defun do-wolfram-computation (query)
  (flet ((xml-response-to-speechstrings (xml)
           (coerce (alexandria:flatten (map 'list
                                            (lambda (el)
                                              (let ((val (dom:first-child el)))
                                                (if val
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
        (clean-up (xml-response-to-speechstrings (get-xml-response query)))
        :nothing-to-compute)))

(defun parse-message-for-wolfram-computation (text)
  (cl-ppcre:scan-to-strings *wolfram-query-regexp* text))

(defun send-notification (what &optional (from ""))
  (drakma:http-request "https://api.pushover.net/1/messages.json"
                       :method :post
                       :external-format-out :UTF-8
                       :parameters `(("token" . ,*pushover-token*)
                                     ("user" . ,*pushover-user*)
                                     ("title" . ,*full-name*)
                                     ("message" . ,(concatenate 'string "<" from "> " what)))
                       :content "hack"
                       :content-length 4))

