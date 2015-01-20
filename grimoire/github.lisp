(in-package #:alice)

(register-matcher :github-issues-link
                  (list (match-score (lambda (input)
                                       (and (directedp input)
                                            (mentions-regexp "issues" (raw-text input))))))
                  (lambda (input) (say (reply-to input) :issues-link :to (author input))))

(register-matcher :add-github-issue
                  (list (match-score (lambda (input)
                                       (and (directedp input)
                                            (mentions-regexp "(dodaj|pisz)" (raw-text input))
                                            (mentions-regexp "issue" (raw-text input)))))) ;NOTE this will conflict with notifications unless matches/scores are tweaked. try, e.g., looking for nicknames and reducing score if one is found. also, word "issue" close by is a strong indicator, and so is text in quotes (lack of it is a minus-alot indicator).
                  (lambda (input)
                    (say (reply-to input) (open-github-issue (author input) (extract-issue-description (raw-text input))) :to (author input))))

(defun extract-issue-description (text)
  (cl-ppcre:scan-to-strings *issue-description-regexp* text))

(defun open-github-issue (who what)
  "Open an issue on request by `WHO' with description `WHAT'."
  (if what
      (let* ((result (ignore-errors (drakma:http-request "https://api.github.com/repos/TeMPOraL/alice/issues"
                                                         :method :post
                                                         :external-format-out :UTF-8
                                                         :external-format-in :UTF-8
                                                         :content (json:encode-json-to-string `((:title . ,(concatenate 'string who " → " what))))
                                                         :content-type "application/json"
                                                         :additional-headers `(("Authorization" . ,(format nil "token ~A" *github-token*))))))
             (parsed-result (if (and result
                                     (not (stringp result)))
                                (map 'string #'code-char result)
                                result))
             (url (cdr (assoc :html--url (json:decode-json-from-string parsed-result)))))
        (if url
            url
            :failed-to-add-issue))
      :no-issue-to-add))

