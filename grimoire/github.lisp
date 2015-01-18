(in-package #:alice)

;; (define-input-match :github-open-issue ((matches "(dodaj|pisz)" (raw-text *sentence*))
;;                                         (matches "issue" (raw-text *sentence*)))
;;   (output :github-issue (open-github-issue (from-who *sentence*)
;;                                            (extract-issue-description (raw-text *sentence*)))) ;how to handle errors?
;;   )

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

