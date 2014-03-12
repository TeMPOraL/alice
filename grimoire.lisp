;;;; Alice's Grimoire, the source of her more powerful magic.
(in-package #:alice)

(defun check-for-spelling-mistakes (message)
  (let* ((mistakes (find-if (lambda (test)
                              (not (null (cl-ppcre:all-matches (car test)
                                                               message))))
                            *spelling-tests*)))
    (if mistakes
        (cdr mistakes)
        nil)))

(defun do-google-search (query)
  (declare (ignore query))
  )

(defun shorten-url (url)
  (if url
      (or (ignore-errors (drakma::http-request "http://tinyurl.com/api-create.php"
                                               :external-format-out :UTF-8
                                               :parameters `(("url" . ,url))))
          :failed-in-shortening)
      :nothing-to-shorten))

(defun parse-message-for-url-shortening (text)
  (cl-ppcre:scan-to-strings *url-shortening-regexp* text))

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
        (or (ignore-errors (clean-up (xml-response-to-speechstrings (get-xml-response query))))
            :failed-in-computing)
        :nothing-to-compute)))

(defun parse-message-for-wolfram-computation (text)
  (cl-ppcre:scan-to-strings *wolfram-query-regexp* text))

(defun send-notification (what &optional (from ""))
  (or (ignore-errors (drakma:http-request "https://api.pushover.net/1/messages.json"
                                          :method :post
                                          :external-format-out :UTF-8
                                          :parameters `(("token" . ,*pushover-token*)
                                                        ("user" . ,*pushover-user*)
                                                        ("title" . ,*full-name*)
                                                        ("message" . ,(concatenate 'string "<" from "> " what)))
                                          :content "hack"
                                          :content-length 4))
      :failed-in-sending-notification))


(defun send-email (where-to text)
  (or (ignore-errors (drakma:http-request (concatenate 'string "https://api.mailgun.net/v2/" *mailgun-domain* "/messages")
                                          :method :post
                                          :basic-authorization `("api" ,*mailgun-key*)
                                          :parameters `(("from" . ,(concatenate 'string "Alice Margatroid <alice.margatroid@" *mailgun-domain* ">"))
                                                        ("to" . ,where-to)
                                                        ("subject" . "Alice Margatroid here; got a notification for you.")
                                                        ("text" . ,text))
                                          :external-format-out :UTF-8))
      :failed-in-sending-notification))


;; MEMOS
;; FIXME move this somewhere?
(defvar *memos* (make-hash-table :test 'equalp))

(defun make-memo (channel who what from-who)
  )

(defun memo-to-string (memo)
  )

(defun save-memo (channel who what from-who)
  "Save a memo for user."
  
  nil)

(defun find-first-memo (destination memos)
  ;; FIXME TODO if `DESTINATION' is nil, find first global memo; otherwise find first memo that has destination component either nil or equal to `DESTINATION'.
  (declare (ignore destination))
  (values (first memos) (rest memos)))

(defun check-for-memos (destination from-who)
  "See if user `FROM-WHO' writing at `DESTINATION' has any pending memos and if so, grab the first one and write it to him/her."
  (let* ((who (identify-person-canonical-name from-who))
         (memos (gethash who *memos*)))
    (if (not (null memos))
        (multiple-value-bind (memo remaining-memos) (find-first-memo destination memos)
          (setf (gethash who *memos*) remaining-memos)
          (say destination memo :to from-who)
          (if (not (null remaining-memos))
              (say destination :more-memos :to from-who))))))

;; GENERAL NOTIFICATIONS

(defun notify-person (channel who what from-who is-global)
  "Notify a person using the most suitable medium available."
  (apply (pick-notifier channel who is-global)
         who what from-who))

(defun pick-notifier (channel who is-global)
  "Select notification method for given user."
  (gethash who *user-notification-medium* (lambda (who what from-who)
                                            (save-memo (and is-global channel)
                                                       who what from-who))))

