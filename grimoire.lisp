;;;; Alice's Grimoire, the source of her more powerful magic.
(in-package #:alice)

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

(defun extract-urls-from-message (message-body)
  (remove nil (mapcar (lambda (str)(cl-ppcre::scan-to-strings *url-regexp* str))
                      (split-sequence:split-sequence #\Space message-body))))

(defun parse-message-for-url-shortening (text)
  (cl-ppcre:scan-to-strings *url-shortening-regexp* text))

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

(defun send-notification (what to-token from)
  (if (ignore-errors (drakma:http-request "https://api.pushover.net/1/messages.json"
                                          :method :post
                                          :external-format-out :UTF-8
                                          :parameters `(("token" . ,*pushover-token*)
                                                        ("user" . ,to-token)
                                                        ("title" . ,*full-name*)
                                                        ("message" . ,(concatenate 'string "<" from "> " what)))
                                          :content "hack"
                                          :content-length 4))
      :notification-sent
      :failed-in-sending-notification))


(defun send-email (where-to text)
  (if (ignore-errors (drakma:http-request (concatenate 'string "https://api.mailgun.net/v2/" *mailgun-domain* "/messages")
                                          :method :post
                                          :basic-authorization `("api" ,*mailgun-key*)
                                          :parameters `(("from" . ,(concatenate 'string "Alice Margatroid <alice.margatroid@" *mailgun-domain* ">"))
                                                        ("to" . ,where-to)
                                                        ("subject" . "Alice Margatroid here; got a notification for you.")
                                                        ("text" . ,text))
                                          :external-format-out :UTF-8))
      ;; 
      :notification-sent
      :failed-in-sending-notification))

(defvar +timestring-format+ '((:DAY 2) #\. (:MONTH 2) #\. (:YEAR 4)  #\  (:HOUR 2) #\: (:MIN 2)
 #\: (:SEC 2)))

;; time utils
(defun make-timestamp ()
  (local-time:format-timestring nil (local-time:now) :format +timestring-format+))

;; MEMOS
;; FIXME move this somewhere?
(defvar *memos* (make-hash-table :test 'equalp))

(defun make-memo (channel who what from-who)
  (let ((target (identify-person-canonical-name who)))
     (when target (list channel (identify-person-canonical-name who) what from-who (local-time:now)))))

(defun memo-to-string (memo)
  (format nil "~A ma dla Ciebie wiadomość z ~A ⇒ ~A" (fourth memo) (local-time:format-timestring nil (fifth memo) :format +timestring-format+) (third memo)))

(defun save-memo (memo)
  "Save a memo for user."
  (let ((memos (gethash (second memo) *memos*)))
    (setf (gethash (second memo) *memos*)
          (append memos (list memo)))))

(defun make-memo-matcher (user destination)
  (lambda (memo)
    (and (equalp destination (first memo))
         (equalp user (second memo)))))

(defun find-matching-memos (user destination memos)
  (remove-if-not (make-memo-matcher user destination)
                 memos))

(defun remove-memo (memo memos)
  (remove-if (make-memo-matcher (second memo) (first memo))
             memos
             :count 1))

(defun check-for-memos (destination for-who)
  "See if user `FROM-WHO' writing at `DESTINATION' has any pending memos and if so, grab the first one and write it to him/her.
Also check for private memos (sent by query), and if any found, send it to him/her in private."
  (let ((who (identify-person-canonical-name for-who)))
    (labels ((dispatch-memo (to-where to-who memo more?)
               (say to-where (memo-to-string memo) :to to-who)
               (when more?
                 (say to-where :more-memos :to to-who)))

             (handle-memos (from-where to-where to-who)
               "Find a first matching memo, dispatch it and remove from memo list."
               (let* ((all-memos (gethash who *memos*))
                      (matching-memos (find-matching-memos who from-where all-memos))
                      (memo (first matching-memos)))
                 (when memo
                   (setf (gethash who *memos*) (remove-memo memo all-memos))
                   (dispatch-memo to-where to-who memo (> (length matching-memos) 1))))))

      (handle-memos destination destination for-who) ;public memos
      (handle-memos nil for-who nil))))              ;private memos


(defun notify-via-memo (channel who what from-who is-private)
  (let ((memo (make-memo (and (not is-private) channel)
                         who what from-who)))
    (if memo
        (progn (save-memo memo)
               :memo-saved)
        :memo-failed)))

(defun make-pushover-notifier (pushover-key)
  (lambda (channel who what from-who is-private)
    (declare (ignore channel who is-private))
    (send-notification what pushover-key from-who)))

(defun make-email-notifier (email)
  (lambda (channel who what from-who is-private)
    (declare (ignore channel who from-who is-private))
    (send-email email what)))

;; GENERAL NOTIFICATIONS

(defun notify-person (channel target-user message-body from-who is-private)
  "Notify a person using the most suitable medium available."
  (funcall (pick-notifier channel target-user message-body from-who is-private)
           channel target-user message-body from-who is-private))

(defun pick-notifier (channel target-user message-body from-who is-private)
  "Select notification method for given user."
  (gethash (identify-person-canonical-name target-user) *user-notification-medium* #'notify-via-memo))
