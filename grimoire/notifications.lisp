(in-package #:alice)

(defparameter *pushover-token* "")
(defparameter *pushover-admin-user* "")

(defparameter *mailgun-domain* "")
(defparameter *mailgun-key* "")

(defparameter *user-notification-medium* (make-hash-table :test 'equalp))

(register-matcher :notify-user
                  (list (match-score (lambda (input)
                                       (and (directedp input)
                                            (or (mentions "zawiadom" (unquoted-part input))
                                                (mentions "powiadom" (unquoted-part input))
                                                (mentions "przeka" (unquoted-part input))
                                                (mentions "pingnij" (unquoted-part input))
                                                (mentions "podrzuć" (unquoted-part input))
                                                (mentions "zapyta" (unquoted-part input))
                                                (mentions "spyta" (unquoted-part input))
                                                (mentions "powiedz" (unquoted-part input))
                                                (mentions "memo" (unquoted-part input))))))) ;NOTE, this will conflict with gihtub; try e.g. looking for nicknames and adding additional score if one found
                  (lambda (input)
                    (say (reply-to input) (notify-person (reply-to input)
                                                         (identify-person-mentioned (unquoted-part input))
                                                         (raw-text input)
                                                         (author input)
                                                         (privatep input)))))

(register-matcher :delayed-notify-user
                  (list (match-score (lambda (input)
                                       (and (directedp input)
                                            (or (mentions "przypomnij")
                                                (mentions "remind")
                                                ;; TODO add conditions based on timestrings
                                                )))))
                  (lambda (input)
                    ...))

(provide-output :more-memos '("Są też kolejne powiadomienia."
                              "Są kolejne mema! :)"
                              "Mam Ci coś więcej do przekazania."
                              "Shanghai mówi, że jest do Ciebie więcej powiadomień."
                              "Shanghai przyniosła więcej powiadomień."))

(provide-output :memo-saved '("zapisałam jako memo"
                              "memo zapisane"
                              "zapisane; przekażę jak zobaczę"
                              "jasne, przekażę jak zobaczę"))

(provide-output :memo-failed  '("Nie umiem wysłać tego memo. Chyba nie wiem o kogo Ci chodzi."
                                "Nie wiem komu co mamy wysłać."
                                "Shanghai wróciła i mówi, że nie wie komu przekazać..."))

(provide-output :notification-sent '("ok, przekazałam"
                                     "jasne, przekazane"
                                     "sure, już przekazuję"
                                     "Shanghai posłana z wiadomością"
                                     "przekazane"))

(provide-output :failed-in-sending-notification '("Coś się spsuło :(."
                                                  "Coś nie działa. *sigh*"
                                                  "Nie umiem w notyfikacje. *sob*"))


;;; notifications

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

;; MEMOS
;; FIXME move this somewhere?
(defvar *memos* (make-hash-table :test 'equalp))

(defun make-memo (channel who what from-who &optional (timestamp (local-time:now)))
  (let ((target (identify-person-canonical-name who)))
     (when target (list channel (identify-person-canonical-name who) what from-who timestamp))))

(defun memo-to-string (memo)
  (format nil "Wiadomość od ~A nadana ~A o ~A ⇒ ~A" (fourth memo) (format-date (fifth memo)) (format-time (fifth memo)) (third memo)))

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
