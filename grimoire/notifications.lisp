(in-package #:alice)

(defparameter *user-notification-medium* (make-hash-table :test 'equalp))

(defvar *memos* (make-hash-table :test 'equalp))

(defclass memo ()
  ((server :initarg :server
           :initform nil
           :type string
           :accessor server
           :documentation "Server for which this memo is relevant. Currently unused.")
   
   (channel :initarg :channel
            :initform nil
            :type string
            :accessor channel
            :documentation "Channel this memo originated from (or `NIL' if it was a private message (i.e. query).")
   
   (recipient :initarg :recipient
              :initform nil
              :accessor recipient)
   
   (author :initarg :author
           :initform nil
           :accessor author)
   
   (text :initarg :text
         :initform nil
         :accessor text)
   
   (send-time :initarg :send-time
              :initform (local-time:now)
              :type local-time:timestamp
              :accessor send-time)
   
   (deliver-after-time :initarg :deliver-after-time
                       :initform nil
                       :type local-time:timestamp
                       :accessor deliver-after-time)))

(defgeneric immediatep (memo)
  (:documentation "Test if memo is for immediate delivery."))

(defmethod immediatep ((memo memo))
  (null (deliver-after-time memo)))

(defmethod print-object ((memo memo) stream)
  (print-unreadable-object (memo stream :type t :identity t)
    (with-slots (channel recipient author send-time deliver-after-time) memo
      (format stream
              "~A@~A ◷~A ✉~A → ~A"
              author
              channel
              (local-time:format-rfc1123-timestring nil send-time)
              (if deliver-after-time
                  (local-time:format-rfc1123-timestring nil deliver-after-time)
                  "IMMEDIATE")
              recipient))))

(defmethod marshal:class-persistant-slots ((memo memo))
  (mapcar #'closer-mop:slot-definition-name (closer-mop:class-direct-slots (class-of memo))))

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
                                            (or (mentions "przypomnij" (unquoted-part input))
                                                (mentions "remind" (unquoted-part input)))))
                                     1.5
                                     -0.5)
                        (match-score (lambda (input)
                                       (and (directedp input)
                                            ;; TODO add conditions based on timestrings
                                            ))
                                     0.5))
                  (lambda (input)
                    (declare (ignore input))
                    ;; TODO isolate and parse timestring and set up a delayed memo
                    ))
;;; TODO some remote memo management solutions

(provide-output :more-memos '("Są też kolejne powiadomienia."
                              "Mam dla Ciebie więcej wiadomości!"
                              "Ale to nie wszystko!"
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
                                "Adresat nieznany. *sigh*"
                                "Shanghai wróciła i mówi, że nie wie komu przekazać..."))

(provide-output :notification-sent '("ok, przekazałam"
                                     "jasne, przekazane"
                                     "sure, już przekazuję"
                                     "Shanghai posłana z wiadomością"
                                     "przekazane!"))

(provide-output :failed-in-sending-notification '("Coś się spsuło :(."
                                                  "Coś nie działa. *sigh*"
                                                  "Nie umiem w notyfikacje. *sob*"))


;;; delayed notification
(defun extract-target-timestamp-from-input (input)
  (let ((time-str ""))
   (ignore-errors (compute-time-offset-from-string time-str))))

(defun make-memo (channel who what from-who &optional (timestamp (local-time:now)))
  (let ((target (identify-person-canonical-name who)))
    (when target
      (make-instance 'memo
                     :channel channel
                     :author from-who
                     :recipient target
                     :text what
                     :send-time timestamp))))

(defun memo-to-string (memo)
  (format nil "Wiadomość od ~A nadana ~A o ~A ⇒ ~A" (author memo) (format-date (send-time memo)) (format-time (send-time memo)) (text memo)))

(defun save-memo (memo)
  "Save a memo for user."
  (let ((memos (gethash (recipient memo) *memos*)))
    (setf (gethash (recipient memo) *memos*)
          (append memos (list memo)))))

(defun make-memo-matcher (user destination current-time)
  (lambda (memo)
    (and (equalp destination (channel memo))
         (equalp user (recipient memo))
         (or (immediatep memo)
             (local-time:timestamp>= current-time (deliver-after-time memo))))))

(defun find-matching-memos (user destination memos current-time)
  (remove-if-not (make-memo-matcher user destination current-time)
                 memos))

(defun remove-memo (memo memos current-time)
  (remove-if (make-memo-matcher (recipient memo) (channel memo) current-time)
             memos
             :count 1))

(defun check-for-memos (destination for-who &optional (current-time (local-time:now)))
  "See if user `FROM-WHO' writing at `DESTINATION' has any pending memos (`IMMEDIATEP' or overdue wrt. to `CURRENT-TIME') and if so, grab the first one and write it to him/her.
Also check for private memos (sent by query), and if any found, send it to him/her in private."
  (let ((who (identify-person-canonical-name for-who)))
    (labels ((dispatch-memo (to-where to-who memo more?)
               (say to-where (memo-to-string memo) :to to-who)
               (when more?
                 (say to-where :more-memos :to to-who)))

             (handle-memos (from-where to-where to-who)
               "Find a first matching memo, dispatch it and remove from memo list."
               (let* ((all-memos (gethash who *memos*))
                      (matching-memos (find-matching-memos who from-where all-memos current-time)) ;TODO clean that up; it's a mess of arguments.
                      (memo (first matching-memos)))
                 (when memo
                   (setf (gethash who *memos*) (remove-memo memo all-memos current-time))
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

;; GENERAL NOTIFICATIONS

(defun notify-person (channel target-user message-body from-who is-private)
  "Notify a person using the most suitable medium available."
  (funcall (pick-notifier target-user)
           channel target-user message-body from-who is-private))

(defun pick-notifier (target-user)
  "Select notification method for given user."
  (gethash (identify-person-canonical-name target-user) *user-notification-medium* 'notify-via-memo))

