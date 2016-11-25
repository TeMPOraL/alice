(in-package #:alice)

(defparameter *user-notification-medium* (make-hash-table :test 'equalp))

(defvar *memos* (make-hash-table :test 'equalp) "List of memos, i.e. notifications to be delivered on IRC.")
(defvar *delayed-notifications* '() "List for delayed notifications, to be checked every now and then.")

(defvar *delayed-notifications-timer* (trivial-timers:make-timer (lambda () (funcall 'alice::check-for-delayed-notifications))))

(defclass memo ()
  ((server :initarg :server
           :initform nil
           ;; :type string
           :accessor server
           :documentation "Server for which this memo is relevant. Currently unused.")
   
   (channel :initarg :channel
            :initform nil
            :accessor channel
            :documentation "Channel this memo originated from (or `NIL' if it was a private message (i.e. query).")
   
   (recipient :initarg :recipient
              :initform nil
              :accessor recipient)      ;NOTE in the future, this will be some sort of weak reference to an user object.
   
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
                       ;; :type local-time:timestamp
                       :accessor deliver-after-time)))

(defgeneric immediatep (memo)
  (:documentation "Test if memo is for immediate delivery."))

(defgeneric overduep (memo &optional current-time)
  (:documentation "Test if memo should be delivred now (it's past delivery time)."))

(defgeneric deliver (memo)
  (:documentation "Deliver the memo."))

(defgeneric to-spoken-string (what)
  (:documentation "Converts object into spoken representation."))

(defmethod privatep ((memo memo))
  (null (channel memo)))

(defmethod immediatep ((memo memo))
  (null (deliver-after-time memo)))

(defmethod overduep ((memo memo) &optional (current-time (local-time:now)))
  (and (not (immediatep memo))
       (local-time:timestamp>= current-time (deliver-after-time memo))))

(defmethod deliver ((memo memo))
  (funcall (pick-notifier (recipient memo) 'dispatch-memo-to-IRC)
           memo)
  (dump-memos))

(defmethod to-spoken-string ((memo memo))
  (format nil "Wiadomość od ~A nadana ~A o ~A ⇒ ~A" (author memo) (format-date (send-time memo)) (format-time (send-time memo)) (text memo)))

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

(defmethod initialize-instance :after ((memo memo) &key)
  ;; Fix strings of invalid specific type that interfere with serialization
  ;; FIXME This is a big HACK because I don't understand how the type poisoning occur; also probably failure of cl-marshal to serialize type (VECTOR CHARACTER n) is a bug in cl-marshal itself.
  ;; cf. https://github.com/wlbr/cl-marshal/issues/6
  (setf (recipient memo) (format nil "~A" (recipient memo))
        (author memo) (format nil "~A" (author memo))
        (text memo) (format nil "~A" (text memo))))

(defmethod marshal:class-persistant-slots ((memo memo))
  (mapcar #'closer-mop:slot-definition-name (closer-mop:class-direct-slots (class-of memo))))

;;; INPUT MATCHERS
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
                                                         (identify-person-mentioned (unquoted-part input) (author input))
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
                                            nil;; TODO add conditions based on timestrings
                                            ))
                                     0.5))
                  (lambda (input)
                    (let ((delivery-time (extract-target-timestamp-from-input input)))
                      (if delivery-time
                          (say (reply-to input) (notify-person-delayed (reply-to input)
                                                                       (identify-person-mentioned (unquoted-part input) (author input))
                                                                       (raw-text input)
                                                                       (author input)
                                                                       (privatep input)
                                                                       delivery-time))
                          (say (reply-to input) :failed-to-extract-timestring)))))

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
                              "przekażę jak się odezwie"
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

(provide-output :failed-to-extract-timestring '("Nie bardzo umiem powiedzieć kiedy..."
                                                "Napisz mi precyzyjniej, kiedy."
                                                "Jeszcze raz - kiedy?"))

(provide-output :delayed-memo-saved '("Ok."
                                      "Jasne, przekażę."
                                      "Zapisane do przekazania."))

(provide-output :delayed-memo-failed '(:memo-failed))


;;; delayed notification
(defun extract-timestring (input)
  "Narrow down `INPUT' to the best timestring."
  (labels ((remove-superfluous-punctuation (string)
             (cl-ppcre:regex-replace-all "[\\.\\,\\:\\-\\?]\\s" string " "))
           (first-timestring-subsequence (tokens &optional accumulated)
             "Recursive helper function that return the first continous sequence of `TOKENS' that are recognized as timestring words."
             (cond ((emptyp tokens)
                    accumulated)

                   ((and (not (emptyp accumulated))
                         (null (chronicity:token-tags (car tokens))))
                    accumulated)
                   
                   ((not (null (chronicity:token-tags (car tokens))))
                    (first-timestring-subsequence (cdr tokens) (cons (car tokens) accumulated)))

                   (t
                    (first-timestring-subsequence (cdr tokens) accumulated)))))
   (multiple-value-bind (result first-try) (compute-time-offset-from-string (remove-superfluous-punctuation (unquoted-part input)))
     (if result
         (remove-superfluous-punctuation (unquoted-part input)) ; on the off-chance we're parsing *just* the timestring we need
         (implode (reverse (mapcar #'chronicity:token-word (first-timestring-subsequence first-try))))))))

(defun extract-target-timestamp-from-input (input)
  "Generate a proper `LOCAL-TIME:TIMESTAMP' object from `INPUT'."
  (let ((time-str (extract-timestring input)))
    (ignore-errors (compute-time-offset-from-string time-str))))

(defun make-memo (channel who what from-who &optional (timestamp (local-time:now)))
  "Create a memo object out of passed params."
  (let ((target (identify-person-canonical-name who)))
    (when target
      (make-instance 'memo
                     :channel channel
                     :author from-who
                     :recipient target
                     :text what
                     :send-time timestamp))))

(defun save-memo (memo)
  "Save a memo for user."
  (let ((memos (gethash (recipient memo) *memos*)))
    (setf (gethash (recipient memo) *memos*)
          (append memos (list memo)))
    (dump-memos)))

(defun make-memo-matcher (user destination current-time)
  (lambda (memo)
    (and (equalp destination (channel memo))
         (equalp user (recipient memo))
         (or (immediatep memo)
             (local-time:timestamp>= current-time (deliver-after-time memo))))))

(defun find-matching-memos (user destination memos current-time)
  (remove-if-not (make-memo-matcher user destination current-time)
                 memos))

(defun remove-memo (memo memos current-time) ;FIXME this should probably remove object by identity.
  (remove-if (make-memo-matcher (recipient memo) (channel memo) current-time)
             memos
             :count 1))

(defun dispatch-memo-to-IRC (memo &optional current-target morep)
  "Output `MEMO' directly to IRC - either to channel or in query. Optionally, set target user nickname to `CURRENT-TARGET', because the nickname might have changed since registering of memo."
  (declare (ignore morep))
  (let ((target (or current-target
                    (recipient memo))))
    (say (if (privatep memo)
             (recipient memo)
             (channel memo))
         (to-spoken-string memo)
         :to target)))


(defun check-for-memos (destination for-who &optional (current-time (local-time:now)))
  "See if user `FROM-WHO' writing at `DESTINATION' has any pending memos (`IMMEDIATEP' or overdue wrt. to `CURRENT-TIME') and if so, grab the first one and write it to him/her.
Also check for private memos (sent by query), and if any found, send it to him/her in private."
  (let ((who (identify-person-canonical-name for-who)))
    (labels ((handle-memos (from-where to-who)
               "Find a first matching memo, dispatch it and remove from memo list."
               (let* ((all-memos (gethash who *memos*))
                      (matching-memos (find-matching-memos who from-where all-memos current-time)) ;TODO clean that up; it's a mess of arguments.
                      )
                 (mapc (lambda (memo)
                         (setf all-memos (remove-memo memo all-memos current-time))
                         (dispatch-memo-to-IRC memo to-who (> (length matching-memos) 1)))
                       matching-memos)
                 (setf (gethash who *memos*) all-memos))))

      (handle-memos destination for-who) ;public memos
      (handle-memos nil nil))))              ;private memos


(defun notify-via-IRC-memo (memo)
  "Stores `MEMO' on the list of memos to deliver via IRC."
  (if memo
      (progn (save-memo memo)
             :memo-saved)
      :memo-failed))

;;; DELAYED NOTIFICATIONS
(defun save-delayed-memo (memo delivery-time)
  (setf (deliver-after-time memo) delivery-time
        *delayed-notifications* (append *delayed-notifications* (list memo))))

(defun check-for-delayed-notifications ()
  "To be called by a timer."
  (setf *delayed-notifications* (mapcan (lambda (notification)
                                          (if (overduep notification)
                                              (progn (deliver notification)
                                                     nil)
                                              (list notification)))
                                        *delayed-notifications*)))


;; GENERAL NOTIFICATIONS

(defun notify-person (channel target-user message-body from-who is-private)
  "Notify a person using the most suitable medium available."
  (let ((memo (make-memo (and (not is-private) channel)
                         target-user message-body from-who)))
    (funcall (pick-notifier target-user)
             memo)))

(defun notify-person-delayed (channel target-user message-body from-who is-private delivery-time)
  "Notify via a delyed memo. Memo is stored to be delivered at `DELIVERY-TIME'. The actual mechanism of delivery will be determined at that time."
  (let ((memo (make-memo (and (not is-private) channel)
                         target-user message-body from-who)))
    (if memo
        (progn (save-delayed-memo memo delivery-time)
               :delayed-memo-saved)
        :delayed-memo-failed)))

(defun pick-notifier (target-user &optional (default-method 'notify-via-IRC-memo))
  "Select notification method for given user."
  (gethash (identify-person-canonical-name target-user) *user-notification-medium* default-method))


(defun start-delayed-notification-timer ()
  (unless (trivial-timers:timer-scheduled-p *delayed-notifications-timer*)
    (trivial-timers:schedule-timer *delayed-notifications-timer* 1 :repeat-interval 30)))
(defun stop-delayed-notification-timer ()
  (when (trivial-timers:timer-scheduled-p *delayed-notifications-timer*)
    (trivial-timers:unschedule-timer *delayed-notifications-timer*)))


;; PERSISTENCE
(defun dump-memos ()
  (serialize-hashtable-to-file *memos* "memos.dat")
  (dump-list *delayed-notifications* "delayed.dat"))

(defun load-dumped-memos ()
  (awhen (deserialize-hashtable-from-file "memos.dat")
    (setf *memos* it))
  (setf *delayed-notifications* (read-back-list "delayed.dat")))
