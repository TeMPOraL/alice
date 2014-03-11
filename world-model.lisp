;;;; Alice (currently) lives in the IRC realm; here she keeps her model in her mind
;;;; of the IRC reality she is in.

(in-package #:alice)

(defvar *connected-channels* '() "Channels the bot is currently at.")

(defvar *memos* (make-hash-table :test 'equalp))

(defun clear-nonpersistent-worldstate ()
  "Cleans up the world state when (re)connecting the bot."
  (setf *connected-channels* '()))

(defun load-persistent-world-model-data ()
  nil)                                  ;one day we'll be loading data from "memory" after restart

;; names tracking
(defun store-joining-name (channel name)
  (declare (ignore channel name))
  nil)

(defun store-parting-name (channel name)
  (declare (ignore channel name))
  nil) 

(defun store-names (channel names)
  (declare (ignore channel names))
  nil)

;; channel tracking
(defun join-channel (channel &key password)
  (irc:join *connection* channel :password password)
  (pushnew channel *connected-channels* :test #'string=))

(defun part-channel (channel)
  (irc:part *connection* channel)
  (setf *connected-channels* (remove-if (lambda (x) (string= x channel)) *connected-channels*)))
  
;; people tracking
(defclass person ()
  ((real-names
    :initform '())

   (irc-names
    :initform '())

   (remote-communication-options
    :initform '())))

(defmethod print-object ((object person) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (real-names irc-names) object
      (format stream "~A (~A)"
              (if real-names (first real-names) "JOHN DOLL")
              (if irc-names (first irc-names) "N/A")))))

(defun get-person-from-nickname (nickname)
  "Return the PERSON object that is Alice's best guess about who is reffered to by NICKNAME at the moment.
Creates the object if not found."
  (let ((new-guy (make-instance 'person))) ;FIXME
    (setf (slot-value new-guy 'irc-names) (list nickname))
    new-guy))


(defun register-nick-change (from to)
  nil)


;; people tracking - skeletal
(defun identify-person-canonical-name (alias)
  "Identifies a person's canonical name given it's alias - it can be an IRC nick or other registered way for referring to that person."
  alias)                                ;temporary



(defun save-memo (channel who what from-who)
  "Save a memo for user."
  ;; append a memo
  nil)
