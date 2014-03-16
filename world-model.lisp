;;;; Alice (currently) lives in the IRC realm; here she keeps her model in her mind
;;;; of the IRC reality she is in.

(in-package #:alice)

(defvar *connected-channels* '() "Channels the bot is currently at.")

(defun clear-nonpersistent-worldstate ()
  "Cleans up the world state when (re)connecting the bot."
  (setf *connected-channels* '()))

(defun load-persistent-world-model-data ()
  nil)                                  ;one day we'll be loading data from "memory" after restart

;; names tracking
(defun store-joining-name (channel name)
  (declare (ignore channel))
  (remember-seen-nick (strip-nick-mode-chars name)))

(defun store-parting-name (channel name)
  (declare (ignore channel))
  (remember-seen-nick (strip-nick-mode-chars name)))

(defun store-names (channel names)
  (declare (ignore channel))
  (map nil (lambda (name) (remember-seen-nick (strip-nick-mode-chars name))) names))

(defun strip-nick-mode-chars (name)
  (cl-ppcre:regex-replace "^[@\+]" name ""))

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

;;; CANONICAL NAME STUFF (MOSTLY TEST / TEMPORARY)

(defvar *canonical-nicks* (make-hash-table :test 'equalp))

(defun known-nick (nick)
  (not (null (gethash nick *canonical-nicks*))))

(defun learn-canonical-name (nick canonical-name)
  (setf (gethash nick *canonical-nicks*) canonical-name))

(defun remember-seen-nick (nick)
  (if (not (known-nick nick))
      (setf (gethash nick *canonical-nicks*) nick)))


(defun find-canonical-entry-with-stem-matching (name)
  (with-hash-table-iterator (generator *canonical-nicks*)
    (loop (multiple-value-bind (more? key value) (generator)
            (unless more? (return nil))
            (if (stem-matches-p name key)
                (return key))))))

;; resolving people from free-form text
;; TODO replace current code with proper references to sentece-features when the latter are done.
(defun identify-person-mentioned (message-body)
  "Take `MESSAGE-BODY', return canonical name of a first recognized person inside the message."
  (let ((words (extract-words message-body)))
    (find-if (lambda (word)
               (and (not (equalp word *nick*))
                    (not (null (identify-person-canonical-name word)))))
             words)))

;; Canonical names

(defun identify-person-canonical-name (alias)
  "Identifies a person's canonical name given it's alias - it can be an IRC nick or other registered way for referring to that person."
  (let ((name (find-canonical-entry-with-stem-matching alias)))
    (gethash name *canonical-nicks*)))

