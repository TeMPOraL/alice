;;;; Alice (currently) lives in the IRC realm; here she keeps her model in her mind
;;;; of the IRC reality she is in.

(in-package :alice)

(defvar *connected-channels* '() "Channels the bot is currently at.")

(defun clear-nonpersistent-worldstate ()
  "Cleans up the world state when (re)connecting the bot."
  (setf *connected-channels* '()))

(defun store-joining-name (channel name)
  (say "TeMPOraL" (concatenate 'string "JOINING: " name " (" channel ")")))

(defun store-parting-name (channel name)
  (say "TeMPOraL" (concatenate 'string "LEAVING: " name " (" channel ")")))

(defun store-names (channel names)
  (say "TeMPOraL" (concatenate 'string "NAMES: " names " (" channel ")")))

(defun join-channel (channel &key password)
  (join *connection* channel :password password)
  (pushnew channel *connected-channels* :test #'string=))

(defun part-channel (channel)
  (part *connection* channel)
  (setf *connected-channels* (remove-if (lambda (x) (string= x channel)) *connected-channels*)))
  
