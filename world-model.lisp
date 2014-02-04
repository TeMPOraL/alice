(in-package :alice)

(defun store-joining-name (channel name)
  (say "TeMPOraL" (concatenate 'string "JOINING: " name " (" channel ")")))

(defun store-parting-name (channel name)
  (say "TeMPOraL" (concatenate 'string "LEAVING: " name " (" channel ")")))

(defun store-names (channel names)
  (say "TeMPOraL" (concatenate 'string "NAMES: " names " (" channel ")")))

