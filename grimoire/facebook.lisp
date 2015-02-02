(in-package #:alice)

(defclass page ()
  ((page-id)
   (access-key)
   (forward-messages-to)))

;;; Transporting messages between Facebook page and info.

;;; outputs


(defun post-to-facebook (text)
  ;; TODO POST to /me/feed
  ;; TODO choose between links&stuff?
  )
