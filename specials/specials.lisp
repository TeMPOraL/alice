;; A file to separate out special situations
(in-package :alice)

(defun handle-specials (destination is-private is-public is-directed from-who message-body) 
  ;; (handle-blueline destination is-private is-public is-directed from-who message-body)
  (handle-comments destination is-private is-public is-directed from-who message-body)
  (handle-standard-answers destination is-private is-public is-directed from-who message-body)
  (handle-general-terms destination is-private is-public is-directed from-who message-body))

