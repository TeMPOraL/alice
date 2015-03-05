(in-package #:alice)

;;; https://trac.common-lisp.net/bordeaux-threads/wiki/ApiDocumentation
;;; #'condition-wait and #'condition-notify could be used to sync the threads on event loop.

;;; also, consider moving timer for delayed actions into a separate
;;; unit with separate thread, that feeds events back to the primary event loop.
