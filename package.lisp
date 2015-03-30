;;;; package.lisp

(defpackage #:alice
  (:use #:cl
        #:alexandria)
  (:export #:*version*
           #:start-alice
           #:stop-alice
           #:impersonate-say
           #:impersonate-join
           #:impersonate-part
           #:impersonate-slap
           #:mute
           #:unmute))

