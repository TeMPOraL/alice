;;;; package.lisp

(defpackage #:alice
  (:use #:cl
        #:alexandria)
  (:export #:start-alice
           #:stop-alice
           #:impersonate-say
           #:impersonate-join
           #:impersonate-part
           #:impersonate-slap
           #:mute
           #:unmute))

