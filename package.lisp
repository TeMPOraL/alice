;;;; package.lisp

(defpackage #:alice
  (:use #:cl)
  (:export #:start-alice
           #:stop-alice
           #:impersonate-say
           #:impersonate-join
           #:impersonate-part
           #:impersonate-slap
           #:mute
           #:unmute))

