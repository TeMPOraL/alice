;;; initialize SWANK

(ql:quickload :swank)
(setf swank::*LOOPBACK-INTERFACE* "0.0.0.0")
(swank:create-server :port 4005 :dont-close t)

;;; initialize app
(push #p"/var/src/" asdf:*central-registry*) ;FIXME ugly/old-style, do a symbolic link to QL's local-projects instead.
(ql:quickload :alice)
(alice:start-alice)
