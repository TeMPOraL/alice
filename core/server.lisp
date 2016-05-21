(in-package #:alice)


;;; State

(defvar *hunchentoot-acceptor* nil)


;;; Routes
;;; NOTE we could (probably should) consider moving the routes and associated functionality to Alice's Grimoire.

(Hunchentoot:define-easy-handler (index :uri "") ()
  (http-get-index-page))

(hunchentoot:define-easy-handler (status :uri "/status") ()
  (http-get-status-page))

(hunchentoot:define-easy-handler (relay-message :uri "/message/relay" :default-request-type :POST) (from to message lang)
  (http-relay-message (from to message lang)))

(hunchentoot:define-easy-handler (relay-canned-message :uri "/message/relay-canned" :default-request-type :POST) (from to message lang)
  (http-relay-canned-message (from to message lang)))


;;; Webserver management interface

(defun start-server (&key (port 12321))
  "Start Alice's management HTTP server on `PORT'."
  (unless *hunchentoot-acceptor*
    (setf *hunchentoot-acceptor* (make-instance 'hunchentoot:easy-acceptor :port port))
    (hunchentoot:start *hunchentoot-acceptor*)))

(defun stop-server ()
  "Stop previously started Alice's HTTP server.
Do NOT call this from a request handler, or it might deadlock."
  (when *hunchentoot-acceptor*
    (hunchentoot:stop *hunchentoot-acceptor* :soft t)
    (setf *hunchentoot-acceptor* nil)))

(defun request-stop-server ()
  "Request Alice's server to be stopped at some time in the nearby future.
Like `STOP-SERVER', but for use from within request handlers."
  ;; TODO
  )

(defun force-stop-server ()
  "Force stop Alice's HTTP server and clear its connection state. Use if server broke
in such a way you can't stop or start it normally with `STOP-SERVER' or `START-SERVER'."
  (when *hunchentoot-acceptor*
    (Hunchentoot:stop *hunchentoot-acceptor* :soft nil))
  (setf *hunchentoot-acceptor* nil))


;;; Route handlers

(defun http-get-index-page ()
  (setf (hunchentoot:content-type) "text/html")
  (who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html
     (:head (:title "Alice Margatroid")) ;TODO use parametric name
     (:body
      (:h1 "Why, hello there!")))))

(defun http-get-status-page ()
  (setf (hunchentoot:content-type) "text/html")
  (who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html
     (:head (:title "Status | Alice Margatroid")) ;TODO use parametric name
     (:body
      (:h1 "Status page - will be here one day.")))))

(defun http-relay-message (from to message lang)
  ;; TOOD
  ;; Also TODO - check authentication
  "TODO")

(defun http-relay-canned-message (from to message lang)
  ;; TODO
  ;; Also TODO - check authentication
  "TODO")


;;; Additional utils

(defun http-authenticatedp ()           ;TODO maybe accept magic cookie as argument?
  ;; TODO
  )
