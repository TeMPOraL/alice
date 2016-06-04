(in-package #:alice)


;;; State

(defvar *hunchentoot-acceptor* nil)
(defparameter *remote-admin-magic-cookie* "")


;;; Routes
;;; NOTE we could (probably should) consider moving the routes and associated functionality to Alice's Grimoire.

(hunchentoot:define-easy-handler (index :uri "") ()
  (http-get-index-page))

(hunchentoot:define-easy-handler (status :uri "/status") ()
  (http-get-status-page))

(hunchentoot:define-easy-handler (relay-message :uri "/message/relay" :default-request-type :POST) (from to message lang)
  (http-relay-message from to message lang))

(hunchentoot:define-easy-handler (relay-canned-message :uri "/message/relay-canned" :default-request-type :POST) (from to message lang)
  (http-relay-canned-message from to message lang))


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

(defun server-debug-mode (mode)
  (setf hunchentoot:*show-lisp-errors-p* mode
        hunchentoot:*show-lisp-backtraces-p* mode))


;;; Implementation of remote management actions
(defparameter *alice-remote-canned-messages-pl* '((":ok" . "przekazuje: OK")
                                                  (":not-ok" . "przekazuje: Nie OK")
                                                  (":???" . "nie wie o co chodzi...")
                                                  (":call-me" . "prosi, żeby ktoś do niego zadzwonił.")))

(defparameter *alice-remote-canned-messages-en* '((":ok" . "relays: OK")
                                                  (":not-ok" . "relays: not OK")
                                                  (":???" . "doesn't know what this is about.")
                                                  (":call-me" . "asks for someone to call him.")))
(defun canned-message (msg lang)
  (cdr (assoc msg
              (if (string-equal lang "pl_PL")
                  *alice-remote-canned-messages-pl*
                  *alice-remote-canned-messages-en*)
              :test #'string-equal)))

(defun canned-message-preamble (from lang)
  (declare (ignore lang))
  from)

(defun relay-message-preamble (from lang)
  (concatenate 'string
               from
               " "
               (if (string= lang "pl_PL")
                   "kazał przekazać:"
                   "asked to say:")))


;;; Route handlers

(defun http-get-index-page ()
  (setf (hunchentoot:content-type*) "text/html")
  (who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html
     (:head (:title "Alice Margatroid")) ;TODO use parametric name
     (:body
      (:h1 "Why, hello there!")))))

(defun http-get-status-page ()
  (setf (hunchentoot:content-type*) "text/html")
  (who:with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html
     (:head (:title "Status | Alice Margatroid")) ;TODO use parametric name
     (:body
      (:h1 "Status page.")
      (:h2 "Connected channels:")
      (:ul (dolist (channel *connected-channels*)
             (who:htm (:li (who:str channel)))))))))

(defun http-relay-message (from to message lang)
  (setf (hunchentoot:content-type*) "application/json")
  (when (http-authenticatedp)
    (say to (concatenate 'string (relay-message-preamble from lang) " " message))))

(defun http-relay-canned-message (from to message lang)
  (setf (hunchentoot:content-type*) "application/json")
  (when (http-authenticatedp)
    (say to (concatenate 'string (canned-message-preamble from lang) " " (canned-message message lang)))))


;;; Additional utils

(defun http-authenticatedp ()
  (multiple-value-bind (user pass) (hunchentoot:authorization)
    (declare (ignorable user))
    (string= pass *remote-admin-magic-cookie*)))
