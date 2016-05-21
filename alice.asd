;;;; alice.asd

(asdf:defsystem #:alice
  :serial t

  :long-name "Alice Margatroid"
  :author "Jacek Złydach"
  :version (:read-file-form "version.lisp" :at (1 2 2))
  :description "IRC bot responding to queries based on natural language.."
  :long-description "Alice Margatroid, the Doll Maker of Bucuresti. An IRC-bot that pretends to be human."

  :license "Teaware - do whatever you want with it, but I wouldn't mind getting invited for a cup of tea ;)."
  :homepage "https://github.com/TeMPOraL/alice"
  :bug-tracker "https://github.com/TeMPOraL/alice/issues"
  :source-control (:git "https://github.com/TeMPOraL/alice.git")
  :mailto "temporal.pl+alice@gmail.com"

  :encoding :utf-8
  
  :depends-on (#:closer-mop
               #:cl-irc
               #:alexandria
               #:drakma
               #:cl-unicode
               #:cl-ppcre
               #:cxml
               #:cl-json
               #:local-time
               #:chronicity
               #:trivial-timers
               #:swank
               #:marshal
               #:hunchentoot
               #:cl-who)
  
  :components ((:file "package")
               (:file "version")

               (:module "utils"
                        :components ((:file "debug")
                                     (:file "persistence")
                                     (:file "string")
                                     (:file "macros")))
               
               (:file "language")
               
               (:module "core"
                        :components ((:file "trivial-event-loop")
                                     (:file "channel")
                                     (:file "message")
                                     (:file "person")
                                     (:file "world-model")
                                     (:file "input-matcher")
                                     (:file "output-builder")
                                     (:file "server")))

               (:module "grimoire"
                        :components ((:file "email")
                                     (:file "github")
                                     (:file "google")
                                     (:file "frequencies")
                                     (:file "meetups")
                                     (:file "notifications")
                                     (:file "people")
                                     (:file "package-tracking")
                                     (:file "pushover")
                                     (:file "tinyurl")
                                     (:file "varia")
                                     (:file "wolfram-alpha")))

               (:file "main" :depends-on ("grimoire"))

               (:module "specials"
                        :components ((:file "blueline")
                                     (:file "comments")
                                     (:file "general-terms")
                                     (:file "standard-answers")
                                     (:file "specials")))

               (:file "local-config" :depends-on ("grimoire"))))

