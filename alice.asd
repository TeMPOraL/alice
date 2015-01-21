;;;; alice.asd

(asdf:defsystem #:alice
  :serial t
  :description "Alice Margatroid, the Doll Maker of Bucuresti. An IRC-bot that pretends to be human."
  :author "Jacek Złydach"
  :license "Teaware - do whatever you want with it, but I wouldn't mind getting invited for a cup of tea ;)."
  :depends-on (#:cl-irc
               #:alexandria
               #:drakma
               #:cl-unicode
               #:cl-ppcre
               #:cxml
               #:cl-json
               #:local-time)
  
  :components ((:file "package")

               (:module "utils"
                        :components ((:file "debug")
                                     (:file "persistence")
                                     (:file "string")))
               
               (:file "language")
               
               (:module "world"
                        :components ((:file "channel")
                                     (:file "message")
                                     (:file "person")
                                     (:file "world-model")
                                     (:file "input-matcher")))

               (:module "grimoire"
                        :components ((:file "github")
                                     (:file "google")
                                     (:file "notifications")
                                     (:file "people")
                                     (:file "package-tracking")
                                     (:file "tinyurl")
                                     (:file "varia")
                                     (:file "wolfram-alpha")))

               (:file "local-config" :depends-on ("grimoire"))
               (:file "main" :depends-on ("grimoire"))

               (:module "specials"
                        :components ((:file "blueline")
                                     (:file "comments")
                                     (:file "general-terms")
                                     (:file "standard-answers")
                                     (:file "specials")))))

