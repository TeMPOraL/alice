;;;; alice.asd

(asdf:defsystem #:alice
  :serial tppp
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
               
               (:file "globals" :depends-on ("package"))
               (:file "language" :depends-on ("globals"))
               
               (:module "world"
                        :components ((:file "message")
                                     (:file "world-model")))

               (:module "grimoire"
                        :components ((:file "github")
                                     (:file "google")
                                     (:file "notifications")
                                     (:file "package-tracking")
                                     (:file "tinyurl")
                                     (:file "wolfram-alpha")))


               
               (:file "local-config" :depends-on ("grimoire"))
               (:file "main" :depends-on ("grimoire"))

               (:module "specials"
                        :components ((:file "blueline")
                                     (:file "comments")
                                     (:file "general-terms")
                                     (:file "standard-answers")
                                     (:file "specials")))))

