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
               
               (:file "globals" :depends-on ("package"))
               (:file "language" :depends-on ("globals"))
               (:file "world-model" :depends-on ("language"))

               (:module "grimoire"
                        :components ((:file "github")
                                     (:file "google")
                                     (:file "notifications")
                                     (:file "package-tracking")
                                     (:file "tinyurl")
                                     (:file "wolfram-alpha")))
               
               (:file "sentence-features" :depends-on ("world-model"))
               (:file "local-config" :depends-on ("grimoire"))
               (:file "main" :depends-on ("grimoire"))
               (:file "specials" :depends-on ("grimoire"))
               ))
