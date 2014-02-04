;;;; alice.asd

(asdf:defsystem #:alice
  :serial t
  :description "Describe alice here"
  :author "Jacek Złydach"
  :license "Specify license here"
  :depends-on (#:cl-irc
               #:alexandria
               #:drakma
               #:cl-unicode
               #:cl-ppcre
               #:cxml)
  :components ((:file "package")
               (:file "globals" :depends-on ("package"))
               (:file "local-config" :depends-on ("globals"))
               (:file "world-model" :depends-on ("local-config-template"))
               (:file "grimoire" :depends-on ("world-model"))
               (:file "main" :depends-on ("grimoire"))))
