;;; alice-tests.asd

(asdf:defsystem #:alice-tests
  :serial t

  :long-name "Alice Margatroid Test Suite"
  :author "Jacek Złydach"
  :version (:read-file-form "version.lisp" :at (1 2 2))
  :description "Tests for Alice, the IRC bot."

  :license "Teaware - do whatever you want with it, but I wouldn't mind getting invited for a cup of tea ;)."
  :homepage "https://github.com/TeMPOraL/alice"
  :bug-tracker "https://github.com/TeMPOraL/alice/issues"
  :source-control (:git "https://github.com/TeMPOraL/alice.git")
  :mailto "temporal.pl+alice@gmail.com"

  :encoding :utf-8

  :depends-on (#:alice
               #:fiveam)

  :perform (test-op (o s)
                    (uiop:symbol-call :alice-tests '#:run-all))

  :components ((:module "tests"
                        :components ((:file "package")
                                     (:file "suites")

                                     (:module "test-suites"
                                              :components ((:file "language")
                                                           (:file "string-utils")

                                                           (:module "behavioral"
                                                                    :components ((:file "input-matchers")))))))))

