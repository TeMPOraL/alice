(in-package #:alice)

(defparameter *default-match-failed-score* 0)
(defparameter *default-match-successful-score* 1)

(defparameter *input-matchers* '() "An alist of pairs (name . (tests . result))")

(defmacro define-input-match (name matchspec &body body)
  ;; TODO one day
  )

;;; matchspec
;;; - set of matching rules with optional score that defaults to 1

(defun make-matcher (name tests result)
  "Create a new matcher instance that stores `TESTS' and `RESULT' code under the name `NAME'."
  (cons name (cons tests result)))

(defun matcher-name (matcher)
  (car matcher))

(defun matcher-tests (matcher)
  (cadr matcher))

(defun matcher-result (matcher)
  (cddr matcher))

;;; FIXME no language support or anything? (or differently named matchers for each language needed)
;;; will deal with that later.

(defun score-matchers (input)
  "Return the list of (name . matcher) sorted by how well they match `INPUT'."
  (mapcar (lambda (matcher)
            (cons (matcher-name matcher)
                  (reduce (lambda (accumulator function)
                            (+ accumulator (funcall function input)))
                          (matcher-tests matcher)
                          :initial-value 0)))
          *input-matchers*))

(defun best-matchers (input)
  "Return the list of matchers matching `INPUT' the best. List can have more than one element."
  (sort (score-matchers input) (lambda (x y) (< (cdr x) (cdr y)))))

(defun register-matcher (name tests match-result)
  "Register a set of `TESTS' under `NAME'. `TESTS' should be a list of functions that returns a number, which will be used to score the best match.
`MATCH-RESULT' is the code to be executed when `NAME' is the highest-scoring match."
  (setf *input-matchers* (cons (make-matcher name tests match-result) (remove-if (lambda (matcher) (eql name (matcher-name matcher))) *input-matchers*))))

(defun match-score (function &optional (score *default-match-successful-score*) (fail-score *default-match-failed-score*))
  "Create a function that wraps passed `FUNCTION' to return `SCORE' if `FUNCTION' returns T, or `FAIL-SCORE' otherwise."
  (lambda (input)
    (if (funcall function input)
        score
        fail-score)))
