(in-package :alice)

;; functions related to language processing

(defun stem-matches-p (word-checked target)
  (let* ((len (length target))
         (suffixless-target (subseq target 0 (- len 1))))
    (or (equalp word-checked target)
        (matches-regexp-p (make-stem-regexp target) word-checked)
        (matches-regexp-p (make-stem-regexp suffixless-target) word-checked))))


(defun make-stem-regexp (base-word)
  (concatenate 'string base-word "(y|i|a|ie|owi|e|ę)$"))


(defun matches-regexp-p (regexp string)
  (not (null (cl-ppcre:scan regexp string))))

;;; stems
;; wiktor owi
;; wiktor a
;; marchew ie
;; marchew e
;; marchew ę
;; dreadlish owi
