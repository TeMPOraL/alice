(in-package :alice)

;; functions related to language processing

(defun stem-matches-p (word-checked target)
  (and (stringp word-checked)
       (stringp target)
       (let* ((lowcase-word-checked (string-downcase word-checked))
              (lowcase-target (string-downcase target))
              (len (length lowcase-target))
              (suffixless-target (subseq lowcase-target 0 (- len 1))))
         (or (equalp lowcase-word-checked lowcase-target)
             (matches-regexp-p (make-stem-regexp lowcase-target) lowcase-word-checked)
             (matches-regexp-p (make-stem-regexp suffixless-target) lowcase-word-checked)))))


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
