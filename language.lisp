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
             (equalp lowcase-word-checked suffixless-target)
             (matches-regexp-p (make-stem-regexp lowcase-target) lowcase-word-checked)
             (matches-regexp-p (make-stem-regexp suffixless-target) lowcase-word-checked)))))


(defun make-stem-regexp (base-word)
  (concatenate 'string "^" (escape-for-regexp base-word) "(y|i|a|ie|owi|e|ę|iowi|ce)$"))

(defun matches-regexp-p (regexp string)
  (not (null (cl-ppcre:scan regexp string))))

(defun escape-for-regexp (text)
  (cl-ppcre:regex-replace-all +regexp-escape-special-characters-regexp-part+ text "\\\\\\1"))


(define-constant +regexp-special-characters+ ".^$*+?()[{\|\\" :test #'string=)
(define-constant
    +regexp-escape-special-characters-regexp-part+
    (concatenate 'string
                 "(["
                 (coerce (mapcan (lambda (x) (list #\\ x))
                                 (coerce +regexp-special-characters+ 'list))
                         'string)
                 "])")
  :test #'string=)

;;; stems
;; wiktor owi
;; wiktor a
;; marchew ie
;; marchew e
;; marchew ę
;; dreadlish owi
