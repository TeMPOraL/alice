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


;; functions related to time formatting
;; TODO if switching to feature/style, this looks like something to be factored out to it's own file!

(defparameter *date-difference-strings*
  '((:today . ("dzisiaj" "dziś"))
    ;; 
    (:yesterday . "wczoraj")
    (:two-days-ago . ("przedwczoraj" "dwa dni temu"))
    (:three-days-ago . "trzy dni temu")
    ;; 
    (:tomorrow . "jutro")
    (:day-after-tomorrow . "pojutrze")
    ;; 
    (:monday . "w poniedziałek")
    (:tuesday . "we wtorek")
    (:wednesday . "w środę")
    (:thursday . "w czwartek")
    (:friday . "w piątek")
    (:saturday . "w sobotę")
    (:sunday . "w niedzielę")
    ;; 
    (:prev-monday . ("w zeszły poniedziałek"
                     "w ostatni poniedziałek"))
    (:prev-tuesday . ("w zeszły wtorek"
                      "w ostatni wtorek"))
    (:prev-wednesday . ("w zeszłą środę"
                     "w ostatnią środę"))
    (:prev-thursday . ("w zeszły czwartek"
                      "w ostatni czwartek"))
    (:prev-friday . ("w zeszły piątek"
                     "w ostatni piątek"))
    (:prev-saturday . ("w zeszłą sobotę"
                      "w ostatnią sobotę"))
    (:prev-sunday . ("w zeszłą niedzielę"
                     "w ostatnią niedzielę"))))

(defparameter *days-of-week* #(:sunday :monday :tuesday :wednesday :thursday :friday :saturday))
(defparameter *days-of-previous-week* #(:prev-sunday :prev-monday :prev-tuesday :prev-wednesday :prev-thursday :prev-friday :prev-saturday))

(defun days-diff (time-a time-b)
  "Compute the difference in calendar days between `TIME-A' and `TIME-B'."
  (- (local-time:day-of time-a)
     (local-time:day-of time-b)))


(defun date-difference (time-a time-b)
  (declare (ignore time-a time-b))
  "Compute natural-language date difference of days between timestamps `TIME-A' and `TIME-B'."
  nil)

