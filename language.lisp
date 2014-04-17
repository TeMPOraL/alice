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
    (:yesterday . ("wczoraj"))
    (:two-days-ago . ("przedwczoraj" "dwa dni temu"))
    (:three-days-ago . ("trzy dni temu"))
    ;; 
    (:tomorrow . ("jutro"))
    (:day-after-tomorrow . ("pojutrze"))
    ;; 
    (:monday . ("w poniedziałek"))
    (:tuesday . ("we wtorek"))
    (:wednesday . ("w środę"))
    (:thursday . ("w czwartek"))
    (:friday . ("w piątek"))
    (:saturday . ("w sobotę"))
    (:sunday . ("w niedzielę"))))

(defparameter *days-of-week* #(:monday :tuesday :wednesday :thursday :friday :saturday :sunday))

(defparameter *days-offset-alist* '((0 . :today)

                                    (1 . :yesterday)
                                    (2 . :two-days-ago)
                                    (3 . :three-days-ago)

                                    (-1 . :tomorrow)
                                    (-2 . :day-after-tomorrow)))

(defvar +timestring-format+ '((:DAY 2) #\. (:MONTH 2) #\. (:YEAR 4)  #\  (:HOUR 2) #\: (:MIN 2) #\: (:SEC 2)))

(defvar +timestring-date-format+ '((:DAY 2) #\. (:MONTH 2) #\. (:YEAR 4)))
(defvar +timestring-time-format+ '((:HOUR 2) #\: (:MIN 2) #\: (:SEC 2)))

;; DAYS
(defun days-diff (time-a time-b)        ;FIXME should go to some sort of date utils
  "Compute the difference in calendar days between `TIME-A' and `TIME-B'."
  (- (local-time:day-of (local-time:timestamp-minimize-part time-a :hour))
     (local-time:day-of (local-time:timestamp-minimize-part time-b :hour))))

(defun fix-day-of-week-offset (offset)
  (mod (1- offset) 7))

(defun timestamp-day-of-week (timestamp)
  (local-time:with-decoded-timestamp (:day-of-week d-o-w) timestamp d-o-w))

(defun get-date-day-of-week-phrase (timestamp reference)
  (let ((days-offset (days-diff reference timestamp))
        (day-of-week (fix-day-of-week-offset (timestamp-day-of-week timestamp))))
    (when (and (> days-offset 0)
             (< days-offset 7))
        (elt *days-of-week* day-of-week))))

(defun get-relative-date-difference-phrase (time-a time-b)
  "Compute natural-language date difference of days between timestamps `TIME-A' and `TIME-B'. Return a proper symbol or NIL if no natural-language representation is available."
  (let ((days-offset (days-diff time-a time-b)))
    (cdr (assoc days-offset *days-offset-alist*))))


(defun format-date (timestamp &optional (reference (local-time:now)))
  (let ((phrase (or (get-relative-date-difference-phrase reference timestamp)
                    (get-date-day-of-week-phrase timestamp reference))))
    (if phrase (alexandria:random-elt (cdr (assoc phrase *date-difference-strings*))) ;FIXME use say-equivalent or sth
        (local-time:format-timestring nil timestamp :format +timestring-date-format+))))

;; TIME
(defun format-time (timestamp)
  (local-time:format-timestring nil timestamp :format +timestring-time-format+))
