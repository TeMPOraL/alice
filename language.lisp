(in-package :alice)

;; functions related to language processing

(defparameter *min-nick-length-for-stem-match* 4)

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

(defun stem-matches-p (word-checked target)
  (and (stringp word-checked)
       (stringp target)
       (let* ((lowcase-word-checked (string-downcase word-checked))
              (lowcase-target (string-downcase target))
              (len (length lowcase-target))
              (suffixless-target (subseq lowcase-target 0 (- len 1))))
         (if (< len *min-nick-length-for-stem-match*)
             (equalp lowcase-word-checked lowcase-target)
             (or (equalp lowcase-word-checked lowcase-target)
                 (equalp lowcase-word-checked suffixless-target)
                 (matches-regexp-p (make-stem-regexp lowcase-target) lowcase-word-checked)
                 (matches-regexp-p (make-stem-regexp suffixless-target) lowcase-word-checked))))))

(defun make-stem-regexp (base-word)
  (concatenate 'string "^" (escape-for-regexp base-word) "(y|i|a|ie|owi|e|ę|iowi|ce)$"))

(defun matches-regexp-p (regexp string)
  (not (null (cl-ppcre:scan regexp string))))

(defun escape-for-regexp (text)
  (cl-ppcre:regex-replace-all +regexp-escape-special-characters-regexp-part+ text "\\\\\\1"))

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

(defparameter *timestring-preprocessing-mappings*
  `(("\\bjed(e|(en|n[aą])\\b)" . "1")
    ("\\bp[oó][lł]tor(a|ej)\\b" . "1.5")
    ("\\bdw(a|ie\\b)" . "2")
    ("\\btrzy" . "3")
    ("\\bcztery?" . "4")
    ("\\bpi[eę][cćt]" . "5")
    ("\\bsze[sś][cć]" . "6")
    ("\\bsiedem" . "7")
    ("\\bosiem" . "8")
    ("\\bdziewi[eę][cćt]" . "9")
    ("\\bdziesi[eę][cć]\\b" . "10")
    ; ("na[sś]cie\\b" . "1") ;; no idea how to prepend with "1"
    ("dzie([sś]cia?|si[aą]t)\\b" . "0")
    ;; also no idea what with "pięćdziesiąt sześć"
    ("\\bsto\\b" . "100")
    ("\\bdwie[sś]cie\\b" . "200")
    ("s(ta|et)\\b" . "00")
    ("\\btysi[aą]c\\b" . "1000")
    ("\\btysi[aąeę]c[ey]\\b" . "000") ;; not really sure about this...
    ("\\bteraz\\b" . "now")
    ("\\bdzi(ś|s|siaj)\\b" . "today")
    ("\\bjutro\\b" . "tomorrow")
    ("\\bpojutrze\\b" . "day after tomorrow")
    ("\\brano\\b" . "06:00")
    ("\\bw po[lł]udnie\\b" . "12:00")
    ("\\bpo po(ł|l)udniu\\b" . "14:00")
    ("\\bwiecz(o|ó)r(em)?\\b" . "17:00")
    ("\\b(o|w) p[oó][lł]nocy?\\b" . "0:00")

    ("\\bp(o|ó)(l|ł) godziny\\b" . "30 minutes")
    ("\\bp(o|ó)(l|ł) minuty\\b" . "30 seconds")
    ("\\bp(o|ó)(l|ł) dnia\\b" . "6 hours")
    ("\\bp(o|ó)(l|ł) miesi(a|ą)ca\\b" . "15 days")
    ("\\bp(o|ó)(l|ł) roku\\b" . "6 months")

    ("\\bkwadrans\\b" . "15 minutes")
    ("\\bkwarta(l|ł)\\b" . "3 months")

    ("\\bsekund(y)?\\b" . "seconds")

    ("\\bza\\ssekund(e|ę)\\b" . "in 1 second")
    ("\\bza\\sminut(e|ę)\\b" . "in 1 minute")
    ("\\bminut(e|ę|y)?\\b" . "minutes")
    ("\\bgodzin(e|ę)\\b" . "1 hour")
    ("\\bgodziny?\\b" . "hours")

    ("\\bza\\srok\\b" . "next year")

    ("\\bdni\\b" . "days")
    ("\\btygodniu\\b" . "week")
    ("\\btygodnie\\b" . "weeks")
    ("\\bmiesi(a|ą)c\\b" . "1 month")
    ("\\bmiesi(a|ą)c[au]\\b" . "month")
    ("\\bmiesi(a|ą|ę)c(e|y)\\b" . "months")
    ("\\brok(u)?\\b" . "year")
    ("\\blat(a)?\\b" . "years")

    ("\\bnast(ę|e)pny(m)?\\b" . "next")
    ("\\bprzysz(ł|l)y(m)?\\b" . "next")

    ("\\bpoprzedni(m)?\\b" . "last")
    ("\\bzesz(l|ł)y(m)?\\b" . "last")

    ("\\bza\\stydzie(n|ń)\\b" . "next week")
    ("\\bza\\b" . "in")

    ("\\bponiedzia(ł|l)ek\\b" . "monday")
    ("\\bwtorek\\b" . "tuesday")
    ("\\b(ś|s)rod(a|ę|e)\\b" . "wednesday")
    ("\\bczwartek\\b" . "thursday")
    ("\\bpi(ą|a)tek\\b" . "friday")
    ("\\bsobot(a|ę|e)\\b" . "saturday")
    ("\\niedziel(a|ę|e)\\b" . "sunday")

    ("\\bza\\schwilk?(e|ę)\\b" . "in 15 seconds")
    ("\\bp(ó|o)(ź|z)niej\\b" . ,(lambda (time)
                     (declare (ignore time))
                     "in 2 hours"))      ;TODO adjust timestamp, reformat.
    ("\\bwe?\\b" . "on")
    ("\\bo\\b" . "at")))

;;; time from natural language
(defun compute-time-offset-from-string (offset-string &optional (reference-time (local-time:now)))
  (labels ((resolve-replacement (replacement)
             (typecase replacement
               (null "")
               (string replacement)
               (function (funcall replacement reference-time))
               (list (resolve-replacement (random-elt replacement)))
               (t "")))
           (preprocess-datetime-string (str)
             (dolist (rule *timestring-preprocessing-mappings*)
               (let ((pattern (car rule))
                     (replacement (resolve-replacement (cdr rule))))
                 (setf str (cl-ppcre:regex-replace pattern str replacement))))
             str))
    ;; NOTE ignore-errors, because in rare cases, chronicity:parse can raise a condition.
    (ignore-errors (chronicity:parse (preprocess-datetime-string (string-downcase offset-string)) :now reference-time))))

