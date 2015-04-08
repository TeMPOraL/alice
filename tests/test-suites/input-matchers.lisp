(in-package #:alice-tests)

(def-suite input-matchers :description "Test how various inputs match in Alice.")

(in-suite input-matchers)

(defmethod print-object ((message alice::message) stream)
  (print-unreadable-object (message stream :type t :identity nil)
    (format stream
            "(~A ~A ~A) ~A@~A: ~A"
            (alice::publicp message)
            (alice::privatep message)
            (alice::directedp message)
            (alice::author message)
            (alice::reply-to message)
            (alice::raw-text message))))

;;TODO eventually replace with `DEF-FIXTURE'/`WITH-FIXTURE' from FiveAM.
(defmacro with-temporary-environment (&body code)
  `(let ((alice::*nick* "Alice_M"))
     ,@code))

;;; helpers
(defun make-input (raw-text &key (publicp t) (privatep nil) (directedp t))
  (make-instance 'alice::message
                 :raw-text raw-text
                 :publicp publicp
                 :privatep privatep
                 :directedp directedp
                 :author "TestUser"
                 :channel (when privatep "#test")
                 :reply-to (if privatep
                               "TestUser"
                               "#test")))

(defun best-match (match input)
  (eql match
       (car (first (alice::best-matchers input)))))

(defun only-primary-match (match input)
  (flet ((remove-non-equally-scored-matches (score matches)
           (remove-if-not (lambda (match)
                            (= (cdr match) score))
                          matches)))
   (let ((best-matches (alice::best-matchers input)))
     (and (= 1 (length (remove-non-equally-scored-matches (cdr (first best-matches))
                                                          best-matches)))
          (eql match
               (car (first best-matches)))))))

;;; tests
(test version-number
  (with-temporary-environment
    (mapc (lambda (input)
            (is (only-primary-match :version (apply 'make-input input))))
          `(
           ;; pl_PL
            ("Alice_M, wersja")
            ("Alice_M, jaką masz wersję?")
            ("Jaki masz numer wersji, Alice_M?")
            ("Alice_M: podaj numer wersji!")

            ;; en_US
            ("Alice_M, version")
            ("Alice_M, version number please!")))))

(add-suite 'input-matchers)
