(in-package #:alice)

;;; query

(register-matcher :events-list
                  (list (match-score (lambda (input)
                                       (and (directedp input)
                                            (or (mentions "eventy" (unquoted-part input))
                                                (mentions "wydarzenia" (unquoted-part input))
                                                (mentions "coś ciekawego" (unquoted-part input)))))))
                  (lambda (input)
                    (declare (ignore input))
                    nil))

;;; output
(provide-output :no-events-planned "")
(provide-output :error-querying-events "")


;;; utilities
