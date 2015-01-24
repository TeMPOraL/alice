(in-package #:alice)

(register-matcher :whois
                  (list (match-score (lambda (input)
                                       (and (directedp input)
                                            (or (mentions "kto to" (unquoted-part input))
                                                (mentions "kim jest" (unquoted-part input)))))))
                  (lambda (input)
                    (if (mentions "kim jestem" (unquoted-part input))
                        (say (reply-to input) (identify-person-canonical-name (author input)) :to (author input))
                        (if (identify-person-mentioned (unquoted-part input))
                            (say (reply-to input) (identify-person-canonical-name (identify-person-mentioned (unquoted-part input))) :to (author input))
                            (say (reply-to input) "chyba nie wiem, o kogo Ci chodzi..." :to (author input)))))) ;FIXME string constant to remove

;; temporary control for remembering names
(register-matcher :assign-name-alias
                  (list (match-score (lambda (input)
                                       (and (not (publicp input))
                                            (mentions "zapamiętaj:" (unquoted-part input))))))
                  (lambda (input)
                    (let* ((names (extract-words (unquoted-part input)))
                           (alias (second names))
                           (canonical (third names)))
                      (if (and alias canonical)
                          (progn
                            (learn-canonical-name alias canonical)
                            (say (reply-to input) (format nil "Zapamiętałam ~A jako ~A." alias canonical)))
                          (say (reply-to input) "You fail at wydawanie poleceń. *sigh*")))))
