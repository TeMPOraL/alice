(in-package #:alice-tests)

(def-suite stemming :description "Test stemming-related functions.")

(def-suite timestrings :description "Test timestring-related functions.")

(in-suite timestrings)

(test polish-time-offsets
  (let ((reference-time (local-time:parse-timestring "2015-04-03T12:34:56+02:00")))
    (is (local-time:timestamp= reference-time
                               (alice::compute-time-offset-from-string "teraz" reference-time)))

    (is (local-time:timestamp= (local-time:parse-timestring "2015-04-04T00:00:00+02:00")
                               (alice::compute-time-offset-from-string "jutro" reference-time)))

    (is (local-time:timestamp= (local-time:parse-timestring "2015-04-04T06:00:00+02:00")
                               (alice::compute-time-offset-from-string "jutro rano" reference-time)))

    (is (local-time:timestamp= (local-time:parse-timestring "2015-04-04T14:00:00+02:00")
                               (alice::compute-time-offset-from-string "jutro po południu" reference-time)))

    (is (local-time:timestamp= (local-time:parse-timestring "2015-04-04T17:00:00+02:00")
                               (alice::compute-time-offset-from-string "jutro wieczorem" reference-time)))

    (is (local-time:timestamp= (local-time:parse-timestring "2015-04-04T17:00:00+02:00")
                               (alice::compute-time-offset-from-string "jutro wieczor" reference-time)))

    (is (local-time:timestamp= (local-time:parse-timestring "2015-04-04T17:00:00+02:00")
                               (alice::compute-time-offset-from-string "jutro wieczór" reference-time)))
    
    ;; TODO add moar tests.
    ))

(add-suite 'timestrings)
