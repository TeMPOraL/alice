(in-package #:alice-tests)

(def-suite implode :description "Test `ALICE::IMPLODE'.")

(in-suite implode)

(test implode-proper-type
  "`ALICE::IMPLODE's result is of proper type."
  (is (stringp (alice::implode '("foo" "bar" "baz"))))
  (is (stringp (alice::implode '())))
  (is (stringp (alice::implode '() "---"))))

(test implode-separator-is-properly-applied
  "Checks if `ALICE::IMPLODE' applies its optional separator parameter correctly."
  (is (string= "1 2 3" (alice::implode '(1 2 3))))
  (is (string= "1-2-3" (alice::implode '(1 2 3) #\-)))
  (is (string= "1 and 2 and 3" (alice::implode '(1 2 3) " and "))))

(add-suite 'implode)
