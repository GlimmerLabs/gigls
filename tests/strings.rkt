#lang racket
(require rackunit)
(require gigls/strings)
(provide (all-defined-out))

(define tests-strings
  (test-suite
   "Tests of the gigls strings library"
   (check-true (string-ends-with? "Hello" "") "Ends with empty string")
   (check-true (string-ends-with? "Hello" "o") "Ends with simple string")
   (check-true (string-ends-with? "Hello" "lo") "Ends with simple string")
   (check-true (string-ends-with? "Hello" "Hello") "Ends with itself")
   (check-false (string-ends-with? "" "Foo") "Empty string does not crash")
   (check-false (string-ends-with? "Foo" "F") "Internal chars do not match")
   ))

