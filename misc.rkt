#lang racket
; A miscellaneous racket library for wrapper functions that 
; a: don't belong in another library or b: would cause 
; libraries to be interdependent.

(require LoudGimp/guard
         LoudGimp/higher)
(provide (all-defined-out))



;;; Procedure:
;;;   increment
;;; Parameters:
;;;   val, a number
;;; Purpose:
;;;   Add 1 to val
;;; Produces:
;;;   incremented, a number
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   incremented = (+ 1 val)
;;; Ponderings:
;;;   An obvious procedure, but one that is often useful.
(define increment (l-s + 1))

;;; Procedure:
;;;   iota
;;; Parameters:
;;;   n, a positive integer
;;; Purpose:
;;;   Create a list of all non-negative integers less than n.
;;; Produces:
;;;   ints, a list of integers
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (length ints) = n
;;;   ints has the form (0 1 2 3 ...)
(define _iota
  (lambda (n)
    (let kernel ((i 0))
      (if (= i n)
          null
          (cons i (kernel (+ i 1)))))))

(define iota 
  (lambda (n)
    (validate-param! 'iota 'non-negative-integer (^and integer? (^not negative?)) (list n))
    (_iota n)))


;;; Procedure:
;;;   repeat!
;;; Parameters:
;;;   i, a non-negative integer
;;;   proc!, an n-ary procedure
;;;   v1 ... vn, 0 or more optional parameters
;;; Purpose:
;;;   Calls (proc! v1 ... vn) i times.
;;; Produces:
;;;   [Nothing; called for the side effect]
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   The procedure has been applied the specified number of times.
(define _repeat
  (lambda (i proc! . args)
    (let kernel ((i i))
      (cond 
        ((positive? i)
         (apply proc! args)
         (kernel (- i 1)))))))

(define repeat
  (lambda (i proc! . args)
    (let ((params (cons i (cons proc! args))))
      (cond
        ((or (not (integer? i)) (negative? i))
         (error/parameter-type 'repeat 1 'positive-integer params))
        ((not (procedure? proc!))
         (error/parameter-type 'repeat 2 'procedure params))
        (else 
         (apply _repeat params))))))

(define repeat! repeat)

;;; Procedure:
;;;   square
;;; Parameters:
;;;   n, a number
;;; Purpose:
;;;   Computes n squared
;;; Produces:
;;;   nsquared, a number
;;; Preconditions:
;;;   [No additional preconditions]
;;; Postconditions:
;;;   nsquared = n*n
(define square
  (lambda (n)
    (* n n)))
