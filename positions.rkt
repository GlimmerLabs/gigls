#lang racket

(provide (all-defined-out))
(require gigls/guard
         gigls/higher
         gigls/utils)

;;; Procedure:
;;;   position?
;;; Purpose:
;;;   To ascertain whether the given value is a position pair
;;; Produces:
;;;   True or False
(define position?
  (lambda (value)
    (and (pair? value)
         (and (integer? (car value))
              (integer? (cdr value))))))

;;; Procedure:
;;;   position-col
;;; Purpose:
;;;   To get the x component of a position pair
;;; Produces:
;;;   X, the column
(define position-col car)


;;; Procedure:
;;;   position-row
;;; Purpose:
;;;   To get the y component of a position pair
;;; Produces:
;;;   Y, the row
(define position-row cdr)

;;; Procedure:
;;;   positions->floats
;;; Parameters:
;;;   positions, a list of positions
;;; Purpose:
;;;   Build an array of floats, applicable to the various
;;;   GIMP PDB procedures that expect positions in that form.
;;; Produces:
;;;   float-positions, an array of floats
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (vector-length float-positions) = (* 2 (length positions))
;;;   For all reasonable i
;;;     (position-col (list-ref positions i)) = 
;;;       (vector-ref float-positions (* 2 i))
;;;     (position-row (list-ref positions i)) = 
;;;        (vector-ref float-positions (+ 1 (* 2 i)))
;;; Philosophy:
;;;   The GIMP PDB procedures need positions in a format that is
;;;   not particularly convenient or clear for novice programmers.
;;;   This procedure, used primarily by MediaScheme GIMP wrappers, allows
;;;   programmers to represent lists of positions in a more natural format.
(define _positions->floats
  (lambda (positions)
    (let* ((len (length positions))
           (floats (make-vector (* 2 len))))
      (let kernel ((pos 0)
                   (remaining positions))
         (if (null? remaining)
             floats
             (begin
               (vector-set! floats pos 
                            (exact->inexact (position-col (car remaining))))
               (vector-set! floats (+ pos 1) 
                            (exact->inexact (position-row (car remaining))))
               (kernel (+ pos 2) (cdr remaining))))))))

(define positions->floats
  (lambda (positions)
    (cond
      ((not (list? positions))
       (error/parameter-type 'positions->floats 1 'list-of-positions 
                             (list positions)))
      ((not (all position? positions))
       (error/parameter-type 'positions->floats 1 'list-of-positions 
                             (list positions)))
      (else
       (_positions->floats positions)))))
;;; Procedure:
;;;     position-new
;;; Parameters:
;;;     Two integers
;;; Purpose:
;;;     Create an X-Y coordinate pair
;;; Produces:
;;;     A pair of integers
(define position-new 
  (lambda(x y)
    (if (and (integer? x) (integer? y))
        (cons x y)
        (error "enter two integers, given: " x y))))
