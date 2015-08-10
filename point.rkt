#lang racket

; point.rkt
;   A simple representation of points on the plane.

(provide (all-defined-out))
(require gigls/guard
         gigls/higher
         gigls/list
         gigls/misc
         gigls/utils)

;;; procedures:
;;;   point
;;;   point-new
;;; Parameters:
;;;   x, a real
;;;   y, a real
;;; Purpose:
;;;   Create an (x,y) coordinate pair
;;; Produces:
;;;   pos, a point
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (point? pos)
;;;   (point-col pos) = x
;;;   (point-row pos) = y
(define _point
  (lambda (x y)
    (cons x y)))

(define point-new
  (guard-proc 'point-new
              _point
              (list 'real 'real)
              (list real? real?)))

(define point
  (guard-proc 'point
              _point
              (list 'real 'real)
              (list real? real?)))

;;; Procedure:
;;;   point?
;;; Parameters:
;;;   value, a Scheme value.
;;; Purpose:
;;;   Determines whether the given value represents a point.
;;; Produces:
;;;   is-point, a Boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If value can be interpreted as a point, is-point is true.
;;;   Otherwise, is-point is false.
(define _point?
  (lambda (value)
    (and (pair? value)
         (and (real? (car value))
              (real? (cdr value))))))
(define point? _point?)

;;; Procedure:
;;;   point-col
;;; Parameters:
;;;   point, a point (created by point-new or similar)
;;; Purpose:
;;;   To get the x component of a point.
;;; Produces:
;;;   x, a real number
;;; Preconditions:
;;;   (point? point)
;;; Postconditions:
;;;   x represents the x coordinate (column) of the point.
(define _point-col car)

(define point-col
  (guard-unary-proc 'point-col _point-col 'point point?))

;;; Procedure:
;;;   point-distance
;;; Parameters:
;;;   p1, a point
;;;   p2, a point
;;; Purpose:
;;;   Computes the distance between p1 and p2.
;;; Produces:
;;;   distance, a non-negative real number.
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   A line from p1 to p2 will have length distance.
(define _point-distance
  (lambda (p1 p2)
      (sqrt (+ (square (- (point-col p1) (point-col p2)))
               (square (- (point-row p1) (point-row p2)))))))

(define point-distance
  (guard-proc 'point-distance
              _point-distance
              (list 'point 'point)
              (list point? point?)))

;;; Procedure:
;;;   point-interpolate
;;; Parameters:
;;;   p1, a point
;;;   p2, a point
;;;   p,  real number
;;; Purpose:
;;;   Compute the point that is p percent of the way from p1 to p2.
;;; Produces:
;;;   newpta point
;;; Preconditions:
;;;   0 <= p <= 1 [unverified]
;;; Postconditions:
;;;   (point-distance p1 newpt) = (* p (point-distance p1 p2))
;;;   newpt falls on the line from p1 to p2.  That is,
;;;     If (point-col p2) and (point-col p1) are different,
;;;       (/ (- (point-row newpt) (point-row p1)) 
;;;          (- (point-col newpt) (point-col p1)))
;;;       = (/ (- (point-row p2) (point-row p1))
;;;            (- (point-col p2) (point-col p1)))
(define _point-interpolate
  (lambda (p1 p2 p)
    (let ((q (- 1 p)))
      (point-new (+ (* p (point-col p2))
                    (* q (point-col p1)))
                 (+ (* p (point-row p2))
                    (* q (point-row p1)))))))

(define point-interpolate
  (guard-proc 'point-interpolate
              _point-interpolate
              (list 'point 'point 'real)
              (list point? point? real?)))

;;; Procedure:
;;;   point-offset
;;; Parameters:
;;;   point, a point
;;;   col-offset, a real number
;;;   row-offset, a real number
;;; Purpose:
;;;   Build a new point, offset by point by the specified offsets.
;;; Produces:
;;;   new-point, a point
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (point-col new-point) = (point-col point) + col_offset
;;;   (point-row new-point) = (point-row point) + row_offset
(define _point-offset
  (lambda (point delta-col delta-row)
    (point (+ (point-col point) delta-col)
           (+ (point-row point) delta-row))))

(define point-offset
  (guard-proc 'point-offset
              _point-offset
              (list 'point 'real 'real)
              (list point? real? real?)))

;;; Procedure:
;;;   point-row
;;; Parameters:
;;;   point, a point (created by point-new or similar)
;;; Purpose:
;;;   To get the y component of a point.
;;; Produces:
;;;   y, a real number
;;; Preconditions:
;;;   (point? point)
;;; Postconditions:
;;;   y represents the y coordinate (row) of the point.
(define _point-row cdr)

(define point-row
  (guard-unary-proc 'point-row _point-row 'point point?))

;;; Procedure:
;;;   points->floats
;;; Parameters:
;;;   points, a list of points
;;; Purpose:
;;;   Build a vector/array of floats, applicable to the various
;;;   GIMP PDB procedures that expect points in that form.
;;; Produces:
;;;   float-points, an array of floats
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (vector-length float-points) = (* 2 (length points))
;;;   For all reasonable i
;;;     (point-col (list-ref points i)) = 
;;;       (vector-ref float-points (* 2 i))
;;;     (point-row (list-ref points i)) = 
;;;        (vector-ref float-points (+ 1 (* 2 i)))
;;; Philosophy:
;;;   The GIMP PDB procedures need points in a format that is
;;;   not particularly convenient or clear for novice programmers.
;;;   This procedure, used primarily by MediaScheme GIMP wrappers, allows
;;;   programmers to represent lists of points in a more natural format.
(define _points->floats
  (lambda (points)
    (let* ((len (length points))
           (floats (make-vector (* 2 len))))
      (let kernel ((pos 0)
                   (remaining points))
         (if (null? remaining)
             floats
             (begin
               (vector-set! floats pos 
                            (exact->inexact (_point-col (car remaining))))
               (vector-set! floats (+ pos 1) 
                            (exact->inexact (_point-row (car remaining))))
               (kernel (+ pos 2) (cdr remaining))))))))

(define points->floats
  (lambda (points)
    (cond
      ((not (list? points))
       (error/parameter-type 'points->floats 1 'list-of-points 
                             (list points)))
      ((not (all point? points))
       (error/parameter-type 'points->floats 1 'list-of-points 
                             (list points)))
      (else
       (_points->floats points)))))

;;; Procedure:
;;;   random-points
;;; Parameters:
;;;   n, an integer
;;;   cols, an integer
;;;   rows, an integer
;;; Purpose:
;;;   Generate a list of n points, each between (0,0) [inclusive]
;;;     and (cols,rows) [exclusive]
;;; Produces:
;;;   points, a list of points.
;;; Preconditions:
;;;   n >= 0
;;;   cols >= 1
;;;   rows >= 1
;;; Postconditions:
;;;   (length points) = n
;;;   For each i, 0 < i < n
;;;     (position? (list-ref points i)) 
;;;     (integer? (position-col (list-ref points i)))
;;;     (integer? (position-row (list-ref points i)))
;;;     0 <= (position-col (list-ref points i)) < cols
;;;     0 <= (position-row (list-ref points i)) < rows
(define _random-points
  (lambda (n cols rows)
    (_list-random n (lambda () (point (random cols) (random rows))))))

(define random-points
  (guard-proc 'random-points
              _random-points
              (list 'non-negative-integer 'positive-integer 'positive-integer)
              (list (^and integer? (r-s >= 0))
                    (^and integer? (r-s > 0))
                    (^and integer? (r-s > 0)))))
