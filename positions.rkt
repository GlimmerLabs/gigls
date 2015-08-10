#lang racket

(provide (all-defined-out))
(require gigls/guard
         gigls/higher
         gigls/point
         gigls/utils)

;;; Note: Although the first version of this code seems to have been
;;;   checked in by SamR, the code looks like it was written by his
;;;   students.  (In particular, the comments don't follow Sam's normal
;;;   style.)

;;; Procedure:
;;;   position?
;;; Purpose:
;;;   To ascertain whether the given value is a position pair
;;; Produces:
;;;   True or False
(define position? point?)

;;; Procedure:
;;;   position-col
;;; Purpose:
;;;   To get the x component of a position pair
;;; Produces:
;;;   X, the column
(define position-col
  (guard-unary-proc 'position-col _point-col 'position position?))

;;; Procedure:
;;;   position-row
;;; Purpose:
;;;   To get the y component of a position pair
;;; Produces:
;;;   Y, the row
(define position-row
  (guard-unary-proc 'position-row _point-row 'position position?))

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
       (_points->floats positions)))))

;;; Procedure:
;;;   position-new
;;; Parameters:
;;;   x, a real
;;;   y, a real
;;; Purpose:
;;;   Create an (x,y) coordinate pair
;;; Produces:
;;;   pos, a position
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (position? pos)
;;;   (position-col pos) = x
;;;   (position-row pos) = y
(define position-new
  (guard-proc 'position-new
              _point
              (list 'real 'real)
              (list real? real?)))

; The following procedures were in the documentation, but did not get
; moved from the old gimplib to gigls.  This is an intermediate update
; as we get ready to get rid of positions.
(define position-distance
  (guard-proc 'position-distance
              _point-distance
              (list 'position 'position)
              (list position? position?)))

(define position-interpolate
  (guard-proc 'position-interpolate
              _point-interpolate
              (list 'position 'position 'real)
              (list position? position? real?)))

(define position-offset
   (guard-proc 'position-offset
               _point-offset
               (list 'position 'real 'real)
               (list position? real? real?)))
