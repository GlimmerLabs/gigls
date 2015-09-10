#lang racket

; gigls/hsv.rkt
;   A few HSV (Hue, Saturation, Value) procedures for the Glimmer Improved
;   gimp library for scripting.

(require gigls/guard
         gigls/higher)

(provide (all-defined-out))

;;; Procedure:
;;;   hsv
;;; Parameters:
;;;   hue, a real number, preferably in the range [0..360)
;;;   saturation, a real number in the range [0..1]
;;;   value, a real number in the range [0..1]
;;; Purpose:
;;;   Create an hsv value.
;;; Produces:
;;;   hsv, an hsv value
;;; Postconditions:
;;;   (hsv? hsv)
;;;   (hsv-hue hsv) = hue
;;;   (hsv-saturation hsv) = saturation
;;;   (hsv-value hsv) = value
(define _hsv list)
(define hsv
  (guard-proc 'hsv
              _hsv
              (list 'real-between-0-and-360 'real-between-0-and-1 'real-between-0-and-1)
              (list (^and real? (l-s <= 0) (r-s < 360))
                    (^and real? (l-s <= 0) (r-s <= 1))
                    (^and real? (l-s <= 0) (r-s <= 1)))))

;;; Procedure:
;;;   hsv?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determines if val could represent a hue-saturation-value color.
;;; Produces:
;;;   is-hsv?, a Boolean
(define hsv?
  (lambda (val)
    (and (list? val)
         (= (length val) 3)
         (real? (car val))
         ; (<= 0 (car val) 360)
         (real? (cadr val))
         (<= 0 (cadr val) 1)
         (real? (caddr val))
         (<= 0 (caddr val) 1))))

;;; Procedure:
;;;   guard-hsv-proc
;;; Parameters:
;;;   procname, a symbol
;;;   proc, a one-parameter procedure that expects an HSV value
;;; Purpose:
;;;   Creates a version of proc that checks that its parameter
;;;   is an HSV value.
;;; Produces:
;;;   guarded-proc, a procedure
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If val is an HSV value, then
;;;     (guarded-proc val) = (proc val)
;;;   Otherwise
;;;     (guarded-proc val) reports an error
(define guard-hsv-proc
  (lambda (procname proc)
    (guard-unary-proc procname proc 'hsv-color hsv?)))

;;; Procedure:
;;;   hsv-hue
;;; Parameters:
;;;   hsv, an HSV color
;;; Purpose:
;;;   Extract the hue from an HSV color.
;;; Produces:
;;;   hue, an integer
;;; Preconditions:
;;;   (hsv? hsv)
;;; Postconditions:
;;;   0 <= hue <= 360
(define _hsv-hue car)
(define hsv-hue (guard-hsv-proc 'hsv-hue _hsv-hue))

;;; Procedure:
;;;   hsv-saturation
;;; Parameters:
;;;   hsv, an HSV color
;;; Purpose:
;;;   Extract the saturation from an HSV color.
;;; Produces:
;;;   saturation, a real
;;; Preconditions:
;;;   (hsv? hsv)
;;; Postconditions:
;;;   0 <= saturation <= 1
(define _hsv-saturation cadr)
(define hsv-saturation 
  (guard-hsv-proc 'hsv-saturation _hsv-saturation))

;;; Procedure:
;;;   hsv-value
;;; Parameters:
;;;   hsv, an HSV color
;;; Purpose:
;;;   Extract the value from an HSV color.
;;; Produces:
;;;   value, a real number
;;; Preconditions:
;;;   (hsv? hsv)
;;; Postconditions:
;;;   0 <= value <= 1
(define _hsv-value caddr)
(define hsv-value
  (guard-hsv-proc 'hsv-value _hsv-value))

