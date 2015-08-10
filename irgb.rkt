#lang racket

; gigls/irgb.rkt
;   Core and additional IRGB functions.

(require "irgb-core"
         gigls/pdb-dbus
         gigls/guard
         gigls/higher)

(provide (all-from-out "irgb-core")
         (all-defined-out))

; +--------------+----------------------------------------------------
; | Constructors |
; +--------------+

;;; Procedure:
;;;   irgb
;;; Parameters:
;;;   red, a integer in the range 0..255
;;;   green, a integer in the range 0..255
;;;   blue, a integer in the range 0..255
;;; Purpose:
;;;   Encode the RGB triplet into a single integer.
;;; Produces:
;;;   color, an integer-encoded rgb color
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (irgb-red rgb) is red
;;;   (irgb-green rgb) is green
;;;   (irgb-blue rgb) is blue
;;; Plus:
;;;   If irgb is provided real numbers as input, it rounds
;;;     them to a nearby integer.
;;;   If irgb is provided a component less than zero, it 
;;;     uses 0.
;;;   If irgb is provided a component greater than 255, it
;;;     uses 255.
(define irgb irgb-new)

; +------------+------------------------------------------------------
; | Predicates |
; +------------+

;;; Procedure:
;;;   irgb?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val can be treated as an integer-encoded RGB value.
;;; Produces:
;;;   is-irgb, a Boolean value
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If is-irgb holds, then val can be used for the various irgb-*
;;;     procedures.
(define irgb? integer?)

; +--------+----------------------------------------------------------
; | Guards |
; +--------+

;;; Procedure:
;;;   guard-irgb-proc
;;; Parameters:
;;;   procname, a symbol
;;;   proc, a one-parameter procedure that expects an integer-encoded
;;;     RGB value
;;; Purpose:
;;;   Creates a version of proc that checks that its parameter
;;;   is an integer-encoded RGB value.
;;; Produces:
;;;   guarded-proc, a procedure
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If val is an integer-encoded RGB value, then
;;;     (guarded-proc val) = (proc val) 
;;;   Otherwise
;;;     (guarded-proc val) reports an error 
(define guard-irgb-proc
  (lambda (procname proc)
    (guard-unary-proc procname proc 'integer-encoded-rgb-color irgb?)))

; +---------------+---------------------------------------------------
; | Miscellaneous |
; +---------------+

;;; Procedure:
;;;   irgb-distance-squared
;;;   rgb-distance-squared [DEPRECATED]
;;; Parameters:
;;;   color1, an integer-encoded RGB color
;;;   color2, an integer-encoded RGB color
;;; Purpose:
;;;   Compute the distance squared between color1 and color2.
;;; Produces:
;;;   dsquared, an integer
;;; Preconditions:
;;;   None.
;;; Postconditions:
;;;   dsquared is the square of the Cartesian difference in a 3D colorspace.
;;; Philosophy:
;;;   It's easy to compute the squared distance.  It takes additional
;;;   (and unnecessary) power to compute the normal distance, so we
;;;   usually just use the squared distance.
(define _irgb-distance-squared
  (let ((square (lambda (x) (* x x))))
    (lambda (color1 color2)
      (+ (square (- (irgb-red color1) (irgb-red color2)))
         (square (- (irgb-green color1) (irgb-green color2)))
         (square (- (irgb-blue color1) (irgb-blue color2)))))))

(define irgb-distance-squared
  (guard-proc 'irgb-distance-squared
              _irgb-distance-squared
              (list 'integer-encoded-rgb-color 'integer-encoded-rgb-color)
              (list irgb? irgb?)))

(define rgb-distance-squared
  (guard-proc 'rgb-distance-squared
              _irgb-distance-squared
              (list 'integer-encoded-rgb-color 'integer-encoded-rgb-color)
              (list irgb? irgb?)))

;;; Procedures:
;;;   irgb-map
;;;   rgb-map [DEPRECATED]
;;; Parameters:
;;;   rgb, an integer-encoded RGB color
;;;   func, a function from components (integers in the range [0..255]) to
;;;     components
;;; Purpose:
;;;   Create a new RGB color by applying func to each component.
;;; Produces:
;;;   new-rgb, an integer-encoded RGB color
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (irgb? new-rgb)
;;;   (irgb-red new-rgb) = (func (irgb-red rgb))
;;;   (irgb-green new-rgb) = (func (irgb-green rgb))
;;;   (irgb-blue new-rgb) = (func (irgb-blue rgb))
(define _irgb-map
  (lambda (color func)
    (irgb (func (irgb-red color))
          (func (irgb-green color))
          (func (irgb-blue color)))))

(define irgb-map 
  (guard-proc 'irgb-map
              _irgb-map
              (list 'irgb-color 'procedure)
              (list irgb? procedure?)))

; +-----------------+-------------------------------------------------
; | Transformations |
; +-----------------+

;;; Procedures:
;;;   irgb-bluer
;;;   rgb-bluer [DEPRECATED]
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   Produce a bluer version of rgb
;;; Produces:
;;;   bluer, an integer-encoded RGB color
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If (irgb-blue) < 255
;;;     (irgb-blue bluer) > (irgb-blue rgb)
;;;   Else
;;;     (irgb-blue bluer) = 255
(define _irgb-bluer
  (lambda (color)
    (irgb (irgb-red color)
          (irgb-green color)
          (min 255 (+ 32 (irgb-blue color))))))

(define irgb-bluer (guard-irgb-proc 'irgb-bluer _irgb-bluer))
(define rgb-bluer (guard-irgb-proc 'rgb-bluer _irgb-bluer))

;;; Procedures:
;;;   irgb-complement
;;; Parameters:
;;;   rgb, an integer-encoded RGB color
;;; Purpose:
;;;   Compute the pseudo-complement of rgb
;;; Produces:
;;;   complement, an integer-encoded RGB color
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (+ (irgb-red rgb) (irgb-red complement)) = 255
;;;   (+ (irgb-green rgb) (irgb-green complement)) = 255
;;;   (+ (irgb-blue rgb) (irgb-blue complement)) = 255
(define _irgb-complement
  (r-s irgb-map (l-s - 255)))

(define irgb-complement (guard-irgb-proc 'irgb-complement _irgb-complement))
(define rgb-complement (guard-irgb-proc 'rgb-complement _irgb-complement))

;;; Procedures:
;;;   irgb-darker
;;;   rgb-darker
;;; Parameters:
;;;   rgb, an integer-encoded RGB color
;;; Purpose:
;;;   Compute a darker version of rgb
;;; Produces:
;;;   darker, an RGB color.
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If (irgb-red rgb) > 0
;;;     (irgb-red darker) < (irgb-red rgb)
;;;   Otherwise
;;;     (irgb-red darker) = 0
;;;   If (irgb-green rgb) > 0
;;;     (irgb-green darker) < (irgb-green rgb)
;;;   Otherwise
;;;     (irgb-green darker) = 0
;;;   If (irgb-blue rgb) > 0
;;;     (irgb-blue darker) < (irgb-blue rgb)
;;;   Otherwise
;;;     (irgb-blue darker) = 0
(define _irgb-darker
  (r-s _irgb-map (o (l-s max 0) (r-s - 16))))

(define irgb-darker (guard-irgb-proc 'irgb-darker _irgb-darker))
(define rgb-darker (guard-irgb-proc 'rgb-darker _irgb-darker))

;;; Procedure:
;;;   irgb-greener
;;;   rgb-greener
;;; Parameters:
;;;   rgb, an integer-encoded RGB color
;;; Purpose:
;;;   Produce a greener version of rgb
;;; Produces:
;;;   greener, an integer-encoded RGB color
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If (irgb-green rgb) < 255
;;;     (irgb-green greener) > (irgb-green rgb)
;;;   Otherwise
;;;     (irgb-green rgb) = 255
(define _irgb-greener
  (lambda (color)
    (irgb (irgb-red color)
          (min 255 (+ 32 (irgb-green color)))
          (irgb-blue color))))

(define irgb-greener (guard-irgb-proc 'irgb-greener _irgb-greener))
(define rgb-greener (guard-irgb-proc 'rgb-greener _irgb-greener))

;;; Procedures:
;;;   irgb-greyscale
;;;   rgb-greyscale [DEPRECATED]
;;; Parameters:
;;;   color, an integer-encoded RGB color
;;; Purpose: 
;;;   Convert rgb to an appropriate shade of grey
;;; Produces:
;;;   grey, an integer-encoded RGB color.
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (irgb-red grey) = (irgb-green grey) = (irgb-blue grey).
;;;   grey has a similar brightness to color.
(define _irgb-greyscale
  (lambda (color)
    (let ((ave (+ (* 0.30 (irgb-red color)) 
                  (* 0.59 (irgb-green color)) 
                  (* 0.11 (irgb-blue color)))))
      (irgb-new ave ave ave))))

(define irgb-greyscale (guard-irgb-proc 'irgb-greyscale _irgb-greyscale))
(define rgb-greyscale (guard-irgb-proc 'rgb-greyscale _irgb-greyscale))

;;; Procedures:
;;;   irgb-lighter
;;;   rgb-lighter [DEPRECATED]
;;; Parameters:
;;;   color, an integer-encoded RGB color
;;; Purpose:
;;;   Compute a lighter version of color
;;; Produces:
;;;   lighter, an integer-encoded RGB color.
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If (irgb-red color) < 255
;;;     (irgb-red lighter) > (irgb-red color)
;;;   Otherwise
;;;     (irgb-red lighter) = 255
;;;   If (irgb-green color) < 255
;;;     (irgb-green lighter) > (irgb-green color)
;;;   Otherwise
;;;     (irgb-green lighter) = 255
;;;   If (irgb-blue color) < 255
;;;     (irgb-blue lighter) > (irgb-blue color)
;;;   Otherwise
;;;     (irgb-blue lighter) = 255
(define _irgb-lighter
  (r-s _irgb-map (o (l-s min 255) (r-s + 16))))

(define irgb-lighter (guard-irgb-proc 'irgb-lighter _irgb-lighter))
(define rgb-lighter (guard-irgb-proc 'rgb-lighter _irgb-lighter))

;;; Procedures:
;;;   irgb-phaseshift
;;;   rgb-phaseshift [DEPRECATED]
;;; Parameters:
;;;   rgb, an integer-encoded RGB color
;;; Purpose:
;;;   'Phase shift' rgb by adding 128 to components less than or equal 
;;;   to 128 and subtracting 128 from components greater than 128.
;;; Produces:
;;;   shifted, an integer-encoded RGB color
(define _irgb-phaseshift
  (r-s _irgb-map (o (r-s modulo 256) (l-s + 128))))

(define irgb-phaseshift (guard-irgb-proc 'irgb-phaseshift _irgb-phaseshift))
(define rgb-phaseshift (guard-irgb-proc 'rgb-phaseshift _irgb-phaseshift))

;;; Procedures:
;;;   irgb-redder
;;;   rgb-redder [DEPRECATED]
;;; Parameters:
;;;   rgb, an integer-encoded RGB color
;;; Purpose:
;;;   Produce a redder version of rgb
;;; Produces:
;;;   redder, an RGB color
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If (irgb-red rgb) < 255
;;;     (irgb-red redder) > (irgb-red rgb)
;;;   Otherwise
;;;     (irgb-red redder) = 255
(define _irgb-redder
  (lambda (color)
    (irgb (min 255 (+ 32 (irgb-red color)))
          (irgb-green color)
          (irgb-blue color))))

(define irgb-redder (guard-irgb-proc 'irgb-redder _irgb-redder))
(define rgb-redder (guard-irgb-proc 'rgb-redder _irgb-redder))

;;; Procedures:
;;;   irgb-rotate
;;;   rgb-rotate [DEPRECATED]
;;; Parameters:
;;;   color, an integer-encoded RGB color
;;; Purpose:
;;;   Compute a 'rotated' version of color
;;; Produces:
;;;   rotated, an integer-encoded RGB color
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (rgb-red rotated) = (rgb-green color)
;;;   (rgb-green rotated) = (rgb-blue color)
;;;   (rgb-blue rotated) = (rgb-red color)
(define _irgb-rotate
  (lambda (color)
    (irgb (irgb-green color)
          (irgb-blue color)
          (irgb-red color))))

(define irgb-rotate (guard-irgb-proc 'irgb-rotate _irgb-rotate))
(define rgb-rotate (guard-irgb-proc 'rgb-rotate _irgb-rotate))

