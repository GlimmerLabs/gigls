#lang racket

; gigls/colors.rkt
;   A variety of functions that work with the various representations
;   of colors (irgb, rgb-list, color-name, and hsv).  Primarily contains
;   the color conversion functions.

(require gigls/color-name
         gigls/guard
         gigls/higher
         gigls/hsv
         gigls/irgb
         gigls/mgimp
         gigls/pdb-dbus
         gigls/rgb-list
         gigls/utils)

(provide (all-defined-out))

; +------------+------------------------------------------------------
; | Predicates |
; +------------+

;;; Procedure:
;;;   color?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determines if val is a color in one of the valid representations.
;;; Produces:
;;;   is-color?, a Boolean
;;; Preconditions
;;;   [No additional]
;;; Postconditions
;;;   returns #t if val is a valid kind of color, and #f otherwise.
(define/contract color? 
  (-> any/c boolean?)
  (lambda (val)
    (or (irgb? val) 
        (rgb-list? val) 
        (hsv? val)
        (color-name? val))))

;;; Procedure:
;;;   hue?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determines if val is a hue
;;; Produces:
;;;   is-hue?, a Boolean
;;; Preconditions
;;;   [No additional]
;;; Postconditions
;;;   returns #t if val is a valid hue, and #f otherwise.
(define/contract hue? 
  (-> any/c boolean?)
  (lambda (val)
    (and (number? val)
         (< val 360)
         (> val 0))))

;;; Procedure:
;;;   saturation?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determines if val is between 0 and 1
;;; Produces:
;;;   is-between?, a Boolean
;;; Preconditions
;;;   [No additional]
;;; Postconditions
;;;   returns #t if val is betwen 0 and 1, and #f otherwise.
(define/contract 0val1? 
  (-> any/c boolean?)
  (lambda (val)
    (and (number? val)
         (< val 1)
         (> val 0))))

; +--------+----------------------------------------------------------
; | Guards |
; +--------+

;;; Procedure:
;;;   guard-color-proc
;;; Parameters:
;;;   procname, a symbol
;;;   proc, a one-parameter procedure that expects a color
;;; Purpose:
;;;   Creates a version of proc that checks that its parameter
;;;   is a color
;;; Produces:
;;;   guarded-proc, a procedure
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If val is a color value, then
;;;     (guarded-proc val) = (proc val)
;;;   Otherwise
;;;     (guarded-proc val) reports an error
;(define guard-color-proc
;  (lambda (procname proc)
;    (guard-unary-proc procname proc 'color color?)))

; +---------------------------+---------------------------------------
; | Color Conversion: General |
; +---------------------------+

;;; Procedure:
;;;   color->color-name
;;; Parameters:
;;;   color, one of the many permitted forms of colors.
;;; Purpose:
;;;   Convert color to the name of the nearest color
;;; Produces:
;;;   color-name, a string
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   When rendered, color-name and color produce similar colors.
(define/contract color->color-name
  (-> color? string?)
  (lambda (color)
    (cond
      [(color-name? color)
       color]
      [else
       (irgb->color-name (color->irgb color))])))

;(define color->color-name
;  (lambda (color)
;    (cond
;      ; Note: We violate the normal "check preconditions first" order
;      ; for efficiency.  This causes a bit of code duplication.
;      [(color-name? color)
;       color]
;      [(not (color? color))
;       (error/parameter-type 'color->color-name 1 'color (list color))]
;      [else
;       (_color->color-name color)])))

;;; Procedure:
;;;   color->hsv
;;; Parameters:
;;;   color, one of the many permitted forms of colors.
;;; Purpose:
;;;   Convert color to an hsv color.
;;; Produces:
;;;   hsv, an HSV color
(define/contract color->hsv
  (-> color? hsv?)
  (lambda (color)
    (cond
      [(hsv? color)
       color]
      [(not (color? color))
       (error/parameter-type 'color->hsv 1 'color (list color))]
      [else
       (irgb->hsv (color->irgb color))])))

;(define color->hsv _color->hsv)

;;; Procedure:
;;;   color->irgb
;;;   color->rgb [DEPRECATED]
;;; Parameters:
;;;   color, a color
;;; Purpose:
;;;   Convert any form of color to an integer-encoded RGB color.
;;; Produces:
;;;   rgb-color, a color
;;; Preconditions:
;;;   color must be a valid color (color-name, irgb, rgb-list, ...)
;;; Postconditions:
;;;   rgb-color has the same components as color
(define/contract color->irgb
  (-> color? irgb?)
  (lambda (color)
    (cond
      [(irgb? color) 
       color]
      [(hsv? color) 
       (hsv->rgb color)]
      [(rgb-list? color) 
       (rgb-list->rgb color)]
      [(color-name? color) 
       (color-name->rgb color)]
      [else
       (error/parameter-type 'color->irgb 1 'color (list color))])))

;(define color->irgb _color->irgb)
(define color->rgb color->irgb)

;;; Procedure:
;;;   color->rgb-list
;;; Parameters:
;;;   color, a color
;;; Purpose:
;;;   Convert a color (in either list or vector form) to list form. 
;;; Produces:
;;;   rgb-list, a list of three values
;;; Preconditions:
;;;  color is a valid color [unverified]
(define/contract color->rgb-list
  (-> color? list?)
  (lambda (color)
    (if (rgb-list? color) 
        color
        (irgb->rgb-list (color->irgb color)))))

;(define color->rgb-list _color->rgb-list)

;;; Procedure:
;;;   color->string
;;; Parameters:
;;;   color, a color [verified]
;;; Purpose:
;;;   Convert color to a string easy for a novice to read
;;; Produces:
;;;   colorstring, a string of the form R/G/B
;;; Preconditions:
;;;   color is a valid rgb color.  That is, (rgb? color) holds.
;;; Postconditions:
;;;   R is (rgb.red color), G is (rgb.green color), B is (rgb.blue color)
(define/contract color->string
  (-> color? string?)
  (lambda (color)
    (irgb->string (color->irgb color))))


; +------------------------------------+------------------------------
; | Color Conversion: From Color Names |
; +------------------------------------+

;;; Procedure:
;;;   color-name->irgb
;;;   color-name->rgb
;;; Parameters:
;;;   color-name, a string
;;; Purpose:
;;;   Convert a named color to an RGB color.
;;; Produces:
;;;   rgb, an RGB color as an integer
;;; Preconditions:
;;;   (color-name? cname) must hold
;;; Process 
;;;   Calls a function implimented as a GIMP plugin
(define/contract color-name->irgb
  (-> string? irgb?)
  (lambda (color-name)
    (car (ggimp-rgb-parse color-name))))

;(define color-name->irgb
;  (guard-color-name-proc 'color-name->irgb _color-name->irgb))
(define color-name->rgb color-name->irgb)

; +-----------------------------------+-------------------------------
; | Color Conversion: From HSV Colors |
; +-----------------------------------+

;;; Procedures:
;;;   hsv->irgb
;;;   hsv->rgb [DEPRECATED]
;;; Parmeters:
;;;   hsv, an hsv color
;;; Purpose:
;;;   Convert an hsv color to an rgb color.
;;; Produces:
;;;   rgb, an rgb color.
;;; Preconditions:
;;;   (hsv? hsv)
;;; Postconditions:
;;;   (rgb? rgb)
;;;   (hsv->rgb (irgb->hsv rgb)) should be close to rgb.
(define/contract hsv->irgb
  (-> hsv? irgb?)
  (lambda (hsv)
    (let* ([h (hsv-hue hsv)]
           [s (hsv-saturation hsv)]
           [v (hsv-value hsv)]
           [hi (inexact->exact (mod (floor (/ h 60)) 6))]
           [f (- (/ h 60) hi)]
           [p (* v (- 1 s))]
           [q (* v (- 1 (* f s)))]
           [t (* v (- 1 (* s (- 1 f))))])
      (cond
        [(equal? hi 0) 
         (irgb (* 255 v) (* 255 t) (* 255 p))]
        [(equal? hi 1) 
	 (irgb (* 255 q) (* 255 v) (* 255 p))]
        [(equal? hi 2) 
	 (irgb (* 255 p) (* 255 v) (* 255 t))]
        [(equal? hi 3) 
	 (irgb (* 255 p) (* 255 q) (* 255 v))]
        [(equal? hi 4) 
 	 (irgb (* 255 t) (* 255 p) (* 255 v))]
        [(equal? hi 5) 
	 (irgb (* 255 v) (* 255 p) (* 255 q))]
	[else 0]))))

;(define hsv->irgb
;  (guard-hsv-proc 'hsv->irgb _hsv->irgb))
(define hsv->rgb hsv->irgb)

; +---------------------------------------------------+---------------
; | Color Conversion: From Integer-Encoded RGB Colors |
; +---------------------------------------------------+

;;; Procedure:
;;;   irgb->color-name
;;;   rgb->color-name [DEPRECATED]
;;; Parameters:
;;;   color, an rgb color
;;; Purpose:
;;;   Find the name of a color that is similar to the given color.
;;; Produces:
;;;   name, a string
;;; Preconditions:
;;;   We can connect to GIMP to get the list of colors.
;;; Postconditions:
;;;   There is no color name k for which
;;;     (irgb-distance-squared color (name->color k)
;;;        < (irgb-distance-squared color (name->color name)
(define/contract irgb->color-name
  (-> irgb? string)
  (let* ((color-names (cadr (ggimp-rgb-list)))
         (color-values (map color-name->rgb color-names))
         (distance irgb-distance-squared))
    (lambda (rgb)
      (let kernel ((guess-name (car color-names))
                   (guess-distance (distance rgb (car color-values)))
                   (remaining-names (cdr color-names))
                   (remaining-values (cdr color-values)))
               (if (null? remaining-names)
                   guess-name
                   (let ((next-distance
                         (distance rgb (car remaining-values))))
                     (if (< next-distance guess-distance)
                         (kernel (car remaining-names)
                                 next-distance
                                 (cdr remaining-names)
                                 (cdr remaining-values))
                         (kernel guess-name
                                 guess-distance
                                 (cdr remaining-names)
                                 (cdr remaining-values)))))))))

;(define irgb->color-name (guard-irgb-proc 'irgb->color-name _irgb->color-name))
;(define rgb->color-name (guard-irgb-proc 'rgb->color-name _irgb->color-name))

;;; Procedures:
;;;  irgb->hue
;;;  rgb->hue [DEPRECTED]
;;; Parmeters:
;;;  col, an integer-encoded rgb color
;;; Purpose:
;;;  Compute the hue of the color, in degrees on the color wheel.
;;; Produces:
;;;  hue, a float between 0 and 360.
;;; Preconditions:
;;;  [No additional]
(define/contract irgb->hue
  (-> irgb? hue?)
  (lambda (rgb)
    (let* ([r (irgb-red rgb)]
           [g (irgb-green rgb)]
           [b (irgb-blue rgb)]
           [components (list r g b)]
           [cmax (apply max components)]
           [cmin (apply min components)]
           [chroma (- cmax cmin)])
      (cond
        [(zero? chroma)
         0]
        [(and (equal? cmax r) (>= g b))
         (* 60 (/ (- g b) chroma))]
        [(and (equal? cmax r) (< g b))
         (+ (* 60 (/ (- g b) chroma)) 360)]
        [(equal? cmax g)
         (+ (* 60 (/ (- b r) chroma)) 120)]
        [(equal? cmax b)
         (+ (* 60 (/ (- r g) chroma)) 240)]))))

;(define irgb->hue (guard-irgb-proc 'irgb->hue _irgb->hue))
(define rgb->hue irgb->hue)

;;; Procedures:
;;;   irgb->hsv
;;;   rgb->hsv [DEPRECATED]
;;; Parmeters:
;;;   color, an integer-encoded rgb color
;;; Purpose:
;;;   To convert an integer-encoded rgb color into an hsv color.
;;; Produces:
;;;   hsv, an HSV color 
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (hsv->irgb hsv) is approximately color
(define/contract irgb->hsv
  (-> color? hsv?)
  (lambda (color)
    (list (irgb->hue color) (irgb->saturation color) (irgb->value color))))

;(define irgb->hsv (guard-irgb-proc 'irgb->hsv _irgb->hsv))
(define rgb->hsv irgb->hsv)

;;; Procedures:
;;;   irgb->rgb-list
;;;   rgb->rgb-list [DEPRECATED]
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   Extract the components and shove 'em in a list.
;;; Produces:
;;;   rgb-list, an RGB list.
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   The components of rgb-list are the same as those of rgb.
(define/contract irgb->rgb-list
  (-> irgb? (listof irgb?))
  (lambda (rgb)
    (list (irgb-red rgb) (irgb-green rgb) (irgb-blue rgb))))

;(define irgb->rgb-list (guard-irgb-proc 'irgb->rgb-list _irgb->rgb-list))
(define rgb->rgb-list irgb->rgb-list)

;;; Procedures:
;;;   rgb->saturation
;;;   irgb->saturation [DEPRECATED]
;;; Parmeters:
;;;   col, an integer-encoded rgb color
;;; Purpose:
;;;   Compute the saturation of the color.
;;; Produces:
;;;   saturation, a float between 0 and 1.
;;; Preconditions:
;;;   [No additional]
(define/contract irgb->saturation
  (-> irgb? 0val1?)
  (lambda (col)
    (let* ((color (color->rgb-list col))
          (cmax (apply max color))
          (cmin (apply min color)))
      (if (equal? cmax 0)
          0
          (- 1 (/ cmin cmax))))))

;(define irgb->saturation (guard-irgb-proc 'irgb->saturation _irgb->saturation))
(define rgb->saturation irgb->saturation)

;;; Procedure:
;;;   irgb->string
;;;   rgb->string [DEPRECATED]
;;; Parameters:
;;;   color, an integer-encoded RGB color [verified]
;;; Purpose:
;;;   Convert color to a string easy for a novice to read
;;; Produces:
;;;   colorstring, a string of the form "R/G/B"
;;; Preconditions:
;;;   color is an integer-encoded RGB color
;;; Postconditions:
;;;   R is (irgb-red color), G is (irgb-green color), B is (irgb-blue color)
(define/contract irgb->string
  (-> irgb? string?)
  (lambda (color)
    (string-append (number->string (irgb-red color)) 
                   "/"
		   (number->string (irgb-green color))
		   "/"
		   (number->string (irgb-blue color)))))

;(define irgb->string (guard-irgb-proc 'irgb->string _irgb->string))
(define rgb->string irgb->string)

;;; Procedures:
;;;   irgb->value
;;;   rgb->value [DEPRECATED]
;;; Parmeters:
;;;   col, an integer-encoded rgb color
;;; Purpose:
;;;   To return the value (as in the V in (HSV) of the color.
;;; Produces:
;;;   value, a real number between 0 and 1.
;;; Preconditions:
;;;   [No additional]
(define/contract irgb->value
  (-> irgb? 0val1?)
  (lambda (col)
    (let ((color (color->rgb-list col)))
      (/ (apply max color) 255))))

;(define irgb->value (guard-irgb-proc 'irgb->value _irgb->value))
(define rgb->value irgb->value)

; +----------------------------------+--------------------------------
; | Color Conversion: From RGB Lists |
; +----------------------------------+

;;; Procedure:
;;;   rgb-list->irgb
;;;   rgb-list->rgb
;;; Parameters:
;;;   color, an rgb-list
;;; Purpose:
;;;   Convert color to an rgb color.
;;; Preconditions:
;;;   color must be an rgb-list.  That is, it must be a list of three
;;;     integers, all in the range [0..255].
;;; Postconditions:
;;;   rgb represents the same color as color.
(define/contract rgb-list->irgb
  (-> (listof irgb?) irgb?)
  (lambda (color)
    (irgb (car color) (cadr color) (caddr color))))

;(define rgb-list->irgb
;  (guard-rgb-list-proc 'rgb-list->irgb _rgb-list->irgb))
(define rgb-list->rgb rgb-list->irgb)

; +------------------------+------------------------------------------
; | Misc. Color Procedures |
; +------------------------+

;;; Procedure:
;;;   color-representation
;;; Parameters:
;;;   color, a color
;;; Purpose:
;;;   Determine what representation is used for color
;;; Produces:
;;;   representation, a symbol (or #f)
(define/contract color-representation
  (-> color? boolean?)
  (lambda (color)
    (cond
      ((irgb? color) 'IRGB)
      ((rgb-list? color) 'RGB-LIST)
      ((hsv? color) 'HSV)
      ((color-name? color) 'COLOR-NAME)
      (else #f))))

;(define color-representation _color-representation)

