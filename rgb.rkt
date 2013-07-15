#lang racket

(provide (all-defined-out))

(require LoudGimp/guard
         LoudGimp/higher)

; Sam's quick hacks to get RGB working.  This needs to be rewritten in C.


(define rgb-new
  (lambda (r g b)
    (+ (* r 256 256) (* g 256) b)))


;; Procedure:
;;;   rgb-map
;;; Parameters:
;;;   rgb, an RGB color
;;;   func, a function from components (integers in the range [0..255]) to
;;;     components
;;; Purpose:
;;;   Create a new RGB color by applying func to each component.
;; Produces:
;;;   new-rgb an RGB color
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (rgb? new-rgb)
;;;   (rgb-red new-rgb) = (func (rgb-red rgb))
;;;   (rgb-green new-rgb) = (func (rgb-green rgb))
;;;   (rgb-blue new-rgb) = (func (rgb-blue rgb))
(define _rgb-map
  (lambda (rgb func)
    (rgb-new (func (rgb-red rgb))
             (func (rgb-green rgb))
             (func (rgb-blue rgb)))))

(define rgb-map _rgb-map)

(define rgb-blue
  (lambda (color)
    (remainder color 256)))

;;; Procedure:
;;;   rgb-bluer
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   Produce a bluer version of rgb
;;; Produces:
;;;   bluer, an RGB color
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (rgb-blue bluer) >= (rgb-blue rgb)
(define _rgb-bluer
  (lambda (color)
    (rgb-new (rgb-red color)
             (rgb-green color)
             (min 255 (+ 32 (rgb-blue color))))))

(define rgb-bluer (guard-rgb-proc 'rgb-bluer _rgb-bluer))

;;; Procedure:
;;;   rgb-complement
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   Compute the pseudo-complement of rgb
;;; Produces:
;;;   complement, an RGB color
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (+ (rgb-red rgb) (rgb-red complement)) = 255
;;;   (+ (rgb-green rgb) (rgb-green complement)) = 255
;;;   (+ (rgb-blue rgb) (rgb-blue complement)) = 255
(define _rgb-complement
  (r-s rgb-map (l-s - 255)))

(define rgb-complement (guard-rgb-proc 'rgb-complement _rgb-complement))


;;; Procedure:
;;;   rgb-darker
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   Compute a darker version of rgb
;;; Produces:
;;;   darker, an RGB color.
(define _rgb-darker
  (r-s rgb-map (o (l-s max 0) (r-s - 16))))

(define rgb-darker (guard-rgb-proc 'rgb-darker _rgb-darker))

(define rgb-green
  (lambda (color)
    (remainder (quotient color 256) 256)))

;;; Procedure:
;;;   rgb-greener
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   Produce a greener version of rgb
;;; Produces:
;;;   greener, an RGB color
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (rgb-green greener) >= (rgb-green rgb)
(define _rgb-greener
  (lambda (color)
    (rgb-new (rgb-red color)
             (min 255 (+ 32 (rgb-green color)))
             (rgb-blue color))))

(define rgb-greener (guard-rgb-proc 'rgb-greener _rgb-greener))

;;; Procedures:
;;;   rgb-greyscale
;;; Parameters:
;;;   rgb, an rgb color
;;; Purpose: 
;;;   Convert rgb to an appropriate shade of grey
;;; Produces:
;;;   grey, an RGB color.
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (rgb-red grey) = (rgb-green grey) = (rgb-blue grey)
;;;   grey has a similar brightness to rgb
(define _rgb-greyscale
  (lambda (rgb)
    (let ((ave (+ (* 0.30 (rgb-red rgb)) 
                  (* 0.59 (rgb-green rgb)) 
                  (* 0.11 (rgb-blue rgb)))))
      (rgb-new ave ave ave))))

(define rgb-greyscale (guard-rgb-proc 'rgb-greyscale _rgb-greyscale))

;;; Procedure
;;;  rgb->hue
;;; Parmeters:
;;;  col, an rgb color
;;; Purpose:
;;;  Compute the hue of the co(de(Define all-itnlor, in degrees on the color wheel.
;;; Produces:
;;;  hue, a float between 0 and 360.
;;; Preconditions
;;;  color must be a valid rgb color
;;; Postconditions
;;;  After rounding, hue should correspond to the Gimp's hue while 
;;;    examining color.
(define _rgb->hue
  (lambda (rgb)
    (let* ((components (rgb->rgb-list rgb))
           (cmax (apply max components))
           (cmin (apply min components))
           (r (rgb-red rgb))
           (g (rgb-green rgb))
           (b (rgb-blue rgb)))
      (cond
        ((equal? cmax cmin) 0)
        ((and (equal? cmax r) (>= g b)) (* 60 (/ (- g b) (- cmax cmin))))
        ((and (equal? cmax r) (< g b)) (+ (* 60 (/ (- g b) (- cmax cmin))) 360))
        ((equal? cmax g) (+ (* 60 (/ (- b r) (- cmax cmin))) 120))
        ((equal? cmax b) (+ (* 60 (/ (- r g) (- cmax cmin))) 240))))))

(define rgb->hue (guard-rgb-proc 'rgb->hue _rgb->hue))

;;; Procedure:
;;;   rgb-lighter
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   Compute a lighter version of rgb
;;; Produces:
;;;   lighter, an RGB color.
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   lighter is likely to be interpreted as similar to, but lighter than
;;;   rgb.
(define _rgb-lighter
  (r-s rgb-map (o (l-s min 255) (r-s + 16))))

(define rgb-lighter (guard-rgb-proc 'rgb-lighter _rgb-lighter))


;;; Procedure:
;;;   rgb-phaseshift
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   "Phase shift" rgb by adding 128 to components less than or equal 
;;;   to 128 and subtracting 128 from components greater than 128.
;;; Produces:
;;;   shifted, an RGB color
(define _rgb-phaseshift
  (r-s rgb-map (o (r-s modulo 256) (l-s + 128))))

(define rgb-phaseshift (guard-rgb-proc 'rgb-phaseshift _rgb-phaseshift))


(define rgb-red
  (let ((scale (* 256 256)))
    (lambda (color)
      (quotient color scale))))

;;; Procedure:
;;;   rgb-redder
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   Produce a redder version of rgb
;;; Produces:
;;;   redder, an RGB color
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (rgb-red redder) >= (rgb-red rgb)
(define _rgb-redder
  (lambda (color)
    (rgb-new (min 255 (+ 32 (rgb-red color)))
             (rgb-green color)
             (rgb-blue color))))

(define rgb-redder (guard-rgb-proc 'rgb-redder _rgb-redder))

;;; Procedure:
;;;   rgb->hsv
;;; Parmeters:
;;;   rgb, an rgb color
;;; Purpose:
;;;   To convert an rgb color into an hsv color.
;;; Produces:
;;;   hsv, a three-element list containing hue, saturation and value.
;;; Preconditions(d
;;;   rgb must be a valid rgb color.
;;; Postconditions
;;;   hsvcolor contains three floats which (with rounding) roughly correspond 
;;;     to the hsv values given for that color in GIMP.  Values are not 
;;;     rounded to preserve color fidelity when converting from rgb to hsv 
;;;     and then back to rgb again.
;;;  (hsv->rgb hsv) should produce color.
(define _rgb->hsv
  (lambda (color)
    (list (_rgb->hue color) (_rgb->saturation color) (_rgb->value color))))

(define rgb->hsv (guard-rgb-proc 'rgb->hsv _rgb->hsv))

;;; Procedure:
;;;   rgb->rgb-list
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
(define _rgb->rgb-list
  (lambda (rgb)
    (list (rgb-red rgb) (rgb-green rgb) (rgb-blue rgb))))

(define rgb->rgb-list (guard-rgb-proc 'rgb->rgb-list _rgb->rgb-list))

;;; Procedure
;;;  rgb->saturation
;;; Parmeters:
;;;  col, an rgb color
;;; Purpose:
;;;  Compute the saturation of the color.
;;; Produces:
;;;  saturation, a float between 0 and 1.
;;; Preconditions
;;;  color must be a valid rgb color.
;;; Postconditions
;;;  After multiplying by 100 and rounding, saturation should correspond 
;;;    to the Gimp's saturation while examining color.
(define _rgb->saturation
  (lambda (col)
    (let* ((color (_rgb->rgb-list col))
          (cmax (apply max color))
          (cmin (apply min color)))
      (if (equal? cmax 0)
          0
          (- 1 (/ cmin cmax))))))

(define rgb->saturation (guard-rgb-proc 'rgb->saturation _rgb->saturation))

;;; Procedure
;;;   rgb->value
;;; Parmeters:
;;;   col, an rgb color
;;; Purpose:
;;;   To return the value (as in the V in (HSV) of the color.
;;; Produces:
;;;   value, a float between 0 and 1.
;;; Preconditions
;;;   color must be a valid rgb color.
;;; Postconditions
;;;    After multiplying by 100 and rounding, value should correspond to 
;;;    the Gimp's value while examining color.
(define _rgb->value
  (lambda (col)
    (let ((color (_rgb->rgb-list col)))
      (/ (apply max color) 255))))

(define rgb->value (guard-rgb-proc 'rgb->value _rgb->value))

;;; Procedure:
;;;   rgb-rotate
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   Compute a 'rotated' version of rgb
;;; Produces:
;;;   rotated, an RGB color
(define _rgb-rotate
  (lambda (rgb)
    (rgb-new (rgb-green rgb)
             (rgb-blue rgb)
             (rgb-red rgb))))

(define rgb-rotate (guard-rgb-proc 'rgb-rotate _rgb-rotate))

;;; Procedure:
;;;   rgb->string
;;; Parameters:
;;;   color, an rgb color [verified]
;;; Purpose:
;;;   Convert color to a string easy for a novice to read
;;; Produces:
;;;   colorstring, a string of the form R/G/B
;;; Preconditions:
;;;   color is a valid rgb color.  That is, (rgb? color) holds.
;;; Postconditions:
;;;   R is (rgb.red color), G is (rgb.green color), B is (rgb.blue color)
(define _rgb->string
  (lambda (color)
    (string-append (number->string (rgb-red color)) 
                   "/"
		   (number->string (rgb-green color))
		   "/"
		   (number->string (rgb-blue color)))))

(define rgb->string (guard-rgb-proc 'rgb->string _rgb->string))
