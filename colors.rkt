#lang racket

(require LoudGimp/rgb)
(require LoudGimp/utils)
(require LoudGimp/guard)

(provide (all-defined-out))


;;; Procedure:
;;;   guard-rgb-proc
;;; Parameters:
;;;   name, a symbol
;;;   proc, a procedure of the form (lambda (rgb) ___)
;;; Purpose:
;;;   Build a new version of proc that checks preconditions.
;;; Produces:
;;;   guarded-proc, a procedure of the form (lambda (rgb) _____)
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If (rgb? val), (guarded-proc val) = (proc val)
;;;   Otherwise, (guarded-proc val) throws an appropriate error
(define guard-rgb-proc
  (lambda (name proc)
    (lambda params
      (validate-unary! name params)
      (validate-param! name 'rgb rgb? params)
      (proc (car params)))))

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
(define color? 
  (lambda (val)
    (or (rgb? val) 
        (rgb-list? val) 
        (hsv? val))))

; [From mscm/color/color-representation.scm]

;;; Procedure:
;;;   color-representation
;;; Parameters:
;;;   color, a color
;;; Purpose:
;;;   Determine what representation is used for color
;;; Produces:
;;;   representation, a symbol (or #f)
(define color-representation
  (lambda (color)
    (cond
      ((rgb? color) 'RGB)
      ((rgb-list? color) 'RGB-LIST)
      ((hsv? color) 'HSV)
      (else #f))))

; [From mscm/color/color-to-hsv.scm]

;;; Procedure:
;;;   color->hsv
;;; Parameters:
;;;   color, one of the many permitted forms of colors.
;;; Purpose:
;;;   Convert color to an hsv color.
;;; Produces:
;;;   hsv, an HSV color
(define color->hsv
  (lambda (color)
    (cond
      ((not (color? color))
       (error "color->hsv: invalid color" color))
      ((hsv? color)
       color)
      (else
       (rgb->hsv (color->rgb color))))))

; [From mscm/color/color-to-rgb.scm]

;;; Procedure:
;;;   color->rgb
;;; Parameters:
;;;   color, a color
;;; Purpose:
;;;   Convert any form of color to rgb.
;;; Produces:
;;;   rgb-color, a color
;;; Preconditions:
;;;   color must be a valid color (color-name, rgb, rgb-list, ...)
;;; Postconditions:
;;;   rgb-color has the same components as color
(define color->rgb
  (lambda (color)
    (cond
      ((rgb? color) color)
      ((hsv? color) (hsv->rgb color))
      ((rgb-list? color) (rgb-list->rgb color))
      (else
       (error "Unknown type of color" color)))))

; [From mscm/color/color-to-rgb-list.scm]

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
(define color->rgb-list
  (lambda (color)
    (if (rgb-list? color) 
        color
        (rgb->rgb-list (color->rgb color)))))

; [From mscm/color/color-to-string.scm]

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
(define color->string
  (lambda (color)
    (rgb->string (color->rgb color))))


;;; Procedure:
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
(define rgb-list->rgb
  (lambda (color)
    (rgb-new (car color) (cadr color) (caddr color))))

; [From mscm/rgb-list/rgb-list-p.scm]

;;; Procedure
;;;   rgb-list?
;;; Parameters
;;;   val, a scheme value
;;; Purpose
;;;   Check if val is an rgb color
;;; Produces
;;;   is-rgb-list, a boolean value
;;; Preconditions
;;;   [none]
;;; Postconditions
;;;   Returns #t if val is an rgb color represented as a list of
;;;     the three components.
;;;   Returns #f otherwise.
(define rgb-list?
  (lambda (val)
    (and (list? val) (equal? (length val) 3) (all-integer? val))))

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
         (integer? (car val))
         (<= 0 (car val) 360)
         (real? (cadr val))
         (<= 0 (cadr val) 1)
         (real? (caddr val))
         (<= 0 (caddr val) 1))))

;;; Procedure:
;;;   rgb->hsv
;;; Parmeters:
;;;   rgb, an rgb color
;;; Purpose:
;;;   To convert an rgb color into an hsv color.
;;; Produces:
;;;   hsv, a three-element list containing hue, saturation and value.
;;; Preconditions
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


;;; Procedure
;;;   hsv->rgb
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
;;;   (hsv->rgb (rgb->hsv rgb)) should be close to rgb.
(define hsv->rgb
  (lambda (hsv)
    (let* ((h (hsv-hue hsv))
           (s (hsv-saturation hsv))
           (v (hsv-value hsv))
           (hi (mod (floor (/ h 60)) 6))
           (f (- (/ h 60) hi))
           (p (* v (- 1 s)))
           (q (* v (- 1 (* f s))))
           (t (* v (- 1 (* s (- 1 f))))))
      (cond
        ((equal? hi 0) (rgb-new (* 255 v) (* 255 t) (* 255 p)))
        ((equal? hi 1) (rgb-new (* 255 q) (* 255 v) (* 255 p)))
        ((equal? hi 2) (rgb-new (* 255 p) (* 255 v) (* 255 t)))
        ((equal? hi 3) (rgb-new (* 255 p) (* 255 q) (* 255 v)))
        ((equal? hi 4) (rgb-new (* 255 t) (* 255 p) (* 255 v)))
        ((equal? hi 5) (rgb-new (* 255 v) (* 255 p) (* 255 q)))))))


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

; [From mscm/rgb/rgb-to-hue.scm]

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

; [From mscm/rgb/rgb-to-saturation.scm]

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
    (let* ((color (color->rgb-list col))
          (cmax (apply max color))
          (cmin (apply min color)))
      (if (equal? cmax 0)
          0
          (- 1 (/ cmin cmax))))))

(define rgb->saturation (guard-rgb-proc 'rgb->saturation _rgb->saturation))

; [From mscm/rgb/rgb-to-value.scm]

;;; Procedure
;;;   rgb->value
;;; Parmeters:
;;;   col, an rgb color
;;; Purpose:
;;;   To return the value (as in the V in HSV) of the color.
;;; Produces:
;;;   value, a float between 0 and 1.
;;; Preconditions
;;;   color must be a valid rgb color.
;;; Postconditions
;;;    After multiplying by 100 and rounding, value should correspond to 
;;;    the Gimp's value while examining color.
(define _rgb->value
  (lambda (col)
    (let ((color (color->rgb-list col)))
      (/ (apply max color) 255))))


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
(define hsv-hue car)

; [From mscm/hsv/hsv-saturation.scm]

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
(define hsv-saturation cadr)

; [From mscm/hsv/hsv-value.scm]

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
(define hsv-value caddr)
