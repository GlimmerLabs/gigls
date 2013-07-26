  #lang racket

(require LoudGimp/mgimp)
(require louDBus/unsafe)
(require LoudGimp/gimp-dbus)
(require LoudGimp/rgb-core)
(require LoudGimp/guard
         LoudGimp/higher
         LoudGimp/utils)

(provide (all-defined-out))


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


;;; Procedure:
;;;   color-name?
;;; Parameters:
;;;   val, a string
;;; Purpose:
;;;   Determines if val names a color.
;;; Produces:
;;;   is-color-name?, a boolean
(define color-name?
  (lambda (val)
    (and (string? val)
         (sequence-contains? (context-get-color-names) val))))

;;; Procedure:
;;;   color-name->rgb
;;; Parameters:
;;;   color-name, a string
;;; Purpose:
;;;   Convert a named color to an RGB color.
;;; Produces:
;;;   rgb, an RGB color as an integer
;;; Preconditions:
;;;   (color-name? cname) must hold
;;; NOTE:
;;;   Calls a function implimented as a GIMP plugin
(define color-name->rgb
  (lambda (color-name)
    (car (loudbus-call gimp 'ggimp_rgb_parse color-name))))

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
;;;   context-get-color-names
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Get a vector of all the available color names.
;;; Produces:
;;;   names, a vector of strings
;;; Partners:
;;;   (context-find-color-names "NAME")
;;;      Provides a way to find a list of names that include "NAME".
;;; Included in colors (not context) to avoid interdependencies
(define context-get-color-names mgimp-get-color-names)


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

;;; Procedure:
;;;   rgb-map
;;; Parameters:
;;;   rgb, an RGB color
;;;   func, a function from components (integers in the range [0..255]) to
;;;     components
;;; Purpose:
;;;   Create a new RGB color by applying func to each component.
;;; Produces:
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


;;; Procedure:
;;;   rgb-phaseshift
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   'Phase shift' rgb by adding 128 to components less than or equal 
;;;   to 128 and subtracting 128 from components greater than 128.
;;; Produces:
;;;   shifted, an RGB color
(define _rgb-phaseshift
  (r-s rgb-map (o (r-s modulo 256) (l-s + 128))))

(define rgb-phaseshift (guard-rgb-proc 'rgb-phaseshift _rgb-phaseshift))

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
    (let* ((color (color->rgb-list col))
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
    (let ((color (color->rgb-list col)))
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

