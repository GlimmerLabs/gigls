#lang racket

; swatch.rkt 
;   Make simple swatches

(require gigls/guard		; For various guarding proceducres
         gigls/colors		; For various color functions
         gigls/tile             ; For image-compute
         gigls/image		; For image-new
         gigls/higher		; For all
         gigls/hacks		; For REPLACE
         gigls/context		; For context-set-fgcolor!
 )

(provide (all-defined-out))

; +--------------------+---------------------------------------------
; | Exported Functions |
; +--------------------+

;;; Procedure:
;;;   color-swatch
;;; Parameters:
;;;   color1, a color
;;;   color2, a color [optional]
;;;   color3, a color [optional]
;;;   color4, a color [optional]
;;;   color5, a color [optional]
;;;   color6, a color [optional]
;;; Purpose:
;;;   Create a "color swatch", an image that shows the colors
;;; Produces:
;;;   swatch, an image
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   The only colors in swatch are color1 ... color6.
;;;   All of the specified colors are in swatch
(define/contract color-swatch
  (->* (color?) (color? color? color? color? color?) image?)
  (let ((width 60)
        (height 16))
    (lambda colors
      (let* [(len (length colors))
             (wid (/ width len))
             (rgbs (map color->irgb colors))]
        (image-compute (lambda (col row)
                         (list-ref rgbs (quotient col wid)))
                       width height)))))

;(define color-swatch
;  (lambda colors
;    (cond
;      ((null? colors)
;       (error/arity 'image-color-swatch "1 to 6" colors))
;      ((< 6 (length colors))
;       (error/arity 'image-color-swatch "1 to 6" colors))
;      ((not (all color? colors))
;       (error/misc 'image-color-swatch "encountered non-color" colors))
;      (else 
;       (apply _color-swatch colors)))))

;;; Procedure:
;;;   color-grid
;;; Parameters:
;;;   box-width, a positive integer
;;;   box-height, a positive integer
;;;   cols, a positive integer
;;;   color1 ... colorn, colors in irgb or color-name form
;;; Purpose:
;;;   Create a simple grid of colors, which each "box" in the grid in the given width
;;;   and height
;;; Produces:
;;;   grid, an image
;;; Preconditions:
;;;   cols >= n
;;; Postconditions:
;;;   grid contains the described grid.
(define/contract color-grid 
  (->* ((and/c integer? positive?) (and/c integer? positive?) (and/c integer? positive?) color?)
       () #:rest (listof color?) image?)
  (lambda (box-width box-height cols . colors)
    (let* ([ncolors (length colors)]
           [lastrow (modulo ncolors cols)]
           [rows (if (zero? lastrow) 
                     (quotient ncolors cols) 
                     (+ 1 (quotient ncolors cols)))]
           [grid (image-new (* cols box-width) (* rows box-height))])
      (let kernel ([remaining colors]
                   [col 0]
                   [row 0])
        (when (not (null? remaining))
          (image-select-rectangle! grid REPLACE
                                   (* col box-width) (* row box-height)
                                   box-width box-height)
          (context-set-fgcolor! (car remaining))
          (image-fill! grid)
          (let ([newcol (modulo (+ col 1) cols)])
            (kernel (cdr remaining)
                    newcol
                    (if (zero? newcol) (+ row 1) row)))))
      (image-select-nothing! grid)
      grid)))

;(define color-grid
;  (lambda (box-width box-height cols . colors)
;    (let ([params (cons box-width (cons box-height (cons cols colors)))])
;      (cond
;        [(or (not (integer? box-width)) (not (positive? box-width)))
;         (error/parameter-type 'color-grid 1 'positive-integer params)]
;        [(or (not (integer? box-height)) (not (positive? box-height)))
;         (error/parameter-type 'color-grid 2 'positive-integer params)]
;        [(or (not (integer? cols)) (not (positive? cols)))
;         (error/parameter-type 'color-grid 3 'positive-integer params)]
;        [(null? colors) 
;         (error "color-grid: Expects at least one color")]
;        [(not (pair? (car colors)))
;         (color-grid box-width box-height cols colors)]
;        [(not (all color? (car colors)))
;         (error "color-grid: Non-color in" (car colors))]
;        [else
;         (_color-grid box-width box-height cols (car colors))]))))
