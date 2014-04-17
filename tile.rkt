#lang racket

; tile.rkt
;   A wrapper for the tile functions

(require "tile-core")		; Functions we're wrapping

(require louDBus/unsafe 	; For loudbus_call
         gigls/pdb-dbus 	; Get access to the PDB
         gigls/guard 	        ; For guarding stuff
         gigls/higher 	        ; For constant
         gigls/image 	        ; For image?
         gigls/drawable 	; For drawable?
         gigls/irgb 		; For irgb-new
 )

(provide (all-defined-out)
         (all-from-out "tile-core"))

; Set up the dbus object to call.
(define gimpplus (loudbus-proxy "edu.grinnell.cs.glimmer.GimpDBus"
                                "/edu/grinnell/cs/glimmer/gimp"
                                "edu.grinnell.cs.glimmer.gimpplus"))

; +----------------+--------------------------------------------------
; | Initialization |
; +----------------+

(tile-core-init loudbus-call gimpplus)

; +----------+--------------------------------------------------------
; | Wrappers |
; +----------+

;(define drawable-fill!
;  (lambda (image drawable color)
;    (display "Filling drawable.\n")
;    (drawable-fill-core loudbus-call gimpplus image drawable color)))

(define drawable-fill!
  (guard-proc 'drawable-fill!
              _drawable-fill!
              (list 'image 'drawable 'irgb-color)
              (list image-id? drawable? integer?)))

(define drawable-recompute!
  (guard-proc 'drawable-recompute!
              _drawable-recompute!
	      (list 'image 'drawable 'pos2color)
	      (list image-id? drawable? (constant #t))))

;;; Procedure:
;;;   image-compute
;;; Parameters:
;;;   pos2color, a function from two integers to a color
;;;   width, a positive integer
;;;   height, a positive integer
;;; Purpose:
;;;   Create a new image
;;; Produces:
;;;   image, an image
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (image-width image) = width
;;;   (image-height image) = height
;;;   For all 0 <= i < width, 0 <= j < height
;;;     (image-get-pixel image i j) = (pos2color i j)
(define _image-compute
  (lambda (pos2color width height)
    (let* ((image (image-new width height))
           (drawable (image-get-layer image)))
      (_drawable-recompute! image drawable pos2color))))
      
(define image-compute
  (guard-proc 'image-compute
              _image-compute
	      (list 'pos2color 'positive-integer 'positive-integer )
	      (list (constant #t)
	            (and integer? exact? positive?)
		    (and integer? exact? positive?))))

;;; Procedure:
;;;   image-transform!
;;; Parameters:
;;;   image, an image
;;;   transform, a function from RGB colors to RGB colors.
;;; Purpose:
;;;   Transform all the pixels in image.
;;; Produces:
;;;   image, the same image, now transformed.
;;; Preconditions:
;;;   IMAGE contains the original pixel values of image.
;;;   For all 0 <= i < (image-width image), 0 <= j < (image-height image)
;;;     (image-get-pixel image i j) = (transform (image-get-pixel IMAGE i j))
(define _image-transform!
  (lambda (image transform)
    (_drawable-transform! image (image-get-layer image) transform)))
      
(define image-transform!
  (guard-proc 'image-transform
              _image-transform!
	      (list 'image 'colorfun)
	      (list image-id? procedure?)))

;;; Procedure:
;;;   image-variant
;;; Parameters:
;;;   image, an image
;;;   transform, a function from RGB to RGB
;;; Purpose:
;;;   To create a variant of image using transform.
;;; Produces:
;;;   variant, an image
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (image-width variant) = (image-width image)
;;;   (image-height variant) = (image-height image)
;;;   For all 0 <= i < (image-width image), 0 <= j < (image-height image)
;;;     (image-get-pixel variant i j) = (transform (image-get-pixel image i j))
(define _image-variant
  (lambda (image transform)
    (let ((result (car (gimp-image-duplicate image))))
      (image-transform! result transform)
      result)))

(define image-variant
  (guard-proc 'image-variant
              _image-variant
	      (list 'image 'colorfun)
	      (list image-id? procedure?)))

