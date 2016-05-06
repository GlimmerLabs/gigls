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

;(define drawable-fill!
;  (guard-proc 'drawable-fill!
;              _drawable-fill!
;              (list 'image 'drawable 'irgb-color)
;              (list image-id? drawable? integer?)))

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
(define/contract image-compute
  (-> (-> integer? integer? irgb?)
      (and/c integer? (not/c negative?)) 
      (and/c integer? (not/c negative?))
      image?)
  (lambda (pos2color width height)
    (let* ((image (image-new width height))
           (drawable (image-get-layer image)))
      (drawable-recompute! image drawable pos2color))))
      
;(define image-compute
;  (guard-proc 'image-compute
;              _image-compute
;	      (list 'pos2color 'positive-integer 'positive-integer )
;	      (list (constant #t)
;	            (and integer? exact? positive?)
;		    (and integer? exact? positive?))))

;;; Procedure:
;;;   image-recompute!
;;; Parameters:
;;;   image, an image
;;;   pos2color, a function from two integers to a color
;;; Purpose:
;;;   Recompute selected portions or all of the image
;;; Produces:
;;;   image, the same image, now modified.
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   For all 0 <= i < width, 0 <= j < height
;;;     If i,j is in the selected region (or nothing is selected)
;;;       (image-get-pixel image i j) = (pos2color i j)
(define/contract image-recompute!
  (-> image? (-> integer? integer? irgb?) image?)
  (lambda (image pos2color)
    (let* ([drawable (image-get-layer image)])
      (drawable-recompute! image drawable pos2color))))
      
;(define image-recompute!
;  (guard-proc 'image-recompute!
;              _image-recompute!
;	      (list 'image 'pos2color)
;	      (list image?  procedure?)))

;;; Procedure:
;;;   image-redo!
;;; Parameters:
;;;   image, an image
;;;   fun, a function from two integers and a color to a color
;;; Purpose:
;;;   Redo selected portions or all of the image
;;; Produces:
;;;   image, the same image, now modified.
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   For all 0 <= i < width, 0 <= j < height
;;;     If i,j is in the selected region (or nothing is selected)
;;;       (image-get-pixel image i j) = (fun i j (image-get-pixel original i j))
(define/contract image-redo!
  (-> image? (-> integer? integer? irgb?) image?)
  (lambda (image fun)
    (let* ([drawable (image-get-layer image)])
      (_drawable-redo! image drawable fun))))
      
;(define image-redo!
;  (guard-proc 'image-redo!
;              _image-redo!
;	      (list 'image 'x-y-color-to-color)
;	      (list image?  procedure?)))

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
(define/contract image-transform!
  (-> image? (-> irgb? irgb?) image?)
  (lambda (image transform)
    (drawable-transform! image (image-get-layer image) transform)))
      
;(define image-transform!
;  (guard-proc 'image-transform
;              _image-transform!
;	      (list 'image 'colorfun)
;	      (list image-id? procedure?)))

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
(define/contract image-variant
  (-> image? (-> irgb? irgb?) image?)
  (lambda (image transform)
    (let ((result (car (gimp-image-duplicate image))))
      (image-transform! result transform)
      result)))

;(define image-variant
;  (guard-proc 'image-variant
;              _image-variant
;	      (list 'image 'colorfun)
;	      (list image-id? procedure?)))

;;; Value:
;;;   IRGB-TRANSPARENT
;;; Type:
;;;   integer-encoded RGB color (more or less)
;;; Description:
;;;   A value for image-recompute! to indicate "don't change the color."
;;; Note:
;;;   The definition might belong in tile-core.c.
(define IRGB-TRANSPARENT -1)