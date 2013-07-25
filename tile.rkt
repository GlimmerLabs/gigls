#lang racket

; tile.rkt
;   A wrapper for the tile functions

(require "tile-core")		; Functions we're wrapping

(require louDBus/unsafe 	; For loudbus_call
         LoudGimp/gimp-dbus 	; Get access to the PDB
         LoudGimp/guard 	; For guarding stuff
         LoudGimp/higher 	; For constant
         LoudGimp/image 	; For image?
         LoudGimp/drawable 	; For drawable?
         LoudGimp/irgb 		; For irgb-new
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

(define _image-transform!
  (lambda (image transform)
    (_drawable-transform! image (image-get-layer image) transform)))
      
(define image-transform!
  (guard-proc 'image-transform
              _image-transform!
	      (list 'image 'colorfun)
	      (list image-id? (constant #t))))
