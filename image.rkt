#lang racket

(require LoudGimp/gimp-dbus)

(require LoudGimp/guard
         LoudGimp/higher
         LoudGimp/makers)

(require LoudGimp/colors
         LoudGimp/context
         LoudGimp/hacks
         LoudGimp/list
         LoudGimp/rgb
         LoudGimp/utils)

(provide (all-defined-out))


(define WHITE 16777215)

;;; Procedure:
;;;   image?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val is one of the valid image descriptions.
(define image?
  (lambda (val)
    (or (image-id? val)
        (image-name? val))))

;;; Procedure:
;;;   image-blot!
;;; Parameters:
;;;   image, an image
;;;   col, an integer
;;;   row, an integer
;;; Purpose:
;;;   Draw a spot at (col,row) using the current brush and foreground
;;;   color.
;;; Produces:
;;;   image, the modified image.
;;; Preconditions:
;;;   (image? image)
;;;   0 <= col < (image-width image)
;;;   0 <= row < (image-height image)
;;; Postconditions:
;;;   image contains an additional spot at (col,row)
;;;   That spot may not yet be visible.
(define _image-blot!
  (lambda (image x y)
    (gimp-paintbrush-default (image-get-layer image) 2 (vector x y))
    image))

(define image-blot!
  (guard-proc 'image-blot! _image-blot!
              '(image real real)
              (list image? real? real?)))

;;; Procedure:
;;;   image-clear-selection!
;;; Parameters:
;;;   image, a GIMP image
;;; Purpose:
;;;    Clears the current selection in the active layer in the image.
;;; Produces:
;;;    image, the same image
;;; Preconditions:
;;;    image is a valid image
;;; Postconditions:
;;;    All pixels inside the current selection in image are now the 
;;;    background color (or transparent if the active layer has an
;;;    alpha channel)
(define _image-clear-selection!
  (lambda (image)
    (gimp-edit-clear (image-get-layer image))
    image))

(define image-clear-selection!
  (guard-unary-proc 'image-clear-selection!
                    _image-clear-selection!
                    'image
                    image?))

;;; Procedure:
;;;   image-draw-line!
;;; Parameters:
;;;   image, an image
;;;   col1, a real
;;;   row1, a real
;;;   col2, a real
;;;   row2, a real
;;; Purpose:
;;;   Draw a line with the current brush and the foreground color, 
;;;   beginning at (col1,row1) and ending at (col2, row2).
;;; Produces:
;;;   image, the original image
(define _image-draw-line!
  (lambda (image col1 row1 col2 row2)
    (gimp-paintbrush-default (image-get-layer image)
                             4                   
                             (vector col1 row1 col2 row2))
    (cond ((context-immediate-updates?) (context-update-displays!)))
    image))

(define image-draw-line!
  (guard-proc 'image-draw-line!
              _image-draw-line!
              (list 'image 'real 'real 'real 'real)
              (list image? real? real? real? real?)))

;;; Procedure
;;;   image-fill!
;;; Parameters
;;;   image, a gimp image
;;; Purpose
;;;   Fill image's current selection (in the active layer) with the current 
;;;   foreground color
;;; Produces
;;;   [Nothing; called for the side effect]
;;; Preconditions
;;;   image is a valid image
;;; Postconditions
;;;   All the pixels of the active layer in the current selection are filled 
;;;   with the current foreground color
(define _image-fill! 
  (lambda (image)
    (gimp-edit-fill (image-get-layer image) 0)
    (cond ((context-immediate-updates?) (context-update-displays!)))
    image))

(define image-fill!
  (guard-unary-proc 'image-fill! _image-fill! 'image image?))

; [From gimp/image/image-fill-selection.scm]

;;; Procedure
;;;   image-fill-selection!
;;; Parameters
;;;   image, a gimp image
;;; Purpose
;;;   Fill image's current selection (in the active layer) with the current 
;;;   foreground color
;;; Produces
;;;   [Nothing; called for the side effect]
;;; Preconditions
;;;   image is a valid image
;;; Postconditions
;;;   All the pixels of the active layer in the current selection are filled 
;;;   with the current foreground color
(define image-fill-selection! 
  (lambda (image)
    (cond
      ((not (image? image))
       (error "image-fill-selection!: invalid image" image))
      (else 
       (gimp-edit-fill (image-get-layer image) 0)
       image))))

;;; Procedure:
;;;   image-get-layer
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Gets the active layer from the image.
;;; Produces:
;;;   layer, a layer
;;; Postconditions:
;;;   If the image has no active layer, returns #f
(define image-get-layer
  (lambda (image)
    (let ((id (and image (image-id image))))
      (and id
           (let ((active (car (gimp-image-get-active-layer id)))
                 (layers (gimp-image-get-layers id)))
             (if (= active -1)
                 (if (= (car layers) 0)
                     #f
                     (vector-ref (cadr layers) 0))
                active))))))


;;; Procedure:
;;;   image-height
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   get the height of an image
;;; Produces:
;;;   height, an integer
;;; Preconditions:
;;;   image is a valid image
;;; Postconditions:
;;;   heith is the height of image.
(define image-height
  (lambda (image)
    (cond 
      ((not (image? image))
       (error "image-height: image must be a valid image"))
      (else
       (car (gimp-image-height image))))))

;;; Procedure:
;;;   image-id
;;; Parameters:
;;;   image, one of the various forms of images
;;; Purpose:
;;;   Get the id from the image.
;;; Produces:
;;;   imageid, the GIMP id for the image.
;;; Problems:
;;;   Returns #f if it does not seem to be an image.
(define image-id
  (lambda (image)
    (cond
      ((and (integer? image) (image-id? image)) image)
      ((string? image) (image-name->image-id image))
      (else #f))))

;;; Procedure:
;;;   image-id?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine whether or not val is a valid image id
(define image-id?
  (lambda (val)
    (and (integer? val)
         (not (= 0 (car (gimp-image-is-valid val)))))))

;;; Procedure:
;;;   image-name
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Gets the name associated with the image.
;;; Produces:
;;;   name, a string
;;; Preconditions:
;;;   image is an image. That is (image? image) holds.
(define image-name
  (lambda (image)
    (cond
      ((image-id? image) (car (gimp-image-get-name image)))
      ((string? image) image)
      (else #f))))

;;; Procedure:
;;;   image-name?
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   Determines if str names an image
;;; Produces:
;;;   is-image, a boolean
(define image-name?
  (lambda (str)
    (and (string? str) (image-name->image-id str) #t)))

;;; Procedure:
;;;   image-name->image-id
;;; Parameters:
;;;   name, a string
;;; Purpose:
;;;   Find the id of the image associated with the given name.
;;; Produces:
;;;   imageid, that id
;;; Postconditions:
;;;   If there is an image with the given name, imageid is one such image.
;;;     That is (image-name imageid) returns name.
;;;   If there is no such image, imageid is #f.
(define image-name->image-id
  (lambda (name)
    (and (string? name)
         (let* ((stuff (gimp-image-list))
                (len (car stuff))
                (images (cadr stuff)))
           (let kernel ((pos 0))
             (cond
               ((= pos len) #f)
               ((equal? name (image-name (vector-ref images pos)))
                (vector-ref images pos))
               (else (kernel (+ pos 1)))))))))

;;; Procedure:
;;;   image-select-all!
;;; Parameters:
;;;   image, a gimp image
;;; Purpose
;;;   Select all pixels in the image
;;; Produces:
;;;   [Nothing; Called for the side effect]
;;; Preconditions:
;;;   image is a valid image
;;; Postconditions:
;;;   All pixels in image have been selected.
(define image-select-all!
  (lambda (image)
    (let ((id (image-id image)))
      (if id
          (begin (gimp-selection-all id) image)
          (error "image-select-all!: invalid image: " image)))))

;;; Procedure:
;;;   image-select-ellipse!
;;; Parameters:
;;;   image, a gimp image
;;;   operation, one of the valid GIMP operations
;;;   left, an integer
;;;   top, an integer
;;;   width, an integer;
;;;   height, an integer
;;; Purpose:
;;;   Select an ellipse according to the selection mode specfied by 
;;;   operation, inscribed in the rectangle with the given top left corner, 
;;;   and the given width and height.
;;; Produces:
;;;   [Noting; called for the side effect]
;;; Preconditions:
;;;   image is a valid gimp image
;;;   operation is one of: ADD, SUBTRACT, REPLACE, and INTERSECT.
;;;   left, top, width, and height describe an ellipse.
;;; Postconditions
;;;   An appropriate ellipse is selected.
(define image-select-ellipse!
  (lambda (image operation left top width height)
    (image-validate-selection! image operation left top width height
                               "image-select-ellipse!")
    (gimp-ellipse-select image
                         left top
                         width height
                         (selection-op operation)
                         1 1 1)
    ; (context-update-displays!)
    image))

;;; Procedure:
;;;   image-select-nothing!
;;; Parameters:
;;;   image, a gimp image
;;; Purpose:
;;;   De-select all pixels in the image
;;; Produces:
;;;   [Nothing; called for the side effect]
;;; Preconditions:
;;;   image is a valid image
;;; Postconditions:
;;;   No pixels in image are selected.
(define image-select-nothing!
  (lambda (image)
    (cond
      ((not (image? image))
       (error "select-nothing: invalid image"))
      (else
       (gimp-selection-none image)
       image))))

;;; Procedure:
;;;   image-select-rectangle!
;;; Parameters:
;;;   image, a gimp image
;;;   operation, one of ADD, SUBTRACT, INTERSECT, REPLACE
;;;   left, an integer
;;;   top, an integer
;;;   width, an integer
;;;   height, an integer
;;; Purpose:
;;;   Select a rectangle according to the selection mode specfied by 
;;;   operation, beginning in the given top left corner, with the given 
;;;   width and height.
;;; Produces:
;;;   [Nothing; called for the side-effect
;;; Preconditions:
;;;   image is a valid gimp image
;;;    operation is one of: ADD, SUBTRACT, REPLACE, INTERSECT
;;;   left and top are integers, width and height are nonnegative integers
;;;   left, top, width, and height descirbe an area onscreen in image.
;;; Postconditions:
;;;   The given rectangle is now selected.
(define image-select-rectangle!
  (lambda (image operation left top width height)
    (image-validate-selection! image operation left top width height
                               "image-select-rectangle!")
    (gimp-rect-select image
                      left top
                      width height
                      (selection-op operation)
                      0 0)
    (context-update-displays!)
    image))

;;; Procedure:
;;;   image-show
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Displays the image (which may have been modified behind the scenes).
;;; Produces:
;;;   image, the original image.
;; Preconditions:
;;;   image must be a valid image (created by create-image or image-load).
(define _image-show 
  (lambda (image)
    (gimp-display-new (image-id image))
    image))

(define image-show
  (guard-unary-proc 'image-show
                    _image-show
                    'image
                    image?))

;;; Procedure:
;;;   image-new
;;; Parameters:
;;;   width, a positive integer
;;;   height, a positive integer
;;; Purpose:
;;;   Create an image of the given width and height.
;;; Produces:
;;;   img, an encapsulated image
(define _image-new
  (lambda (width height)
    (let* ((image (car (gimp-image-new width height 0)))
           (layer (car (gimp-layer-new image width height 0 "Layer" 100 0))))
      (gimp-image-add-layer image layer 0)
      ; The following line is a temporary hack while I try to figure
      ; stuff out
      ;(let ((bgcolor WHITE))
      ; (rgb-set-region! layer 0 0 width height bgcolor))
      (gimp-selection-all image)
      (gimp-edit-clear layer)
      (gimp-selection-none image)
      image)))

(define image-new
  (guard-proc 'image-new
              _image-new
              (list 'positive-integer 'positive-integer)
              (list (^and integer? positive?)
                    (^and integer? positive?))))


;;; Procedure:
;;;   image-validate-selection!
;;; Purpose:
;;;   Validate the typical parameters to a selection routine, such
;;;   as image.select-ellipse! or image.select-rectangle!.
;;; Parameters:
;;;   image, an image
;;;   operation, one of ADD, SUBTRACT, REPLACE, INTERSECT
;;;   left, an integer
;;;   top, an integer
;;;   width, an integer
;;;   height, an integer
;;;   proc, a string
;;; Produces:
;;;   [Nothing, called for the side effect]
;;; Postconditions:
;;;   If any parameter is invalid, throws an exception.
;;;   Otherwise, it should be safe to do the selection.
(define image-validate-selection!
  (lambda (image operation left top width height proc)
    (let ((crash (lambda (message) (error (string-append proc ": " message)))))
      (cond
        ((not (image? image))
         (crash "invalid image"))
        ((not (member operation (list ADD SUBTRACT REPLACE INTERSECT)))
         (crash "operation must be ADD, SUBTRACT, REPLACE, or INTERSECT" 
                (number->string operation)))
;       ((or (not (integer? left)) (not (integer? top)) 
;            (not (integer? width)) (not (integer? height)))
;        (crash "left, top, width, and height must all be integers"))
        ((or (< width 1) (< height 1))
         (crash "width and height must be at least 1"))
        ((or (>= left (image-width image))
             (>= top (image-height image))
             (>= 0 (+ left width))
             (>= 0 (+ top height)))
         (crash "selection is outside of the bounds of the image"))))))


;;; Procedure:
;;;   image-width
;;; Parameters:
;;;   image, an image
;;; Purpose
;;;   get the width of an image
;;; Produces
;;;   an integer
;;; Preconditions
;;;   image is a valid image
;;; Postconditions
;;;   returns the width of image, an integer
(define image-width
  (lambda (image)
    (cond 
      ((not (image? image))
       (error "image-width: invalid image" image))
      (else
       (car (gimp-image-width image))))))
