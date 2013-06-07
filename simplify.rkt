#lang racket
(require LoudGimp/gimp-dbus)
(require LoudGimp/guard)
(require LoudGimp/higher)

;;; File:
;;;   LoudGimp/simplify
;;; Summary:
;;;   Simplified versions of the PDB procedures, intended
;;;   for beginning programmers.

(provide WHITE
         image-new
         image-show)

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

