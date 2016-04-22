#lang racket

(require gigls/pdb-dbus)

(require gigls/guard
         gigls/higher
         gigls/makers
         gigls/mgimp)

(require gigls/colors
         gigls/context
         gigls/hacks
         gigls/irgb
         gigls/list
         gigls/point
         gigls/utils)

(provide (all-defined-out))

;;; Procedure:
;;;   image-id?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine whether or not val is a valid image id
(define/contract image-id?
  (-> any/c boolean?)
  (lambda (val)
    (and (integer? val)
         (not (= 0 (car (gimp-image-is-valid val)))))))

;;; Procedure:
;;;   image-name?
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   Determines if str names an image
;;; Produces:
;;;   is-image, a boolean
(define/contract image-name?
  (-> string? boolean?)
  (lambda (str)
    (and (string? str) (image-name->image-id str) #t)))


;;; Procedure:
;;;   image?
;;; Parameters:
;;;   val, a Scheme value
;;; Produces
;;;   is-image, a boolean
;;; Purpose:
;;;   Determine if val is one of the valid image descriptions.
(define/contract image?
  (-> any/c boolean?)
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
(define/contract image-blot!
  (-> image? integer? integer? image?)
  (lambda (image x y)
    (gimp-paintbrush-default (image-get-layer image) 2 (vector x y))
    image))

;(define image-blot!
;  (guard-proc 'image-blot! _image-blot!
;              '(image real real)
;              (list image? real? real?)))

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
(define/contract image-clear-selection!
  (-> image? image?)
  (lambda (image)
    (gimp-edit-clear (image-get-layer image))
    image))

;(define image-clear-selection!
;  (guard-unary-proc 'image-clear-selection!
;                    _image-clear-selection!
;                    'image
;                    image?))

;;; Procedure:
;;;   image-copy-paste-block!
;;; Parameters:
;;;   source, an image id
;;;   source-col, an integer
;;;   source-row, an integer
;;;   target, an image id
;;;   target-col, an integer
;;;   target-row, an integer
;;;   width, an integer
;;;   height, an integer
;;; Purpose:
;;;   Copies a width-x-height block from source to target, with the top-left of
;;;   each block as specified.
;;; Produces:
;;;   [Nothing; called for the side effect.]
;;; Problems:
;;;   Need to deal with out-of-bounds issues.  
;;;     (See paste-buffer! in newgrid.scm for an approach.)
(define/contract image-copy-paste-block!
  (-> image-id? integer? 
      integer? image-id? 
      integer? integer? 
      integer? integer?
      image?)
  (lambda (source source-col source-row 
           target target-col target-row 
           width height)
    (image-select-rectangle! source REPLACE source-col source-row width height)
    (gimp-edit-copy (image-get-layer source))
    (image-select-rectangle! target REPLACE target-col target-row width height)
    (gimp-floating-sel-anchor 
     (car (gimp-edit-paste (image-get-layer target) 0)))
    (image-select-nothing! source)
    (image-select-nothing! target)))

;;; Name:
;;;   arrow-types
;;; Type:
;;;   list-of-symbols
;;; Value:
;;;   A list of all the valid arrow types
(define arrow-types
  (list 'lines 'hollow 'filled 'pointy 'hollow-pointy))

;;; Procedure:
;;;   arrow-type?
;;; Parameters:
;;;   type, a symbol
;;; Purpose:
;;;   Determine if type is one of the valid arrow types
;;; Produces:
;;;   valid?, a Boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If type is a valid type, valid? is #t.
;;;   Otherwise, valid? is #f.
(define/contract arrow-type?
  (-> symbol? boolean?)
  (r-s member? arrow-types))

;;; Procedure:
;;;   image-draw-arrow!
;;; Parameters:
;;;   image, an image
;;;   type, a symbol
;;;   from-col, a real number
;;;   from-row, a real number
;;;   to-col, a real number
;;;   to-row, a real number
;;;   head-width, a positive real number
;;;   head-length, a positive real number
;;; Purpose:
;;;   Draw an arrow from (from-col,from-row) to (to-col,to-row), with a 
;;;   head of the specified width and length.
;;; Produces:
;;;   image, the same image (now updated with an arrow)
;;; Preconditions:
;;;   (from-col,from-row) != (to-col,to-row)
;;;   type must be one of 'lines, 'filled, 'hollow, 'pointy, and
;;;     'hollow-pointy
(define/contract image-draw-arrow!
  (-> image? symbol? real? real? real? real?
      (and/c real? positive?) (and/c real? positive?) image?)
  (lambda (image type from-col from-row to-col to-row head-width head-length)
    (let* ((delta-col (- to-col from-col))
           (delta-row (- to-row from-row))           
           (distance (sqrt (+ (* delta-col delta-col) (* delta-row delta-row))))
           (scale-col (/ delta-col distance))
           (scale-row (/ delta-row distance))
           (half-width (* head-width 0.5))
           ; (c0,r0) gives a point head-length back along the line
           (c0 (- to-col (* scale-col head-length)))
           (r0 (- to-row (* scale-row head-length)))
           ; hoff and voff give horizontal and vertical offsets to
           ; arrow tail points
           (hoff (* scale-row half-width))
           (voff (* scale-col half-width))
           ; (c1,r1) and (c2,r2)
           (c1 (+ c0 hoff))
           (r1 (- r0 voff))
           (c2 (- c0 hoff))
           (r2 (+ r0 voff))
           ; (c3,r3) gives a point halfway to c0 r0
           (c3 (- to-col (* scale-col head-length 0.5)))
           (r3 (- to-row (* scale-row head-length 0.5))))
      (cond
        ((eq? type 'lines)
         (image-draw-line! image from-col from-row to-col to-row)
         (image-draw-line! image c1 r1 to-col to-row)
         (image-draw-line! image c2 r2 to-col to-row))
        ((eq? type 'hollow)
         (image-draw-line! image from-col from-row c0 r0)
         (image-draw-line! image c1 r1 to-col to-row)
         (image-draw-line! image c2 r2 to-col to-row)
         (image-draw-line! image c1 r1 c2 r2))
        ((eq? type 'hollow-pointy)
         (image-draw-line! image from-col from-row c3 r3)
         (image-draw-line! image c1 r1 to-col to-row)
         (image-draw-line! image c2 r2 to-col to-row)
         (image-draw-line! image c1 r1 c3 r3)
         (image-draw-line! image c2 r2 c3 r3))
        ((or (eq? type 'filled) (eq? type 'pointy))
         (let ((sel (image-selection-save image)))
           (cond
             ((eq? type 'filled)
              (image-draw-line! image from-col from-row to-col to-row)
              (image-select-polygon! image REPLACE ; should be INTERSECT, but needs work
                                     (point c1 r1)
                                     (point c2 r2)
                                     (point to-col to-row)))
             ((eq? type 'pointy)
              (image-draw-line! image from-col from-row c3 r3)
              (image-select-polygon! image REPLACE ; should be INTERSECT, but needs work
                                     (point c1 r1)
                                     (point c3 r3)
                                     (point c2 r2)
                                     (point to-col to-row))))
           (gimp-selection-grow image 1)
           (when (image-has-selection? image) (image-fill! image))
           (image-selection-load! image sel)
           (image-selection-drop! image sel)))
        ((eq? type 'pointy)
         (let ((sel (image-selection-save image)))
           
           (image-fill! image)
           (image-selection-load! image sel)
           (image-selection-drop! image sel)))
        (else
         (error/misc 'image-draw-arrow!
                     (string-append "Invalid arrow type: "
                                    (value->string type)
                                    ", expects one of "
                                    "lines, filled, hollow, "
                                    "pointy, hollow-pointy")
                     (list image type from-col from-row to-col to-row 
                           head-width head-length)))))))

;(define image-draw-arrow!
;  (guard-proc 'image-draw-arrow!
;              _image-draw-arrow!
;              (list 'image 
;                    'arrow-type
;                    'real 'real 'real 'real 
;                    'positive-real 'positive-real)
;              (list image?
;                    ^true
;                    real? real? real? real?
;                    (^and positive? real?) (^and positive? real?))))

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
(define/contract image-draw-line!
  (-> image? real? real? real? real? image?)
  (lambda (image col1 row1 col2 row2)
    (gimp-paintbrush-default (image-get-layer image)
                             4                   
                             (vector col1 row1 col2 row2))
    (cond ((context-immediate-updates?) (context-update-displays!)))
    image))

;(define image-draw-line!
;  (guard-proc 'image-draw-line!
;              _image-draw-line!
;              (list 'image 'real 'real 'real 'real)
;              (list image? real? real? real? real?)))

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
(define/contract image-fill! 
  (-> image? image?)
  (lambda (image)
    (gimp-edit-fill (image-get-layer image) 0)
    (cond ((context-immediate-updates?) (context-update-displays!)))
    image))

;(define image-fill!
;  (guard-unary-proc 'image-fill! _image-fill! 'image image?))

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
(define/contract image-fill-selection! 
  (-> image? image?)
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
(define/contract image-get-layer
  (-> image? real?)
  (lambda (image)
    (let ((id (and image (image-id image))))
      (and id
           (let ((active (car (gimp-image-get-active-layer id)))
                 (layers-info (gimp-image-get-layers id)))
             (if (= active -1)
                 (if (= (car layers-info) 0)
                     #f
                     (let ((layers (cadr layers-info)))
                       (if (vector? layers)
                           (vector-ref layers 0)
                           (car layers))))
                active))))))

;;; Procedure:
;;;   image-get-pixel
;;; Parameters:
;;;   image, an image
;;;   col, an integer
;;;   row, an integer
;;; Purpose:
;;;   Extract the pixel at (col,row) from image.
;;; Produces:
;;;   color, a color
(define/contract image-get-pixel
  (-> image? integer? integer? color?)
  (lambda (image col row)
    (let* ((drawable (car (gimp-image-get-active-layer image)))
           (color (cadr (gimp-drawable-get-pixel drawable col row))))
      (irgb-new (bytes-ref color 0) (bytes-ref color 1) (bytes-ref color 2)))))


;;; Procedure:
;;;   image-has-selection?
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Determine if anything is selected on the image
;;; Produces:
;;;   has-selection?, a Boolean
(define/contract image-has-selection?
  (-> image? boolean?)
  (lambda (image)
    (zero? (car (gimp-selection-is-empty (image-id image))))))

;(define image-has-selection?
;  (guard-unary-proc 'image-has-selection? _image-has-selection?
;                    'image image?))

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
(define/contract image-height
  (-> image? integer?)
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
(define/contract image-id
  (-> image? image-id?)
  (lambda (image)
    (cond
      ((and (integer? image) (image-id? image)) image)
      ((string? image) (image-name->image-id image))
      (else #f))))


;;; Procedure:
;;;   image-load
;;; Parameters:
;;;   fname, a string
;;; Purpose:
;;;   Loads the given image
;;; Produces:
;;;   img, an encapsulated image that corresponds to the image stored
;;;     in the given file.
;;; Preconditions:
;;;   fname names a valid image file.
(define/contract image-load
  (-> string? image?)
  (lambda (fname)
    (cond
      ((not (file-exists? fname))
       (error "image-load: no such file" fname))
      (else
       (let* ((image (car (gimp-file-load 1 fname fname))))
         image)))))

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
(define/contract image-name
  (-> image? string?)
  (lambda (image)
    (cond
      ((image-id? image) (car (gimp-image-get-name image)))
      ((string? image) image)
      (else #f))))


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
(define/contract image-name->image-id
  (-> string? image-id?)
  (lambda (name)
    (and (string? name)
         (let* ((stuff (gimp-image-list))
                (len (car stuff))
                (images (cadr stuff)))
           (let kernel ((pos 0))
             (cond
               ((= pos len) #f)
               ((equal? name (image-name (sequence-ref images pos)))
                (sequence-ref images pos))
               (else (kernel (+ pos 1)))))))))

;;; Procedure:
;;;   image-save
;;; Parameters:
;;;   image, an image
;;;   fname, a file name
;;; Purpose:
;;;   Save image in the given file.
;;; Produces:
;;;   image, the saved image
;;; Preconditions:
;;;   The user can legally write to fname.
;;;   fname ends in one of the standard image file suffixes (.gif,
;;;     .jpg, .png, xcf, ...)
;;; Postconditions:
;;;   The given file now contains a copy of the image.
(define/contract image-save
  (-> image? string? image?)
  (lambda (image fname)
    (gimp-file-save 1 ; non-interactive
                    image
                    (image-get-layer image)
                    fname
                    fname)
    image))

;;; Procedure:
;;;   image-selection-drop!
;;; Parameters:
;;;   image, an image
;;;   selection, a selection
;;; Purpose:
;;;   Remove a saved selection.  (Afterwards, it is invalid to try to
;;;   reload the selection with image-selection-load!.)
;;; Produces:
;;;   [Nothing, called for the side effect]
;;; Preconditions:
;;;   selection must have been created with (image-selection-save image)
;;;   selection must not have been previously dropped

; Note: Contract may not be most effective due to the fact that Gemma
;  couldn't figure out what a selection was.
(define/contract image-selection-drop!
  (-> image? any/c void)
  (lambda (image selection)
    (gimp-image-remove-channel image selection)
    (void)))

;;; Procedure:
;;;   image-selection-load!
;;; Parameters:
;;;   image, an image
;;;   selection, a selection
;;; Purpose:
;;;   Restore a previously saved selection
;;; Produces:
;;;   [Nothing; called for the side effect.]
;;; Preconditions:
;;;   selection must have been created with (image-save-selection image).
;;;   selection must not have been previously deleted with image-drop-selection.

; Note: Contract may not be most effective due to the fact that Gemma
;  couldn't figure out what a selection was.
(define/contract image-selection-load!
  (-> image? any/c void)
  (lambda (image selection)
    (gimp-selection-load selection)
    (void)))

;;; Procedure:
;;;   image-selection-save
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Get a value that represents the current selection of the image
;;; Produces:
;;;   selection, a value that represents the selection
;;; Preconditions:
;;;   image must be a valid image
;;; Postconditions:
;;;   The image is unaffected.
;;;   (image-restore-selection image selection) will restore the selection.
(define/contract image-selection-save
  (-> image? any/c)
  (lambda (image)
    (car (gimp-selection-save image))))

;;; Procedure:
;;;   image-set-pixel!
;;; Parameters:
;;;   image, an image
;;;   col, a column
;;;   row, a row
;;;   color, an RGB color
;;; Purpose:
;;;   Sets the pixel at (col,row) to color.
;;; Produces:
;;;   [Nothing; called for the side effect]
(define/contract image-set-pixel!
  (-> image? integer? integer? irgb? void)
  (lambda (image col row color)
    (when (>= col (image-width image))
       (error "image-set-pixel!: column too large" col))
    (when (>= row (image-height image))
       (error "image-set-pixel!: row too large" row))
    (let ((drawable (car (gimp-image-get-active-layer image)))
          (irgb (color->rgb color)))
        (gimp-drawable-set-pixel drawable col row 3 
                                 (bytes (irgb-red irgb)
                                        (irgb-green irgb)
                                        (irgb-blue irgb))))
    (void)))

;(define image-set-pixel!
;  (guard-proc 'image-set-pixel!
;              _image-set-pixel!
;              (list 'image 
;                    'non-negative-integer 
;                    'non-negative-integer 
;                    'color)
;              (list image?
;                    (^and integer? (^not negative?))
;                    (^and integer? (^not negative?))
;                    color?)))

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
(define/contract image-select-all!
  (-> image? void)
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
;;;   width, an integer
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
(define/contract image-select-ellipse!
  (-> image? (-> any/c any/c) integer? integer? integer? integer? void)
  (lambda (image operation left top width height)
    (image-validate-selection! image operation left top width height
                               'image-select-ellipse!)
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
(define/contract image-select-nothing!
  (-> image? void)
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
(define/contract image-select-rectangle!
  (-> image? (flat-named-contract 'ADD-SUBTRACT-REPLACE-or-INTERSECT
                (lambda (val)
                  (member val (list ADD SUBTRACT REPLACE INTERSECT))))
      integer? integer? integer? integer? void)
  (lambda (image operation left top width height)
    (image-validate-selection! image operation left top width height
                               'image-select-rectangle!)
    (gimp-rect-select image
                      left top
                      width height
                      (selection-op operation)
                      0 0)
    ; (context-update-displays!)
    image))

;;; Procedure:
;;;   image-select-polygon!
;;; Parameters:
;;;   image, an image
;;;   operation, one of the selection operations (ADD, SUBTRACT,
;;;     INTERSECT, REPLACE)
;;;   point, a list of points
;;;     OR
;;;   pt1 ... ptn, n points
;;; Purpose:
;;;   Select the polygon bounded by the given points
;;; Produces:
;;;   image, the image

; Note: The contract for image-select-polygon! does not work at the moment, but
;  that will be fixed in soon.
(define/contract image-select-polygon!
  (->* (image? (flat-named-contract 'ADD-SUBTRACT-REPLACE-or-INTERSECT
                (lambda (val)
                  (member val (list ADD SUBTRACT REPLACE INTERSECT))))
                real?) () #:rest (listof real?) image?)
  (lambda (image operation first . rest)
    (let* ((points (if (null? rest) first (cons first rest)))
           (floats (points->floats points))
           (len (vector-length floats)))
      (gimp-free-select image (vector-length floats) floats
                        operation 1 0 0))))

;(define image-select-polygon!
;  (let ((operations (list ADD SUBTRACT INTERSECT REPLACE)))
;    (lambda (image operation first . rest)
;      (let* ((points (if (null? rest) first (cons first rest)))
;             (params (list image operation points)))
;        (cond
;          ((not (image? image))
;           (error/parameter-type 'image-select-polygon! 1 'image params))
;          ((not (member? operation operations))
;           (error/parameter-type 'image-select-polygon! 2 'selection-op params))
;          ((not (list? points))
;           (error/parameter-type 'image-select-polygon! 3 
;                                 'list-of-points params))
;          ((not (all point? points))
;           (error/parameter-type 'image-select-polygon! 3
;                                 'list-of-points  params))
;          ((or (null? points) 
;               (null? (cdr points)) 
;               (null? (cdr (cdr points))))
;           (error/misc 'image-select-polygon!
;                       (string-append "Requires at least 3 points, given "
;                                      (number->string (length points)))
;                       params))
;          (else
;           (apply _image-select-polygon! params)))))))

;;; Procedure:
;;;   image-show
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Displays the image (which may have been modified behind the scenes).
;;; Produces:
;;;   image, the original image.gimp
;;; Preconditions:
;;;   image must be a valid image (created by create-image or image-load).
(define/contract image-show 
  (-> image? image?)
  (lambda (image)
    (gimp-display-new (image-id image))
    image))

;(define image-show
;  (guard-unary-proc 'image-show
;                    _image-show
;                    'image
;                    image?))

;;; Procedure:
;;;   image-stroke!
;;;   image-stroke-selection!
;;; Parameters:
;;;   image, a gimp image
;;; Purpose:
;;;   Trace the edge of the selected region of the current image
;;;   (in the active layer) using the current brush and foreground
;;;    color.
;;; Produces:
;;;   image, the updated image
;;; Preconditions:
;;;   image is a valid image
;;; Postconditions:
;;;   The image has been stroked, as in the stroke menu item.
(define/contract image-stroke-selection!
  (-> image? image?)
  (lambda (image)
    (cond 
      ((not (image? image))
       (error "image-stroke-selection!: invalid image" image))
      (else
       (gimp-edit-stroke (image-get-layer image))))))

;(define image-stroke-selection!
;  (guard-unary-proc 'image-stroke-selection! _image-stroke-selection!
;                    'image image?))
(define image-stroke! image-stroke-selection!)

;;; Procedure:
;;;   image-transform-pixel!
;;; Parameters:
;;;   image, an image identifier
;;;   col, an integer
;;;   row, an integer
;;;   ctrans, a function from rgb colors to rgb colors
;;; Purpose:
;;;   Transform one pixel in the image
;;; Produces:
;;;   [Nothing; Called for the side effect]
;;; Preconditions:
;;;   image names a valid image.
;;;   0 <= col < (image-width image)
;;;   0 <= row < (image-height image)
;;;   For any rgb color, c, (rgb? (ctrans c))
;;; Postconditions:
;;;   Let c be (image.get-pixel image col row) prior to this call.
;;;   After this call, (image.get-pixel image col row) is now (ctrans c).

; Note: Gemma could not get image-transform-pixel! to work with or without its
;  contract, but Gemma wrote one for it anyway.
(define/contract image-transform-pixel!
  (-> image? integer? integer? (-> irgb? irgb?) void)
  (lambda (image col row ctrans)
    (image-set-pixel! image col row
                      (ctrans (image-get-pixel image col row)))))

;;; Procedure:
;;;   image-new
;;; Parameters:
;;;   width, a positive integer
;;;   height, a positive integer
;;; Purpose:
;;;   Create an image of the given width and height.
;;; Produces:
;;;   img, an encapsulated image
(define/contract image-new
  (-> integer? integer? image?)
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

;(define image-new
;  (guard-proc 'image-new
;              _image-new
;              (list 'positive-integer 'positive-integer)
;              (list (^and integer? positive?)
;                    (^and integer? positive?))))

;;; Procedure:
;;;   image-refresh-display
;;; Parameters:
;;;   img, an image
;;; Purpose:
;;;   Refresh the display.  (Useful when we're setting pixels.)
;;; Produces:
;;;   img, the same image
;;; Preconditions:
;;;   [No additional.]
;;; Postconditions:
;;;   The display has been refreshed (hopefully, showing all the updates)
(define/contract image-refresh-display!
  (-> image? void)
  (lambda (img)
    ; Hack!  Rotating it twice seems to get context-update-displays
    ; to work.
    (gimp-image-rotate img 1)
    (gimp-image-rotate img 1)
    (context-update-displays!)))
  
;(define image-refresh-display!
;  (guard-unary-proc
;   'image-refresh-display!
;   _image-refresh-display!
;   'image
;   image?))

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
(define/contract image-validate-selection!
  (-> image? (flat-named-contract 'ADD-SUBTRACT-REPLACE-or-INTERSECT
                (lambda (val)
                  (member val (list ADD SUBTRACT REPLACE INTERSECT))))
      integer? integer? integer? integer?  string? void)
  (lambda (image operation left top width height proc)
    (let ((crash (lambda (message) 
                   (error (string-append 
                           (symbol->string proc) ": " message "\n  in "
                           (value->string (list proc image operation
                                                left top width height)))))))
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
(define/contract image-width
  (-> image? integer?)
  (lambda (image)
    (cond 
      ((not (image? image))
       (error "image-width: invalid image" image))
      (else
       (car (gimp-image-width image))))))
