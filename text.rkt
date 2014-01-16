#lang racket

; gigls/text.rkt
;   Procedures to work with text and fonts.

(require gigls/context
         gigls/guard
         gigls/higher
         gigls/image
         gigls/makers
         gigls/pdb-dbus
         gigls/utils)
(provide (all-defined-out))

; +-------+-----------------------------------------------------------
; | Notes |
; +-------+

; (gimp-text-get-extents-fontname text/string 
;                                 size/float 
;                                 size-type/0  ; for pixels
;                                 fontname/string)
;    => width, height, ascent, descent
;    Get the bounding box.

; +---------+---------------------------------------------------------
; | Context |
; +---------+

;;; Procedure:
;;;   font-name?
;;; Parameters:
;;;   name, a Scheme value
;;; Purpose:
;;;   Determine if name is a font name
;;; Produces:
;;;   is-font-name, a Boolean
(define font-name?
  (lambda (name)
    (and (string? name) 
         (sequence-contains? (cadr (gimp-fonts-get-list name)) name))))

;;; Procedure:
;;;   context-list-font-names
;;; Parameters:
;;;   pattern, a string [optional]
;;; Purpose:
;;;   List all font names that match the given pattenr.
;;; Produces:
;;;   font-names, a list of strings
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   Any member of font-names can be used by context-set-font-name.
(define _context-list-font-names
  (lambda params
    (let ([pattern (if (null? params) "" (car params))])
      (sequence->list (cadr (gimp-fonts-get-list pattern))))))

(define context-list-font-names
  (guard-01-proc 'context-list-font-names
                 _context-list-font-names
                 'string
                 string?))

;;; Procedure:
;;;   context-get-font-name
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Get the current font name.
;;; Produces:
;;;   font-name, a string
(define _context-get-font-name
  (lambda ()
    (car (gimp-context-get-font))))
(define context-get-font-name _context-get-font-name)

;;; Procedure:
;;;   context-set-font-name!
;;; Parameters:
;;;   font-name, a string that is in (context-list-font-names)
;;; Purpose:
;;;   Set the current font name to font-name.
;;; Produces:
;;;   [Nothing; called for side effects.]
;;; Preconditions:
;;;   (member? font-name (context-list-font-names))
;;; Postconditions:
;;;   Subsequent calls to (context-get-font-name) return font-name
(define _context-set-font-name!
  (lambda (font-name)
    (gimp-context-set-font font-name)))
(define context-set-font-name!
  (guard-unary-proc 'context-set-font-name! 
                    _context-set-font-name!
                    'font-name
                    font-name?))

;;; Procedure:
;;;   font-size
;;; Parameters:
;;;   newsize, a positive integer [optional]
;;; Purpose:
;;;   Get or set the font size
;;; Produces:
;;;   size, the font size
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If called with no parameters, returns the current font size.
;;;   If called with a parameter, sets the current font size and
;;;     then returns it.
(define _font-size (make-state 12))
(define font-size
  (guard-01-proc 'font-size 
                 _font-size 
                 'positive-integer 
                 (and integer? positive?)))

;;; Name:
;;;   context-set-font-size!
;;; Parameters:
;;;   size, a positive integer
;;; Purpose:
;;;   Set the current font size.
;;; Produces:
;;;   [Nothing; called for the side effect.]
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   Future calls to context-get-font-size return size.
(define _context-set-font-size! 
  (lambda (size)
    (_font-size size)
    (void)))

(define context-set-font-size!
  (guard-unary-proc 'context-set-font-size!
                    _context-set-font-size!
                    'positive-integer
                    (^and integer? positive?)))
 
;;; Procedure:
;;;   context-get-font-size
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Get the current font size
;;; Produces:
;;;   size, an integer
(define _context-get-font-size _font-size)
(define context-get-font-size
  (lambda () (_context-get-font-size)))

;;; Names:
;;;   ALIGN-LEFT
;;;   ALIGN-RIGHT
;;;   ALIGN-CENTER
;;;   ALIGN-TOP
;;;   ALIGN-BOTTOM
;;; Type:
;;;   Symbol
;;; Value:
;;;   Values to indicate whether text should be left-aligned, 
;;;   right-aligned, or centered.
;;; Context:
;;;   Used by image-display-text!
(define ALIGN-CENTER 'ALIGN-CENTER)
(define ALIGN-LEFT 'ALIGN-LEFT)
(define ALIGN-RIGHT 'ALIGN-RIGHT)
(define ALIGN-TOP 'ALIGN-TOP)
(define ALIGN-BOTTOM 'ALIGN-BOTTOM)

;;; Procedure:
;;;   image-display-text-basic!
;;; Parameters:
;;;   image, an image
;;;   text, a string
;;;   x, the x coordinate
;;;   y, the y coordinate
;;; Purpose:
;;;   Display text at (x,y) [however GIMP interprets that]
;;; Produces:
;;;   image, the updated image
;;; Problems:
;;;   Requires a subsequent call to context-update-displays!
(define _image-display-text-basic!
  (lambda (image text x y)
    (gimp-text-fontname image -1 
                        x y
                        text
                        0       ; border
                        1       ; antialias
                        (context-get-font-size)
                        0       ; pixels (1 for points)
                        (context-get-font-name))
    (gimp-image-flatten image)
    image))

(define image-display-text-basic!
  (guard-proc 'image-display-text-basic!
              _image-display-text-basic!
              (list 'image 'string 'real 'real)
              (list image? string? real? real?)))

;;; Procedure:
;;;   image-display-text!
;;; Parameters:
;;;   image, an image id
;;;   text, the text to display
;;;   x, the x position to display the text
;;;   y, the y position to display the text
;;;   halign, one of ALIGN-CENTER, ALIGN-LEFT, or ALIGN-RIGHT
;;;   valign, one of ALIGN-CENTER, ALIGN-TOP, or ALIGN-BOTTOM
;;; Purpose:
;;;   Display the text using the current font name and size.
;;; Produces:
;;;   image, the same image
(define _image-display-text!
  (lambda (image text x y halign valign)
    (let* ([bbox (text-bbox text)]
           [left (cond 
                   [(equal? halign ALIGN-CENTER)
                    (- x (/ (car bbox) 2))]
                   [(equal? halign ALIGN-RIGHT)
                    (- x (car bbox))]
                   [else
                    x])]
           [top (cond 
                  [(equal? valign ALIGN-CENTER)
                   (- y (/ (cadr bbox) 2))]
                  [(equal? valign ALIGN-BOTTOM)
                   (- y (caddr bbox))]
                  [else
                   (+ y (cadddr bbox))])])
      (_image-display-text-basic! image text left top))))

(define image-display-text!
  (guard-proc 'image-display-text!
              _image-display-text!
              (list 'image 'string 
                    'real 'real 
                    'horizontal-alignment
                    'vertical-alignment)
              (list image? string?
                    real? real?
                    (r-s member? (list ALIGN-LEFT ALIGN-RIGHT ALIGN-CENTER))
                    (r-s member? (list ALIGN-TOP ALIGN-BOTTOM ALIGN-CENTER)))))

;;; Procedure:
;;;   text-bbox
;;; Parameters:
;;;   text, a string
;;; Purpose:
;;    Get the bounding box info for the text
;;; Produces:
;;;   bbox, a list of the form (width height ascender descender)
(define _text-bbox
  (lambda (text)
    (gimp-text-get-extents-fontname text
                                    (context-get-font-size)
                                    0
                                    (context-get-font-name))))

(define text-bbox
  (guard-unary-proc 'text-bbox _text-bbox 'string string?))

;;; Procedure:
;;;   text-height
;;; Parameters:
;;;   text, a string
;;; Purpose:
;;;   Determines the height of the given text in the current font settings.
;;; Produces:
;;;   height, a real
(define _text-height
  (lambda (text)
    (cadr (gimp-text-get-extents-fontname text
                                          (context-get-font-size)
                                          0
                                          (context-get-font-name)))))
(define text-height 
  (guard-unary-proc 'text-height _text-height 'string string?))

;;; Procedure:
;;;   text-width
;;; Parameters:
;;;   text, a string
;;; Purpose:
;;;   Determines the width of the given text in the current font settings.
;;; Produces:
;;;   width, a real
(define _text-width
  (lambda (text)
    (car (gimp-text-get-extents-fontname text
                                          (context-get-font-size)
                                          0
                                          (context-get-font-name)))))
(define text-width 
  (guard-unary-proc 'text-width _text-width 'string string?))


