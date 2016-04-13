#lang racket

(require gigls/pdb-dbus) ;For gimp-pencil
(provide (all-defined-out))
(require gigls/colors
         gigls/context
         gigls/current-brush
         gigls/guard
         gigls/hacks
         gigls/image
         gigls/higher
         gigls/list
         gigls/point
         gigls/rgb-core
         gigls/utils)

;;; Procedure:
;;;   guard-drawing-proc
;;; Parameters:
;;;   name, a symbol
;;;   proc, a procedure of the form (lambda (drawing) ___)
;;; Purpose:
;;;   Build a new version of proc that checks preconditions.
;;; Produces:
;;;   guarded, a procedure of the form (lambda (drawing) _____)
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If (drawing? val), (guarded val) = (proc val)
;;;   Otherwise, (guarded val) throws an appropriate error
(define guard-drawing-proc
  (lambda (name proc)
    (guard-unary-proc name proc 'drawing drawing?)))

;;; Procedure:
;;;   drawing-shape?
;;; Parameters:
;;;   val, a value
;;; Purpose:
;;;   Determine if val is one of the drawing shapes
;;; Produces:
;;;   is-shape?, a Boolean
(define/contract drawing-shape?
  (-> any/c boolean?)
  (let* ((nnr? (^and real? (^not negative?))))
    (lambda (val)
      (check-list? (list (l-s eq? 'drawing) 
                         (r-s member '(ellipse rectangle))
                         color?         ; color
                         string?        ; brush
                         real?          ; left
                         real?          ; right
                         nnr?           ; width
                         nnr?           ; height
                         )
                   val))))

;(define drawing-shape? _drawing-shape?)

;;; Procedure:
;;;   drawing?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine whether val can be appropriately interpreted as a
;;;   drawing
;;; Produces:
;;;   is-drawing?, a Boolean
(define/contract drawing?
  (-> any/c boolean?)
  (lambda (val)
    (or (drawing-blank? val)
        (drawing-group? val)
        (drawing-line? val)
        (drawing-rule? val)
        (drawing-shape? val))))

;;; Procedure:
;;;   drawing-blank?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val is a  blank drawing.
;;; Produces:
;;;   is-blank?, a Boolean
(define/contract drawing-blank?
  (-> any/c boolean?)
  (lambda (val)
    (check-list? (list (l-s eq? 'drawing) 
                       (l-s eq? 'blank))
                 val)))

;(define drawing-blank? _drawing-blank?)

;;; Procedure:
;;;   drawing-group?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val is a drawing-group
;;; Produces:
;;;   is-group?, a Boolean
(define/contract drawing-group?
  (-> any/c boolean?)
  (lambda (val)
    (check-list? (list (l-s eq? 'drawing) 
                       (l-s eq? 'group)
                       (l-s all drawing?))
                 val)))

;(define drawing-group? _drawing-group?)

;;; Procedure:
;;;   drawing-line?
;;; Parameters:
;;;   val, a value
;;; Purpose:
;;;   Determine if val can be interpreted as a line in a drawing
;;; Produces:
;;;   is-line?, a boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If val can be used with the various drawing-line-PROC procedures,
;;;     is-line? is true.
;;;   Otherwise is-line? is false.
(define/contract drawing-line?
  (-> any/c boolean?)
  (let* ((nnr? (^and real? (^not negative?))))
    (lambda (val)
      (check-list? (list (l-s eq? 'drawing) (l-s eq? 'line)
                         color?
                         real? real? real? real?
                         nnr? nnr?)
                   val))))

;(define drawing-line? _drawing-line?)

;;; Procedure:
;;;   drawing-rule?
;;; Parameters:
;;;   val, a value
;;; Purpose:
;;;   Determine if val can be interpreted as a rule in a drawing
;;; Produces:
;;;   is-rule?, a boolean
(define/contract drawing-rule?
  (-> any/c boolean?)
  (let* ((nnr? (^and real? (^not negative?)))
         (types (list (l-s eq? 'drawing) (l-s eq? 'rule)
                      real? real? real? real?)))
    (lambda (val)
      (check-list? (list (l-s eq? 'drawing) (l-s eq? 'rule)
                         color?
                         real? real? real? real?)
                   val))))

;(define drawing-rule? _drawing-rule?)

;;; Value: 
;;;   drawing-blank
;;; Type:
;;;   drawing
;;; Description:
;;;   A blank drawing.  Included for the sake of completeness and,
;;;   more importantly, to provide a base case for recursion.
(define drawing-blank (list 'drawing 'blank))

;;; Procedure:
;;;   drawing-bottom
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Find the bottom edge of drawing
;;; Produces:
;;;   bottom, a real
(define/contract drawing-bottom
  (-> drawing? real?)
  (lambda (drawing)
    (let ((type (drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         0)
        ((eq? type 'group)
         (apply max (map drawing-bottom (drawing-members drawing))))
        ((eq? type 'line)
         (drawing-line-bottom drawing))
        ((eq? type 'rule)
         (drawing-rule-bottom drawing))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape-bottom drawing))
        (else
         (error "drawing-bottom: Unknown drawing type: " type))))))

;(define drawing-bottom
;  (guard-drawing-proc 'drawing-bottom _drawing-bottom))

;;; Procedure:
;;;   drawing-brush 
;;; Parameter:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the brush associated with the drawing
;;; Produces:
;;;   brush, a string
(define/contract drawing-brush
  (-> drawing? string?)
  (lambda (drawing)
    (let ((type (drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         "")
        ((eq? type 'group)
         "")
        ((eq? type 'line)
         "")
        ((eq? type 'rule)
         "")
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape-brush drawing))
        (else
         (error "drawing-brush: unknown drawing type" type))))))

;(define drawing-brush
;  (guard-drawing-proc 'drawing-brush _drawing-brush))


;;; Procedure:
;;;   drawing-color
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the color of drawing.
;;; Produces:
;;;   type, a color
(define/contract drawing-color 
  (-> drawing? color?)
  (lambda (drawing)
    (let ((type (drawing-type drawing)))
      (cond 
        ;((eq? type 'blank)
        ; rgb-transparent)
        ((eq? type 'group)
         (rgb-new 0 0 0))
        ((eq? type 'line)
         (drawing-line-color drawing))
        ((eq? type 'rule)
         (drawing-rule-color drawing))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape-color drawing))
        (else
         (error "drawing-color: unknown drawing type" type))))))

;;; Procedure:
;;;   drawing-compose
;;; Parameters:
;;;   drawings, a list of drawings
;;; Purpose:
;;;   Create a new drawing by composing the drawings in drawings
;;; Produces:
;;;   composed, a drawing
;;; Preconditions:
;;;   (length drawings) >= 1
(define/contract drawing-compose
  (-> (listof drawing?) drawing?)
  (lambda (drawings)
    (apply drawing-group drawings)))

;(define drawing-compose
;  (lambda (drawings)
;    (cond
;      ((null? drawings)
;       (error "drawing-compose: expects at least one parameter"))
;      ((not (all drawing? drawings))
;       (error "drawing-compose: called with at least one non-drawing"))
;      (else (_drawing-compose drawings)))))

;(define drawing-color
;  (guard-drawing-proc 'drawing-color _drawing-color))

;;; Procedure:
;;;   drawing-ellipse
;;; Purpose:
;;;   Create a new drawing that represents an ellipse
;;; Parameters:
;;;   left, a real
;;;   top, a real
;;;   width, a postitive real
;;;   height, a positive real
;;; Produces:
;;;   ellipse, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   When rendered, ellipse will be drawn as a filled ellipse, 
;;;   with the specified left margin, top margin, width, and
;;;   height.
(define/contract drawing-ellipse
  (-> real? real? (and/c real? positive?) (and/c real? positive?) drawing?)
  (lambda (left right width height)
    (drawing-shape 'ellipse (rgb-new 0 0 0) "" left right width height)))

;(define drawing-ellipse
;  (guard-proc 'drawing-ellipse
;              _drawing-ellipse
;              (list 'real 'real 'positive-real 'positive-real)
;              (list real? real? (^and real? positive?) (^and real? positive?))))


;;; Procedure:
;;;   drawing-filled?
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Determine if drawing is filled.
;;; Produces:
;;;   filled?, a Boolean
(define/contract drawing-filled?
  (-> drawing? boolean?)
  (lambda (drawing)
    (let ((type (drawing-type drawing)))
      (and (or (eq? type 'ellipse) (eq? type 'rectangle))
           (string=? "" (drawing-brush drawing))))))

;(define drawing-filled? 
;  (guard-drawing-proc 'drawing-filled? _drawing-filled?))

;;; Procedure:
;;;   drawing-format
;;; Parameters:
;;;   type, a valid drawing type
;;; Purpose:
;;;   Shows the format of drawing values of the same type as drawing
;;; Produces:
;;;   format, a list
(define/contract drawing-format
  (-> symbol? list?)
  (lambda (type)
    (cond
      ((eq? type 'blank)
       '(drawing blank))
      ((eq? type 'group)
       '(drawing group (drawing1 drawing2)))
      ((eq? type 'ellipse)
       '(drawing ellipse color brush left top width height))
      ((eq? type 'line)
       '(drawing line color col1 row col2 row2 hscale vscale))
      ((eq? type 'rectangle)
       '(drawing rectangle color brush left top width height))
      ((eq? type 'rule)
       '(drawing rule color col1 row1 col2 row2))
      (else
       (error "drawing-format: unknown type of drawing: " type)))))

;(define drawing-format _drawing-format)

;;; Procedure:
;;;   drawing-group
;;; Parameters:
;;;   drawing1 ... drawingn, n drawings 
;;; Purpose:
;;;   Create a new drawing by composing drawing1 ... drawingn.
;;; Produces:
;;;   grouped, a drawing
;;; Preconditions:
;;;   There is at least one parameter.
(define/contract drawing-group
  (->* (drawing?) () #:rest (listof drawing?) drawing?)
  (lambda drawings
    (list 'drawing 'group drawings)))

;(define drawing-group
;  (lambda drawings
;    (cond
;      ((null? drawings)
;       (error "drawing-group: expects at least one parameter"))
;      ((not (all drawing? drawings))
;       (error "drawing-group: called with at least one non-drawing"))
;      (else (apply _drawing-group drawings)))))


;;; Procedure:
;;;   drawing-group-render!
;;; Parameters:
;;;   image, an image
;;;   drawing, a gropued drawing
;;; Purpose:
;;;   Render drawing on image.
;;; Produces:
;;;   image, the input image
;;; Preconditions:
;;;   image is a valid image.
;;;   drawing is a valid drawing.
;;; Postconditions:
;;;   image has been extended by the appropriate drawing.
(define/contract drawing-group-render!
  (-> image? drawing? image?)
  (lambda (drawing image)
    (foreach! (lambda (d)
                (and (or (eq? (drawing-type d) 'group)
                         (drawing-on-image? d image))
                     (drawing-render! d image)))
              (drawing-members drawing))))

;(define drawing-group-render!
;  (guard-proc 'drawing-group-render!
;              _drawing-group-render!
;              (list 'drawing-group 'image)
;              (list drawing-group? image?)))


;;; Procedure:
;;;   drawing-height
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the height of drawing.
;;; Produces:
;;;   height, a real
(define/contract drawing-height
  (-> drawing? real?)
  (lambda (drawing)
    (let ((type (drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         0)
        ((eq? type 'group)
         (- (drawing-bottom drawing) (drawing-top drawing)))
        ((eq? type 'line)
         (drawing-line-height drawing))
        ((eq? type 'rule)
         (drawing-rule-height drawing))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape-height drawing))
        (else
         (error "drawing-height: Unknown drawing type: " type))))))

;(define drawing-height
;  (guard-drawing-proc 'drawing-height _drawing-height))

;;; Procedure:
;;;   drawing-hscale
;;; Parameters:
;;;   drawing, a drawing
;;;   factor, a real number
;;; Purpose:
;;;   Create a new version of drawing that is horizontally scaled by the
;;;   specified factor.
;;; Produces:
;;;   scaled, a drawing
;;; Postconditions:
;;;   scaled is the same color as drawing, but the width is
;;;   scaled by factor (as is the left side).
(define/contract drawing-hscale
  (-> drawing? real? drawing?)
  (lambda (drawing factor)
    (let ((type (drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         drawing)
        ((eq? type 'group)
         (apply drawing-group
                (map (r-s drawing-hscale factor)
                     (drawing-members drawing))))
        ((eq? type 'line)
         (drawing-line-hscale drawing factor))
        ((eq? type 'rule)
         (drawing-rule-core (drawing-rule-color drawing)
                            (* factor (drawing-rule-left drawing))
                            (drawing-rule-top drawing)
                            (* factor (drawing-rule-right drawing))
                            (drawing-rule-bottom drawing)))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape type
                        (drawing-color drawing) 
                        (drawing-brush drawing)
                        (* factor (drawing-left drawing))
                        (drawing-top drawing)
                        (* factor (drawing-width drawing)) 
                        (drawing-height drawing)))
        (else
         (error "drawing-hscale: unknown drawing type" type))))))

;(define drawing-hscale
;  (guard-proc 'drawing-hscale
;              _drawing-hscale
;              (list 'drawing 'real)
;              (list drawing? real?)))

;;; Procedure:
;;;   drawing-hshift
;;; Parameters:
;;;   drawing, a drawing
;;;   amt, a real number
;;; Purpose:
;;;   Create a new version of drawing that is horizontally shifted 
;;;   by the specified amt.
;;; Produces:
;;;   shifted , a drawing
;;; Postconditions:
;;;   scaled is the same overall "shape", color, and size as
;;;   drawing, but shifted to the right by amt (or to the left
;;;   by |amt|, if amt is negative).
(define/contract drawing-hshift
  (-> drawing? real? drawing?)
  (lambda (drawing amt)
    (let ((type (drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         drawing)
        ((eq? type 'group)
         (apply drawing-group
                (map (r-s drawing-hshift amt)
                     (drawing-members drawing))))
        ((eq? type 'line)
         (drawing-line-hshift drawing amt))
        ((eq? type 'rule)
         (drawing-rule-core (drawing-rule-color drawing)
                            (+ amt (drawing-rule-left drawing))
                            (drawing-rule-top drawing)
                            (+ amt (drawing-rule-right drawing))
                            (drawing-rule-bottom drawing)))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape type
                        (drawing-shape-color drawing) 
                        (drawing-shape-brush drawing)
                        (+ amt (drawing-shape-left drawing)) 
                        (drawing-shape-top drawing)
                        (drawing-shape-width drawing) 
                        (drawing-shape-height drawing)))
        (else
         (error "drawing-hshift: unknown drawing type" type))))))

;(define drawing-hshift
;  (guard-proc 'drawing-hshift
;              _drawing-hshift
;              (list 'drawing 'real)
;              (list drawing? real?)))

;;; Procedure:
;;;   drawing->image
;;; Parameters:
;;;   drawing, a drawing
;;;   width, an integer
;;;   height, an integer
;;; Purpose:
;;;   Creates an image that shows the given drawing.
;;; Produces:
;;;   img, an image id
;;; Preconditions:
;;;   width > 0
;;;   height > 0
;;; Postconditions:
;;;   img, when shown, contains the given drawing.
(define/contract drawing->image
  (-> drawing? integer? integer? image?)
  (lambda (drawing width height)
    (let ([bgcolor (context-get-bgcolor)])
      (context-set-bgcolor! "white")
      (let ((img (image-new width height)))
        (drawing-render! drawing img)
        (context-set-bgcolor! bgcolor)
        img))))

;;; Procedure:
;;;   drawing-left
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Find the left edge of drawing
;;; Produces:
;;;   left, a real
(define/contract drawing-left
  (-> drawing? real?)
  (lambda (drawing)
    (let ((type (drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         0)
        ((eq? type 'group)
         (apply min (map drawing-left (drawing-members drawing))))
        ((eq? type 'line)
         (drawing-line-left drawing))
        ((eq? type 'rule)
         (drawing-rule-left drawing))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape-left drawing))
        (else
         (error "drawing-left: Unknown drawing type: " type))))))

;(define drawing-left
;  (guard-drawing-proc 'drawing-left _drawing-left))


;;; Procedure:
;;;   drawing-line-height
;;; Parameters:
;;;   line, a drawing line
;;; Purpose:
;;;   Get the height of the drawing
;;; Produces:
;;;   height, a real
(define/contract drawing-line-height
  (-> drawing-line? real?)
  (lambda (line)
    (abs (- (list-ref line 4) (list-ref line 6)))))

;(define drawing-line-height 
;  (guard-unary-proc 'drawing-line-height 
;                    _drawing-line-height
;                    'drawing-line 
;                    drawing-line?))


;;; Procedure:
;;;   drawing-line-hscale
;;; Parameters:
;;;   line, a drawing line
;;;   factor, a real number
;;; Purpose:
;;;   Create a new version of line, scaled horizontally by factor
;;; Produces:
;;;   scaled, a drawing
(define/contract drawing-line-hscale
  (-> drawing-line? real? drawing?)
  (lambda (line factor)
    (drawing-line-core (drawing-line-color line)
                       (* factor (drawing-line-left line))
                       (drawing-line-top line)
                       (* factor (drawing-line-right line))
                       (drawing-line-bottom line)
                       (* factor (drawing-line-hstretch line))
                       (drawing-line-vstretch line))))

;(define drawing-line-hscale
;  (guard-proc 'drawing-line-hscale
;              _drawing-line-hscale
;              (list 'drawing-line 'real)
;              (list drawing-line? real?)))

;;; Procedure:
;;;   drawing-line-left
;;; Parameters:
;;;   line, a drawing line
;;; Purpose:
;;;   Get the left edge of the drawing
;;; Produces:
;;;   left, a real
;;; Preconditions:
;;;   (drawing-line? line)
(define/contract drawing-line-left 
  (-> drawing-line? real?)
  (lambda (line)
    (min (list-ref line 3) (list-ref line 5))))

;(define drawing-line-left 
;  (guard-unary-proc 'drawing-line-left 
;                    _drawing-line-left
;                    'drawing-line 
;                    drawing-line?))



;;; Procedure:
;;;   drawing-line-bottom
;;; Parameters:
;;;   line, a drawing line
;;; Purpose:
;;;   Get the bottom edge of the drawing
;;; Produces:
;;;   bottom, a real
(define/contract drawing-line-bottom 
  (-> drawing-line? real?)
  (lambda (line)
    (max (list-ref line 4) (list-ref line 6))))

;(define drawing-line-bottom 
;  (guard-unary-proc 'drawing-line-bottom 
;                    _drawing-line-bottom
;                    'drawing-line 
;                    drawing-line?))

;;; Procedure:
;;;   drawing-line-core
;;; Parameters:
;;;   color, a color
;;;   c1, a real
;;;   r1, a real
;;;   c2, a real
;;;   r2, a real
;;;   hstretch, a real
;;;   vstretch, a real
;;; Purpose:
;;;   Create a 'line' from (c1,r1) to (c2,r2)
;;; Produces:
;;;   line, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   When rendered, line will be a line from (c1,r1) to (c2,r2),
;;;     in color color, and "stretched" horizontally by hstretch
;;;     and vertically by vstretch
(define/contract drawing-line-core
  (-> color? real? real? real? real? real? real? drawing?)
  (lambda (color c1 r1 c2 r2 hstretch vstretch)
    (list 'drawing 'line                     ; 0, 1
          color                             ; 2
          c1 r1 c2 r2                       ; 3, 4, 5, 6
          (abs hstretch) (abs vstretch))))  ; 7, 8

;(define drawing-line-core
;  (lambda params
;    (validate-params! 'drawing-line-core
;                      (list 'color 'real 'real 'real 'real 'real 'real)
;                      (list color? real? real? real? real? real? real?)
;                      params)
;    (apply _drawing-line-core params)))

;;; Procedure:
;;;   drawing-line-hstretch
;;; Parameters:
;;;   line, a drawing line
;;; Purpose:
;;;   Get the horizontal 'stretch' of the line
;;; Produces:
;;;   stretch, a real
(define/contract drawing-line-hstretch 
  (-> drawing-line? real?)
  (r-s list-ref 7))

;(define drawing-line-hstretch 
;  (guard-unary-proc 'drawing-line-hstretch 
;                    _drawing-line-hstretch
;                    'drawing-line 
;                    drawing-line?))
;;; Procedure:
;;;   drawing-line-vstretch
;;; Parameters:
;;;   line, a drawing line
;;; Purpose:
;;;   Get the horizontal 'stretch' of the line
;;; Produces:
;;;   stretch, a real
(define/contract drawing-line-vstretch 
  (-> drawing-line? real?)
  (r-s list-ref 8))

;(define drawing-line-vstretch 
;  (guard-unary-proc 'drawing-line-vstretch 
;                    _drawing-line-vstretch
;                    'drawing-line 
;                    drawing-line?))

;;; Procedure:
;;;   drawing-line-color
;;; Parameters:
;;;   line, a drawing line
;;; Purpose:
;;;   Get the color of the line
;;; Produces:
;;;   color, a color
(define/contract drawing-line-color 
  (-> drawing-line? color?)
  (r-s list-ref (list-index (drawing-format 'line) 'color)))

;(define drawing-line-color 
;  (guard-unary-proc 'drawing-line-color 
;                    _drawing-line-color
;                    'drawing-line 
;                    drawing-line?))

;;; Procedure:
;;;   drawing-line-hshift
;;; Parameters:
;;;   line, a drawing line
;;;   amt, a real number
;;; Purpose:
;;;   Create a new version of line, shifted horizontally by amt
;;; Produces:
;;;   shifted, a drawing
(define/contract drawing-line-hshift
  (-> drawing-line? real? drawing?)
  (lambda (line amt)
    (drawing-line-core (drawing-line-color line)
                       (+ amt (drawing-line-left line))
                       (drawing-line-top line)
                       (+ amt (drawing-line-right line))
                       (drawing-line-bottom line)
                       (drawing-line-hstretch line)
                       (drawing-line-vstretch line))))

;(define drawing-line-hshift
;  (guard-proc 'drawing-line-hshift
;              _drawing-line-hshift
;              (list 'drawing-line 'real)
;              (list drawing-line? real?)))

;;; Procedure:
;;;   drawing-line-recolor
;;; Parameters:
;;;   line, a line
;;;   color, a color
;;; Purpose:
;;;   Recolor line in color
;;; Produces:
;;;   newline, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   When rendered, newline is in the same place as line, but
;;;   is in the new color.
(define/contract drawing-line-recolor
  (-> drawing-line? color? drawing?)
  (lambda (line color)
    (drawing-line-core color
                       (drawing-line-left line)
                       (drawing-line-top line)
                       (drawing-line-right line)
                       (drawing-line-bottom line)
                       (drawing-line-hstretch line)
                       (drawing-line-vstretch line))))

;(define drawing-line-recolor
;  (guard-proc 'drawing-line-recolor 
;              _drawing-line-recolor
;              (list 'drawing-line 'color)
;              (list drawing-line? color?)))

;;; Procedure:
;;;   drawing-recolor
;;; Parameters:
;;;   drawing, a drawing
;;;   color, a color
;;; Purpose:
;;;   Create a new version of drawing that is colored by the
;;;   given color
;;; Produces:
;;;   recolored, a drawing
;;; Preconditions:
;;;   color is a valid color.
;;; Postconditions:
;;;   recolored is the same overall "shape" but is colored the given color.
(define/contract drawing-recolor
  (-> drawing? color? drawing?)
  (lambda (drawing color)
    (let ((type (drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         drawing)
        ((eq? type 'group)
         (apply drawing-group
                (map (r-s drawing-recolor color)
                     (drawing-members drawing))))
        ((eq? type 'line)
         (drawing-line-recolor drawing color))
        ((eq? type 'rule)
         (drawing-rule-core color
                            (drawing-rule-left drawing)
                            (drawing-rule-top drawing)
                            (drawing-rule-right drawing)
                            (drawing-rule-bottom drawing)))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape type
                        (color->rgb color) 
                        (drawing-brush drawing)
                        (drawing-left drawing) 
                        (drawing-top drawing)
                        (drawing-width drawing) 
                        (drawing-height drawing)))
        (else
         (error "drawing-recolor: unknown drawing type" type))))))

; Because color? may get redefined, we don't use guard-drawing-proc
;(define drawing-recolor
;  (lambda params
;    (validate-params! 'drawing-recolor
;                      (list 'drawing 'color)
;                      (list drawing? color?)
;                      params)
;    (apply _drawing-recolor params)))

;;; Procedure:
;;;   drawing-line-render!
;;; Parameters:
;;;   image, an image
;;;   line, a drawing line
;;; Purpose:
;;;   Render line on image.
;;; Produces:
;;;   image, the input image
;;; Preconditions:
;;;   (image? image)
;;;   (drawing? drawing)
;;;   (drawing-line? drawing)
;;; Postconditions:
;;;   image has been extended by the appropriate drawing.
(define/contract drawing-line-render!
  (-> image? drawing-line? image?)
  (lambda (line image)
    (let ((c1 (drawing-line-left line))
          (r1 (drawing-line-top line))
          (c2 (drawing-line-right line))
          (r2 (drawing-line-bottom line))
          (newcolor (drawing-line-color line))
          (h (drawing-line-hstretch line))
          (v (drawing-line-vstretch line))
          (savecolor (context-get-fgcolor)))
      
      ; Change the fgcolor if necessary
      (cond ((not (equal? newcolor savecolor))
             (context-set-fgcolor! newcolor)))
      
      ; Decide how we're going to draw the line
      (cond
        ; If h and v are zero, we just draw a rule
        ((and (zero? h) (zero? v))
         (let ((savebrush (context-get-brush))
               (newbrush "Circle (01)"))
           ; Change the brush if necessary
           (cond ((not (string=? newbrush savebrush))
                  (context-set-brush! newbrush)))
           ; Draw the line
           (image-draw-line! image c1 r1 c2 r2)
           ; Restore the brush if necessary
           (cond ((not (string=? newbrush savebrush))
                  (context-set-brush! savebrush)))))
        ; Otherwise, we do some fancy footwork to draw the line
        (else
         (let* ((hh (/ h 2))
                (vv (/ v 2))
                (points (list (point (- c1 hh) (+ r1 vv))
                              (point (+ c1 hh) (- r1 vv))
                              (point (+ c2 hh) (- r2 vv))
                              (point (- c2 hh) (+ r2 vv)))))
           (image-select-polygon! image REPLACE points)
           (image-fill-selection! image)
           (image-select-nothing! image))))
      
      ; Restore the fgcolor if necessary
      (cond ((not (equal? newcolor savecolor))
             (context-set-fgcolor! newcolor)))
      
      ; And return the modified image
      image)))

;(define drawing-line-render!
;  (guard-proc 'drawing-line-render!
;              _drawing-line-render!
;              (list 'drawing-line 'image)
;              (list drawing-line? image?)))

;;; Procedure:
;;;   drawing-line-right
;;; Parameters:
;;;   line, a drawing
;;; Purpose:
;;;   Get the right edge of the drawing
;;; Produces:
;;;   right, a real
;;; Preconditions:
;;;   (drawing-line? line)
;;; Postcondition:
;;;   The right edge of drawing
(define/contract drawing-line-right
  (-> drawing-line? real?)
  (lambda (drawing)
    (max (list-ref drawing 3) (list-ref drawing 5))))

;(define drawing-line-right 
;  (guard-unary-proc 'drawing-line-right _drawing-line-right
;                    'drawing/line drawing-line?))

;;; Procedure:
;;;   drawing-line-scale
;;; Parameters:
;;;   line, a drawing line
;;;   factor, a real number
;;; Purpose:
;;;   Create a new version of line, scaled by factor
;;; Produces:
;;;   scaled, a drawing
(define/contract drawing-line-scale
  (-> drawing-line? real? drawing?)
  (lambda (line factor)
    (drawing-line-core (drawing-line-color line)
                       (* factor (drawing-line-left line))
                       (* factor (drawing-line-top line))
                       (* factor (drawing-line-right line))
                       (* factor (drawing-line-bottom line))
                       (* factor (drawing-line-hstretch line))
                       (* factor (drawing-line-vstretch line)))))

;(define drawing-line-scale
;  (guard-proc 'drawing-line-scale
;              _drawing-line-scale
;              (list 'drawing-line 'real)
;              (list drawing-line? real?)))

;;; Procedure:
;;;   drawing-line-top
;;; Parameters:
;;;   line, a drawing line
;;; Purpose:
;;;   Get the top edge of the drawing
;;; Produces:
;;;   top, a real
(define/contract drawing-line-top 
  (-> drawing-line? real?)
  (lambda (line)
    (min (list-ref line 4) (list-ref line 6))))

;(define drawing-line-top 
;  (guard-unary-proc 'drawing-line-top 
;                    _drawing-line-top
;                    'drawing-line 
;                    drawing-line?))

;;; Procedure:
;;;   drawing-line-vscale
;;; Parameters:
;;;   line, a drawing line
;;;   factor, a real number
;;; Purpose:
;;;   Create a new version of line, scaled vertically by factor
;;; Produces:
;;;   scaled, a drawing
(define/contract drawing-line-vscale
  (-> drawing-line? real? drawing?)
  (lambda (line factor)
    (drawing-line-core (drawing-line-color line)
                       (drawing-line-left line)
                       (* factor (drawing-line-top line))
                       (drawing-line-right line)
                       (* factor (drawing-line-bottom line))
                       (drawing-line-hstretch line)
                       (* factor (drawing-line-vstretch line)))))

;(define drawing-line-vscale
;  (guard-proc 'drawing-line-vscale
;              _drawing-line-vscale
;              (list 'drawing-line 'real)
;              (list drawing-line? real?)))

;;; Procedure:
;;;   drawing-line-vshift
;;; Parameters:
;;;   line, a drawing line
;;;   amt, a real number
;;; Purpose:
;;;   Create a new version of line, shifted vertically by amt
;;; Produces:
;;;   shifted, a drawing
(define/contract drawing-line-vshift
  (-> drawing-line? real? drawing?)
  (lambda (line amt)
    (drawing-line-core (drawing-line-color line)
                       (drawing-line-left line)
                       (+ amt (drawing-line-top line))
                       (drawing-line-right line)
                       (+ amt (drawing-line-bottom line))
                       (drawing-line-hstretch line)
                       (drawing-line-vstretch line))))

;(define drawing-line-vshift
;  (guard-proc 'drawing-line-vshift
;              _drawing-line-vshift
;              (list 'drawing-line 'real)
;              (list drawing-line? real?)))

;;; Procedure:
;;;   drawing-line-width
;;; Parameters:
;;;   line, a drawing line
;;; Purpose:
;;;   Get the width of the drawing
;;; Produces:
;;;   width, a real
(define/contract drawing-line-width
  (-> drawing-line? real?)
  (lambda (line)
    (abs (- (list-ref line 3) (list-ref line 5)))))

;(define drawing-line-width 
;  (guard-unary-proc 'drawing-line-width 
;                    _drawing-line-width
;                    'drawing-line 
;                    drawing-line?))


;;; Procedure:
;;;   drawing-members
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get a list of all the sub-drawings in drawing.
;;; Produces:
;;;   sub-drawings, a list of drawings
(define/contract drawing-members
  (-> drawing? (listof drawing?))
  (lambda (drawing)
    (if (eq? (drawing-type drawing) 'group)
        (caddr drawing)
        (list drawing))))

;(define drawing-members 
;  (guard-drawing-proc 'drawing-members _drawing-members))

;;; Procedure:
;;;   drawing-on-image?
;;; Parameters:
;;;   drawing, a drawing
;;;   image, an image
;;; Sam has forgotten to document the rest of this procedure. I guess it
;;;  determines whether the drawing can be put on the image and returns a
;;;   boolean.
(define/contract drawing-on-image?
  (-> drawing? image? boolean?)
  (lambda (drawing image)
    (and (>= (drawing-right drawing) 0)
         (>= (drawing-bottom drawing) 0)
         (< (drawing-left drawing) (image-width image))
         (< (drawing-top drawing) (image-height image)))))

;(define drawing-on-image?
;  (guard-proc 'drawing-on-image?
;              _drawing-on-image?
;              (list 'drawing 'image)
;              (list drawing? image?)))

;;; Procedure:
;;;   drawing-render!
;;; Parameters:
;;;   image, an image
;;;   drawing, a drawing
;;; Purpose:
;;;   Render drawing on image.
;;; Produces:
;;;   image, the input image
;;; Preconditions:
;;;   image is a valid image.
;;;   drawing is a valid drawing.
;;; Postconditions:
;;;   image has been extended by the appropriate drawing.
(define/contract drawing-render!
  (-> image? drawing? image?)
  (lambda (drawing image)
    (let ((type (drawing-type drawing)))
      (cond
        ((and (not (eq? (drawing-type drawing) 'group))
              (not (drawing-on-image? drawing image)))
         (error "drawing-render!: Drawing does not fit within image bounds"))
        ((eq? type 'blank))
        ((eq? type 'group)
         (drawing-group-render! drawing image))
        ((eq? type 'line)
         (drawing-line-render! drawing image))
        ((eq? type 'rule)
         (drawing-rule-render! drawing image))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape-render! drawing image))
        (else
         (error "drawing-render!: Unable to render" drawing))))
    image))

;(define drawing-render!
;  (lambda (drawing image)
;    (validate-params! 'drawing-render!
;                      (list 'drawing 'image)
;                      (list drawing? image?)
;                      (list drawing image))
;    (cond
;      ((and (not (eq? (drawing-type drawing) 'group))
;            (not (drawing-on-image? drawing image)))
;       (error "drawing-render!: Drawing does not fit within image bounds"
;              (list
;               'image-width: (image-width image)
;               'image-height: (image-height image)
;               'drawing-left: (drawing-left drawing)
;               'drawing-top: (drawing-top drawing)
;               'drawing-right: (drawing-right drawing)
;               'drawing-bottom: (drawing-bottom drawing)
;               )))
;      (else
;       (_drawing-render! drawing image)))))

;;; Procedure:
;;;   drawing-rectangle
;;; Purpose:
;;;   Create a new drawing that represents an rectangle
;;; Parameters:
;;;   left, a real number
;;;   top, a real number
;;;   width, a positive real number
;;;   height, a positive real number
;;; Produces:
;;;   rectangle, a drawing
;;; Preconditions:
;;;   width > 0.
;;;   height > 0.
;;; Postconditions:
;;;   When rendered, rectangle will be drawn as a filled rectangle, 
;;;   with the specified left margin, top margin, width, and
;;;   height.
(define/contract drawing-rectangle
  (-> real? real? (and/c real? positive?) (and/c real? positive?) drawing?)
  (lambda (left right width height)
    (drawing-shape 'rectangle (rgb-new 0 0 0) "" left right width height)))

;(define drawing-rectangle
;  (guard-proc 'drawing-rectangle
;              _drawing-rectangle
;              (list 'real 'real 'positive-real 'positive-real)
;              (list real? real? 
;                    (^and real? positive?) (^and real? positive?))))

;;; Procedure:
;;;   drawing-rule
;;; Parameters:
;;;   c1, a real
;;;   r1, a real
;;;   c2, a real
;;;   r2, a real
;;; Purpose:
;;;   Creates a drawing of a rule form (c1,r1) to (c2,r2)
;;; Produces:
;;;   rule, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   When rendered, rule is a rule from (c1,r1) to (c2,r2)
(define/contract drawing-rule
  (-> real? real? real? real? drawing?)
  (lambda (c1 r1 c2 r2)
    (drawing-rule-core (rgb-new 0 0 0) c1 r1 c2 r2)))

;(define drawing-rule
;  (guard-proc 'drawing-rule
;              _drawing-rule
;              (list 'real 'real 'real 'real)
;              (list real? real? real? real?)))

;;; Procedure:
;;;   drawing-rule-color
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the color of the rule
;;; Produces:
;;;   color, a color
;;; Preconditions:
;;;   (drawing-rule? drawing)
(define/contract drawing-rule-color 
  (-> drawing? color?)
  (r-s list-ref (list-index (drawing-format 'rule) 'color)))

;(define drawing-rule-color 
;  (guard-unary-proc 'drawing-rule-color 
;                    _drawing-rule-color
;                    'drawing-rule 
;                    drawing-rule?))


;;; Procedure:
;;;   drawing-rule-core
;;; Parameters:
;;;   color, a color
;;;   c1, a real
;;;   r1, a real
;;;   c2, a real
;;;   r2, a real
;;; Purpose:
;;;   Creates a drawing of a rule from (c1,r1) to (c2,r2)
;;; Produces:
;;;   rule, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   When rendered, rule is a rule from (c1,r1) to (c2,r2)
(define/contract drawing-rule-core
  (-> color? real? real? real? real? drawing?)
  (lambda (color c1 r1 c2 r2)
    (list 'drawing 'rule color c1 r1 c2 r2)))

;(define drawing-rule-core
;  (lambda params
;    (validate-params! 'drawing-rule-core
;                      (list 'color 'real 'real 'real 'real)
;                      (list color? real? real? real? real?)
;                      params)
;    (apply _drawing-rule-core params)))



;;; Procedure:
;;;   drawing-rule-bottom
;;; Parameters:
;;;   drawing, a drawing rule
;;; Purpose:
;;;   Get the bottom edge of the drawing
;;; Produces:
;;;   bottom, a real
;;; Preconditions:
;;;   (drawing-rule? drawing)
(define/contract drawing-rule-bottom 
  (-> drawing-rule? real?)
  (lambda (drawing)
    (max (list-ref drawing 4) (list-ref drawing 6))))

;(define drawing-rule-bottom 
;  (guard-unary-proc 'drawing-rule-bottom 
;                    _drawing-rule-bottom
;                    'drawing-rule 
;                    drawing-rule?))


;;; Procedure:
;;;   drawing-rule-height
;;; Parameters:
;;;   drawing, a drawing rule
;;; Purpose:
;;;   Get the height of the drawing
;;; Produces:
;;;   height, a real
;;; Preconditions:
;;;   (drawing-rule? drawing)
(define/contract drawing-rule-height
  (-> drawing-rule? real?)
  (lambda (drawing)
    (abs (- (list-ref drawing 4) (list-ref drawing 6)))))

;(define drawing-rule-height 
;  (guard-unary-proc 'drawing-rule-height 
;                    _drawing-rule-height
;                    'drawing-rule 
;                    drawing-rule?))


;;; Procedure:
;;;   drawing-rule-left
;;; Parameters:
;;;   drawing, a drawing rule
;;; Purpose:
;;;   Get the left edge of the drawing
;;; Produces:
;;;   left, a real
;;; Preconditions:
;;;   (drawing-rule? drawing)
(define/contract drawing-rule-left 
  (-> drawing-rule? real?)
  (lambda (drawing)
    (min (list-ref drawing 3) (list-ref drawing 5))))

;(define drawing-rule-left 
;  (guard-unary-proc 'drawing-rule-left 
;                    _drawing-rule-left
;                    'drawing-rule 
;                    drawing-rule?))

;;; Procedure:
;;;   drawing-rule-render!
;;; Parameters:
;;;   image, an image
;;;   drawing, a rule drawing
;;; Purpose:
;;;   Render drawing on image.
;;; Produces:
;;;   image, the input image
;;; Preconditions:
;;;   (image? image)
;;;   (drawing? drawing)
;;;   (drawing-line? drawing)
;;; Postconditions:
;;;   image has been extended by the appropriate drawing.
(define/contract drawing-rule-render!
  (-> image? drawing-rule? image?)
  (lambda (drawing image)
    (let ((c1 (drawing-rule-left drawing))
          (r1 (drawing-rule-top drawing))
          (c2 (drawing-rule-right drawing))
          (r2 (drawing-rule-bottom drawing))
          (saved-color (and (context-preserve?) (context-get-fgcolor)))
          (saved-brush (and (context-preserve?) (context-get-brush))))
      (context-set-fgcolor! "grey")
      (gimp-pencil (image-get-layer image)
                   4
                   (vector c1 r1 c2 r2))
      (context-set-brush! "Circle (01)")
      ; (image-draw-line! image c1 r1 c2 r2)
      (cond (saved-color (context-set-fgcolor! saved-color)))
      (cond (saved-brush (context-set-brush! saved-brush)))
      image)))

;(define drawing-rule-render!
;  (guard-proc 'drawing-rule-render!
;              _drawing-rule-render!
;              (list 'drawing-rule 'image)
;              (list drawing-rule? image?)))

;;; Procedure:
;;;   drawing-rule-right
;;; Parameters:
;;;   drawing, a drawing rule
;;; Purpose:
;;;   Get the right edge of the drawing
;;; Produces:
;;;   right, a real
;;; Preconditions:
;;;   (drawing-rule? drawing)
(define/contract drawing-rule-right 
  (-> drawing-rule? real?)
  (lambda (drawing)
    (max (list-ref drawing 3) (list-ref drawing 5))))

;(define drawing-rule-right 
;  (guard-unary-proc 'drawing-rule-right 
;                    _drawing-rule-right
;                    'drawing-rule 
;                    drawing-rule?))

;;; Procedure:
;;;   drawing-right
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Find the right edge of drawing
;;; Produces:
;;;   right, a real
(define/contract drawing-right
  (-> drawing? real?)
  (lambda (drawing)
    (let ((type (drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         0)
        ((eq? type 'group)
         (apply max (map drawing-right (drawing-members drawing))))
        ((eq? type 'line)
         (drawing-line-right drawing))
        ((eq? type 'rule)
         (drawing-rule-right drawing))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape-right drawing))
        (else
         (error "drawing-right: Unknown drawing type: " type))))))

;(define drawing-right
;  (guard-drawing-proc 'drawing-right _drawing-right))

;;; Procedure:
;;;   drawing-rule-top
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the top edge of the drawing
;;; Produces:
;;;   top, a real
;;; Preconditions:
;;;   (drawing-rule? drawing)
(define/contract drawing-rule-top 
  (-> drawing? real?)
  (lambda (drawing)
    (min (list-ref drawing 4) (list-ref drawing 6))))

;(define drawing-rule-top 
;  (guard-unary-proc 'drawing-rule-top 
;                    _drawing-rule-top
;                    'drawing-rule 
;                    drawing-rule?))

;;; Procedure:
;;;   drawing-rule-width
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the width of the drawing
;;; Produces:
;;;   width, a real
;;; Preconditions:
;;;   (drawing-rule? drawing)
(define/contract drawing-rule-width
  (-> drawing? real?)
  (lambda (drawing)
    (abs (- (list-ref drawing 3) (list-ref drawing 5)))))

;(define drawing-rule-width 
;  (guard-unary-proc 'drawing-rule-width 
;                    _drawing-rule-width
;                    'drawing-rule 
;                    drawing-rule?))


;;; Procedure:
;;;   drawing-scale
;;; Parameters:
;;;   drawing, a drawing
;;;   factor, a real number
;;; Purpose:
;;;   Create a new version of drawing that is scaled by the
;;;   specified factor.
;;; Produces:
;;;   scaled, a drawing
;;; Postconditions:
;;;   scaled is the same overall "shape" and color as drawing, but
;;;   is larger or smaller, based on the scale factor.
(define/contract drawing-scale
  (-> drawing? real? drawing?)
  (lambda (drawing factor)
    (let ((type (drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         drawing)
        ((eq? type 'group)
         (apply drawing-group
                (map (r-s drawing-scale factor)
                     (drawing-members drawing))))
        ((eq? type 'line)
         (drawing-line-scale drawing factor))
        ((eq? type 'rule)
         (drawing-rule-core (drawing-rule-color drawing)
                            (* factor (drawing-rule-left drawing))
                            (* factor (drawing-rule-top drawing))
                            (* factor (drawing-rule-right drawing))
                            (* factor (drawing-rule-bottom drawing))))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape type
                        (drawing-color drawing) 
                        (drawing-brush drawing)
                        (* factor (drawing-left drawing))
                        (* factor (drawing-top drawing))
                        (* factor (drawing-width drawing)) 
                        (* factor (drawing-height drawing))))
        (else
         (error "drawing-scale: unknown drawing type" type))))))

;(define drawing-scale
;  (guard-proc 'drawing-scale
;              _drawing-scale
;              (list 'drawing 'real)
;              (list drawing? real?)))


;;; Procedure:
;;;   drawing-shape
;;; Parameters:
;;;   type, a symbol
;;;   color, a color
;;;   brush, a string
;;;   left, a real
;;;   top, a real
;;;   width, a real
;;;   height, a real
;;; Purpose:
;;;   Create one of the shape drawings
;;; Produces:
;;;   shape, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (drawing-type drawing) = type
;;;   (drawing-color drawing) = color
;;;   (drawing-brush drawing) = brush
;;;   (drawing-left drawing) = left
;;;   (drawing-top drawing) = top
;;;   (drawing-width drawing) = width
;;;   (drawing-height drawing) = height
(define/contract drawing-shape
  (-> symbol? color? string? real? real? real? real? drawing?)
  (lambda (type color brush left top width height)
    (list 'drawing type color brush left top width height)))

;(define drawing-shape
;  (lambda params
;    (validate-params! 'drawing-shape
;                      (list 'symbol 'color 'brush 'real 'real 'real 'real)
;                      (list symbol? color? string? real? real? real? real?)
;                      params)
;    (apply _drawing-shape params)))


;;; Procedure:
;;;   drawing-shape-brush 
;;; Parameter:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the brush associated with the drawing
;;; Produces:
;;;   brush, a string
;;; Preconditions:
;;;   (drawing-shape? drawing)
(define/contract drawing-shape-brush
  (-> drawing? string?)
  (r-s list-ref 3))

;(define drawing-shape-brush
;  (guard-unary-proc 'drawing-shape-brush
;                    _drawing-shape-brush
;                    'drawing-shape
;                    drawing-shape?))

;;; Procedure:
;;;   drawing-shape-right
;;; Parameters:
;;;   drawing, a drawing shape
;;; Purpose:
;;;   Grab the right edge of the shape
;;; Produces:
;;;   right, a real
;;; Preconditions:
;;;   (drawing-shape? drawing)
(define/contract drawing-shape-right
  (-> drawing-shape? real?)
  (lambda (drawing)
    (+ (drawing-shape-left drawing) (drawing-shape-width drawing))))

;(define drawing-shape-right 
;  (guard-unary-proc 'drawing-shape-right 
;                    _drawing-shape-right
;                    'drawing-shape 
;                    drawing-shape?))


;;; Procedure:
;;;   drawing-shape-width
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the width of the drawing
;;; Produces:
;;;   width, a real
;;; Preconditions:
;;;   (drawing-shape? drawing)
(define/contract drawing-shape-width
  (-> drawing? real?)
  (r-s list-ref 6))

;(define drawing-shape-width 
;  (guard-unary-proc 'drawing-shape-width 
;                    _drawing-shape-width
;                    'drawing-shape 
;                    drawing-shape?))


;;; Procedure:
;;;   drawing-shape-bottom
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the bottom edge of the drawing
;;; Produces:
;;;   bottom, a real
;;; Preconditions:
;;;   (drawing-shape? drawing)
(define/contract drawing-shape-bottom
  (-> drawing? real?)
  (lambda (drawing)
    (+ (drawing-shape-top drawing) (drawing-shape-height drawing))))

;(define drawing-shape-bottom 
;  (guard-unary-proc 'drawing-shape-bottom 
;                    _drawing-shape-bottom
;                    'drawing-shape 
;                    drawing-shape?))


;;; Procedure:
;;;   drawing-shape-color 
;;; Parameter:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the color associated with the drawing
;;; Produces:
;;;   color, a color
;;; Preconditions:
;;;   (drawing-shape? drawing)
(define/contract drawing-shape-color
  (-> drawing? color?)
  (r-s list-ref 2))

;(define drawing-shape-color
;  (guard-unary-proc 'drawing-shape-color
;                    _drawing-shape-color
;                    'drawing-shape
;                    drawing-shape?))


;;; Procedure:
;;;   drawing-shape-height
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the height of the drawing
;;; Produces:
;;;   height, a real
;;; Preconditions:
;;;   (drawing-shape? drawing)
(define/contract drawing-shape-height 
  (-> drawing? real?)
  (r-s list-ref 7))

;(define drawing-shape-height 
;  (guard-unary-proc 'drawing-shape-height 
;                    _drawing-shape-height
;                    'drawing-shape 
;                    drawing-shape?))


;;; Procedure:
;;;   drawing-shape-left
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the left edge of the shape
;;; Produces:
;;;   left, a real
;;; Preconditions:
;;;   (drawing-shape? drawing)
(define/contract drawing-shape-left
  (-> drawing? real?)
  (r-s list-ref 4))

;(define drawing-shape-left 
;  (guard-unary-proc 'drawing-shape-left 
;                    _drawing-shape-left
;                    'drawing-shape 
;                    drawing-shape?))

;;; Procedure:
;;;   drawing-shape-top
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the top edge of the drawing
;;; Produces:
;;;   top, a real
;;; Preconditions:
;;;   (drawing-shape? drawing)
(define/contract drawing-shape-top
  (-> drawing? real?)
  (r-s list-ref 5))

;(define drawing-shape-top 
;  (guard-unary-proc 'drawing-shape-top 
;                    _drawing-shape-top
;                    'drawing-shape 
;                    drawing-shape?))

;;; Procedure:
;;;   drawing-top
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Find the top edge of drawing
;;; Produces:
;;;   top, a real
(define/contract drawing-top
  (-> drawing? real?)
  (lambda (drawing)
    (let ((type (drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         0)
        ((eq? type 'group)
         (apply min (map drawing-top (drawing-members drawing))))
        ((eq? type 'line)
         (drawing-line-top drawing))
        ((eq? type 'rule)
         (drawing-rule-top drawing))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape-top drawing))
        (else
         (error "drawing-top: unknown drawing type: " type))))))

;(define drawing-top
;  (guard-drawing-proc 'drawing-top _drawing-top))

;;; Procedure:
;;;   drawing-shape-render!
;;; Parameters:
;;;   image, an image
;;;   drawing, a drawing shape (ellipse or rectangle)
;;; Purpose:
;;;   Render drawing on image.
;;; Produces:
;;;   image, the input image
;;; Preconditions:
;;;   image is a valid image.
;;;   drawing is a valid drawing.
;;; Postconditions:
;;;   image has been extended by the appropriate drawing.
(define/contract drawing-shape-render!
  (-> image? drawing-shape? image?)
  (lambda (drawing image)
    (let ((select! (if (eq? (drawing-type drawing) 'ellipse) 
                       image-select-ellipse! 
                       image-select-rectangle!))
          (fgcolor (context-get-fgcolor)))
      (select! image REPLACE
               (drawing-left drawing) (drawing-top drawing)
               (drawing-width drawing) (drawing-height drawing))
      (context-set-fgcolor! (drawing-color drawing))
      (if (drawing-filled? drawing)
          (image-fill-selection! image)
          (let ((brush (context-get-brush)))
            (context-set-brush! (drawing-brush drawing))
            (image-stroke-selection! image)
            (cond ((context-preserve?) (context-set-brush! brush)))))
      (image-select-nothing! image)
      (cond ((context-preserve?) (context-set-fgcolor! fgcolor))))))

;(define drawing-shape-render!
;  (guard-proc 'drawing-shape-render!
;              _drawing-shape-render!
;              (list 'drawing-shape 'image)
;              (list drawing-shape? image?)))

;;; Procedure:
;;;   drawing-type
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the type of drawing.
;;; Produces:
;;;   type, a symbol.
(define/contract drawing-type
  (-> drawing? symbol?)
  cadr)

;(define drawing-type
;  (guard-drawing-proc 'drawing-type _drawing-type))


; Value:
;   drawing-unit-circle
; Description:
;   A circle with diameter 1, centered at 0,0
(define drawing-unit-circle (drawing-ellipse -0.5 -0.5 1 1))


; Value:
;   drawing-unit-square
; Description:
;   A circle with edge length 1, centered at 0,0
(define drawing-unit-square (drawing-rectangle -0.5 -0.5 1 1))

;;; Procedure:
;;;   drawing-vscale
;;; Parameters:
;;;   drawing, a drawing
;;;   factor, a number
;;; Purpose:
;;;   Create a new version of drawing that is vertically scaled by the
;;;   specified factor.
;;; Produces:
;;;   scaled, a drawing
;;; Postconditions:
;;;   scaled has the same width, color, and form (filled or outlined)
;;;   as drawing, but the height is scaled by factor (as is the top
;;;   margin).
(define/contract drawing-vscale
  (-> drawing? number? drawing?)
  (lambda (drawing factor)
    (let ((type (drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         drawing)
        ((eq? type 'group)
         (apply drawing-group 
                (map (r-s drawing-vscale factor)
                     (drawing-members drawing))))
        ((eq? type 'line)
         (drawing-line-vscale drawing factor))
        ((eq? type 'rule)
         (drawing-rule-core (drawing-rule-color drawing)
                            (drawing-rule-left drawing)
                            (* factor (drawing-rule-top drawing))
                            (drawing-rule-right drawing)
                            (* factor (drawing-rule-bottom drawing))))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape type
                        (drawing-color drawing) 
                        (drawing-brush drawing)
                        (drawing-left drawing)
                        (* factor (drawing-top drawing))
                        (drawing-width drawing)
                        (* factor (drawing-height drawing))))
        (else
         (error "drawing-vscale: unknown drawing type" type))))))

;(define drawing-vscale
;  (guard-proc 'drawing-vscale
;              _drawing-vscale
;              (list 'drawing 'real)
;              (list drawing? real?)))

;;; Procedure:
;;;   drawing-vshift
;;; Parameters:
;;;   drawing, a drawing
;;;   amt, a number
;;; Purpose:
;;;   Create a new version of drawing that is shifted vertically by
;;;   the specified amt.
;;; Produces:
;;;   shifted , a drawing
;;; Postconditions:
;;;   scaled is the same overall "shape", color, and size as
;;;   drawing, but shifted down by amt (or up by |amt|, if 
;;;   amt is negative).
(define/contract drawing-vshift
  (-> drawing? number? drawing?)
  (lambda (drawing amt)
    (let ((type (drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         drawing)
        ((eq? type 'group)
         (apply drawing-group
                (map (r-s drawing-vshift amt)
                     (drawing-members drawing))))
        ((eq? type 'line)
         (drawing-line-vshift drawing amt))
        ((eq? type 'rule)
         (drawing-rule-core (drawing-rule-color drawing)
                            (drawing-rule-left drawing)
                            (+ amt (drawing-rule-top drawing))
                            (drawing-rule-right drawing)
                            (+ amt (drawing-rule-bottom drawing))))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape type
                        (drawing-color drawing) 
                        (drawing-brush drawing)
                        (drawing-left drawing)
                        (+ amt (drawing-top drawing))
                        (drawing-width drawing) 
                        (drawing-height drawing)))
        (else
         (error "drawing-vshift: unknown drawing type" type))))))

;(define drawing-vshift
;  (guard-proc 'drawing-vshift
;              _drawing-vshift
;              (list 'drawing 'real)
;              (list drawing? real?)))


;;; Procedure:
;;;   drawing-width
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the width of drawing
;;; Produces:
;;;   width, a real
(define/contract drawing-width
  (-> drawing? real?)
  (lambda (drawing)
    (let ((type (drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         drawing)
        ((eq? type 'group)
         (- (drawing-right drawing) (drawing-left drawing)))
        ((eq? type 'line)
         (drawing-line-width drawing))
        ((eq? type 'rule)
         (drawing-rule-width drawing))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape-width drawing))
        (else
         (error "drawing-width: Unknown drawing type: " type))))))

;(define drawing-width
;  (guard-drawing-proc 'drawing-width _drawing-width))

;;; Procedure:
;;;   drawing-ellipse?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;    Determine whether val can be appropriately interpreted as a
;;;   rectagular drawing (e.g., built by scaling, shifting, and/or
;;;   recoloring the unit circle).
;;; Produces:
;;;   is-drawing?, a Boolean
(define/contract drawing-ellipse?
  (-> any/c boolean?)
  (^and drawing-shape? (o (l-s eq? 'ellipse) drawing-type)))

;(define drawing-ellipse? _drawing-ellipse?)

;;; Procedure:
;;;   drawing-rectangle?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;    Determine whether val can be appropriately interpreted as a
;;;   rectagular drawing (e.g., built by scaling, shifting, and/or
;;;   recoloring the unit square).
;;; Produces:
;;;   is-drawing?, a Boolean
(define/contract drawing-rectangle?
  (-> any/c boolean?)
  (^and drawing-shape? (o (l-s eq? 'rectangle) drawing-type)))

;(define drawing-rectangle? _drawing-rectangle?)

; +------------------------------------------+------------------------
; | Alternate versions of drawing procedures |
; +------------------------------------------+

(define scale-drawing (swap-params drawing-scale))
;(define scale-drawing
;  (guard-proc 'scale-drawing
;              _scale-drawing
;              (list 'real 'drawing)
;              (list real? drawing?)))

(define hscale-drawing (swap-params drawing-hscale))
;(define hscale-drawing
;  (guard-proc 'hscale-drawing
;              _hscale-drawing
;              (list 'real 'drawing)
;              (list real? drawing?)))

(define vscale-drawing (swap-params drawing-vscale))
;(define vscale-drawing
;  (guard-proc 'vscale-drawing
;              _vscale-drawing
;              (list 'real 'drawing)
;              (list real? drawing?)))

(define hshift-drawing (swap-params drawing-hshift))
;(define hshift-drawing
;  (guard-proc 'hshift-drawing
;              _hshift-drawing
;              (list 'real 'drawing)
;              (list real? drawing?)))

(define vshift-drawing (swap-params drawing-vshift))
;(define vshift-drawing
;  (guard-proc 'vshift-drawing
;              _vshift-drawing
;              (list 'real 'drawing)
;              (list real? drawing?)))

(define recolor-drawing (swap-params drawing-recolor))
;(define recolor-drawing
;  (guard-proc 'recolor-drawing
;              _recolor-drawing
;              (list 'color 'drawing)
;              (list color? drawing?)))
