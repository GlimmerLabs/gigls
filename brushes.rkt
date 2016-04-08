#lang racket
(provide (all-defined-out))
(require gigls/pdb-dbus)
(require louDBus/unsafe)
(require gigls/guard
         gigls/higher
         gigls/strings
         gigls/utils)

; +------------+------------------------------------------------------
; | Predicates |
; +------------+

;;; Procedure:
;;;   brush-info?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val represents information on a brush
;;;   (e.g., if it was produced by brush-info)
;;; Produces:
;;;   is-brush-info?, a Boolean
(define/contract brush-info?
  (-> any/c boolean?)
  (lambda (val)
    (and (vector? val)
         (= (vector-length val) 9)
         (eq? (vector-ref val 0) 'brush)
         (brush-name? (vector-ref val 1))
         (brush-valid-shape? (vector-ref val 2))
         (brush-valid-radius? (vector-ref val 3))
         (brush-valid-spikes? (vector-ref val 4))
         ((^and real? (l-s <= 0) (r-s <= 1)) (vector-ref val 5))   ; hardness
         ((^and real? (l-s <= 1)) (vector-ref val 6))              ; aspect raio
         ((^and real? (l-s <= 0) (r-s <= 180)) (vector-ref val 7))  ; angle
         ((^and real? (l-s <= 1) (r-s <= 1000)) (vector-ref val 8)) ; spacing
         #t)))

;;; Procedure:
;;;   brush-name?
;;; Parameters:
;;;   brush, a string
;;; Purpose:
;;;   Determines if brush names a string
;;; Produces:
;;;   is-brush-name?, a boolean value
(define/contract brush-name?
  (-> any/c boolean?)
  (lambda (brush)
    (and (string? brush)
         (member? brush (brushes-list brush)))))

;(define brush-name? _brush-name?)

;;; Procedure:
;;;   brush?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val is a brush.
;;; Produces:
;;;   is-brush?, a Boolean.
;;; Points:
;;;   The #t at the end may seem a bit odd, since it's pointless.  However,
;;;   it ensures that brush? returns a Boolean.
(define/contract brush?
  (-> any/c (-> any/c boolean?))
  (lambda (val)
    (and string? (or brush-name? brush-info?))))

;;; Procedure:
;;;   brush-generated?
;;; Parameters:
;;;   brush, a string
;;; Purpose:
;;;   Determine if it's a generated brush (which means that we get 
;;;   various attributes
;;;   values).
(define/contract brush-generated?
  (-> string? boolean?)
  (lambda (brush)
    (and (string? brush)
         (brush-name? brush)
         (= 1 (car (gimp-brush-is-generated brush))))))

;;; Procedure:
;;;   brush-mutable?
;;; Parameters:
;;;   brush, a string
;;; Purpose:
;;;   Determines if the brush (or at least a copy of the brush) can be 
;;;   mutated.  That is, can you set one of the key attributes?
;;; Produces:
;;;   mutable? boolean
;;; Preconditions:
;;;   (brush-name? brush)
;;; Postconditions:
;;;   If mutable?, then it is safe to call brush-set-ATTRIBUTE, where
;;;   ATTRIBUTE is one of the key attributes (shape, size, radius, etc.)
(define brush-mutable?
  brush-generated?)

;;; Procedure:
;;;   brush-valid-angle?
;;; Parameter:
;;;   angle, a real number
;;; Purpose:
;;;   Determines if angle is a valid angle for a brush
;;; Produces:
;;;   is-valid?, a boolean
(define/contract brush-valid-angle?
  (-> real? boolean?)
  (lambda (val)
    (and (real? val) (<= 0 val 180))))

;;; Procedure:
;;;   brush-valid-aspect-ratio?
;;; Parameters:
;;;   aspect-ratio, a real number
;;; Purpose:
;;;   Determines if aspect-ration is a valid aspect ratio for a brush
;;; Produces:
;;;   is-valid?, a boolean
(define/contract brush-valid-aspect-ratio?
  (-> real? boolean?)
  (lambda (val)
    (and (real? val (<= 1 val 1000)))))

(define brush-valid-aspect-ratio 'real-between-1-and-1000)

;;; Procedure:
;;;   brush-valid-radius?
;;; Parameters:
;;;   radius, a real number
;;; Purpose:
;;;   Determines if radius is a valid radius for a brush
;;; Produces:
;;;   is-valid?, a boolean
(define brush-valid-radius?
  (^and real? positive? (r-s <= 1000)))

(define brush-valid-radius 'positive-real-less-than-1000)

;;; Procedure:
;;;   brush-valid-shape?
;;; Parameters:
;;;   shape, an integer
;;; Purpose:
;;;   Determine if shape is a valid brush shape
;;; Produces:
;;;   is-valid?, a boolean value
(define brush-valid-shape?
  (r-s member? '(0 1 2 square circle diamond other)))

;;; Procedure
;;;   brush-valid-spikes?
;;; Parameters:
;;;   spikes, an integer
;;; Purpose:
;;;   Determines if spikes is a valid number of spikes
;;; Produces:
;;;   is-valid?, a boolean
(define brush-valid-spikes?
  (^and integer? (l-s <= 2)))

(define brush-valid-spikes 'integer-greater-than-1)


; +-----------+-------------------------------------------------------
; | Observers |
; +-----------+

;;; Procedure:
;;;   brushes-list
;;; Parameters:
;;;   pattern, a string
;;; Purpose:
;;;   List all of the brushes containing the pattern.
;;; Produces:
;;;   brushes, a list
(define/contract brushes-list
  (-> string? (listof string?))
  (lambda (pattern)
    (let ((brushes (cadr (gimp-brushes-get-list (string-escape pattern)))))
      (if (vector? brushes)
          (vector->list brushes)
          brushes))))

;(define brushes-list
;  (guard-unary-proc 'brushes-list
;                    _brushes-list
;                    'string
;                    string?))

;;; Procedure:
;;;   brush-get-angle
;;; Parameters:
;;;   brush, a brush (either a brush name or brush info)
;;; Purpose:
;;;   Get the angle of the brush
;;; Produces:
;;;   angle, a real
(define/contract brush-get-angle
  (-> brush? real?)
  (lambda (brush)
    (cond
      [(brush-name? brush)
       (if (brush-generated? brush)
           (car (gimp-brush-get-angle brush))
           0.0)]
      [(brush-info? brush)
       (vector-ref brush 7)]
      [else (error/parameter-type 'brush-get-angle
                                  0
                                  'brush
                                  (list brush))])))

;(define brush-get-angle _brush-get-angle)

;;; Procedure:
;;;   brush-get-aspect-ratio
;;; Parameters:
;;;   brush, a brush (either a brush name or brush info)
;;; Purpose:
;;;   Get the aspect ratio of the brush
;;; Produces:
;;;   aspect-ratio, a real
(define/contract brush-get-aspect-ratio
  (-> brush? real?)
  (lambda (brush)
    (cond
      [(brush-name? brush)
       (if (brush-generated? brush)
           (car (gimp-brush-get-aspect-ratio brush))
           1.0)]
      [(brush-info? brush)
       (vector-ref brush 6)]
      [else (error/parameter-type 'brush-get-aspect-ratio
                                  0
                                  'brush
                                  (list brush))])))

;(define brush-get-aspect-ratio _brush-get-aspect-ratio)

;;; Procedure:
;;;   brush-get-hardness
;;; Parameters:
;;;   brush, a brush (either a brush name or brush info)
;;; Purpose:
;;;   Get the hardness of the brush
;;; Produces:
;;;   hardness, a real
(define/contract brush-get-hardness
  (-> brush? real?)
  (lambda (brush)
    (cond
      [(brush-name? brush)
       (if (brush-generated? brush)
           (car (gimp-brush-get-hardness brush))
           0.0)]
      [(brush-info? brush)
       (vector-ref brush 5)]
      [else (error/parameter-type 'brush-get-hardness
                                  0
                                  'brush
                                  (list brush))])))

;(define brush-get-hardness _brush-get-hardness)

;;; Procedure:
;;;   brush-get-radius
;;; Parameters:
;;;   brush, a brush (either a brush name or brush info)
;;; Purpose:
;;;   Get the radius of the brush
;;; Produces:
;;;   radius, a real
(define/contract brush-get-radius
  (-> brush? real?)
  (lambda (brush)
    (cond
      [(brush-name? brush)
       (if (brush-generated? brush)
           (car (gimp-brush-get-radius brush))
           25.0)]
      [(brush-info? brush)
       (vector-ref brush 3)]
      [else (error/parameter-type 'brush-get-radius
                                  1
                                  'brush
                                  (list brush))])))

;(define brush-get-radius _brush-get-radius)

;;; Procedure:
;;;   brush-get-shape
;;; Parameters:
;;;   brush, a brush (either a brush name or brush info)
;;; Purpose:
;;;   Get the shape of the brush
;;; Produces:
;;;   shape, a symbol
;;; Postconditions:
;;;   (member? shape '(circle square diamond other))
(define/contract brush-get-shape
  (-> brush? symbol?)
  (lambda (brush)
    (cond
      [(brush-name? brush)
       (if (brush-generated? brush)
           (brush-shape-number-to-name (car (gimp-brush-get-shape brush)))
           'other)]
      [(brush-info? brush)
       (brush-shape-number-to-name (vector-ref brush 2))]
      [else (error/parameter-type 'brush-get-shape
                                  1
                                  'brush
                                  (list brush))])))

;(define brush-get-shape _brush-get-shape)

;;; Procedure:
;;;   brush-get-spacing
;;; Parameters:
;;;   brush, a brush (either a brush name or brush info)
;;; Purpose:
;;;   Get the spacing of the brush
;;; Produces:
;;;   spacing, a real
(define/contract brush-get-spacing
  (-> brush? real?)
  (lambda (brush)
    (cond
      [(brush-name? brush)
       (car (gimp-brush-get-spacing brush))]
      [(brush-info? brush)
       (vector-ref brush 8)]
      [else (error/parameter-type 'brush-get-spacing
                                  0
                                  'brush
                                  (list brush))])))

;(define brush-get-spacing _brush-get-spacing)

;;; Procedure:
;;;   brush-get-spikes
;;; Parameters:
;;;   brush, a brush (either a brush name or brush info)
;;; Purpose:
;;;   Get the number of spikes for the brush
;;; Produces:
;;;   spikes, an integer
(define/contract brush-get-spikes
  (-> brush? (and/c number? integer?))
  (lambda (brush)
    (cond
      [(brush-name? brush)
       (if (brush-generated? brush)
           (car (gimp-brush-get-spikes brush))
           2)]
      [(brush-info? brush)
       (vector-ref brush 4)]
      [else (error/parameter-type 'brush-get-spikes
                                  0
                                  'brush
                                  (list brush))])))

;(define brush-get-spikes _brush-get-spikes)

;;; Procedure:
;;;   brush-info
;;; Parameters:
;;;   name, a string
;;; Purpose:
;;;   To create a new vector that represents the
;;;   current brush info
(define/contract brush-info
  (-> string? any/c)
  (lambda (name)
    (vector 'brush                        ; 0
            name                          ; 1
            (brush-get-shape name)        ; 2
            (brush-get-radius name)       ; 3
            (brush-get-spikes name)       ; 4
            (brush-get-hardness name)     ; 5
            (brush-get-aspect-ratio name) ; 6
            (brush-get-angle name)        ; 7
            (brush-get-spacing name))))   ; 8

;(define brush-info
;  (guard-unary-proc 'brush-info _brush-info
;                    "brush name" brush-name?))

;;; Procedure:
;;;   brush-original
;;; Parameters:
;;;   brush, a string
;;; Purpose:
;;;   Finds the original version of brush.
;;; Produces:
;;;   original, a string
(define/contract brush-original
  (-> string? string?)
  (lambda (brush)
    (cond
      [(string-ends-with? brush " (editable)")
       (substring brush 
                  0 
                  (- (string-length brush) (string-length " (editable)")))]
      
      [(string-ends-with? brush " copy")
       (substring brush 0 (- (string-length brush) (string-length " copy")))]
      [else brush])))

;(define brush-original
;  (guard-unary-proc 'brush-original
;                    _brush-original
;                    'string
;                    string?))

;;; Procedure:
;;;   brush-shape-to-number
;;; Parameters:
;;;   shape, a brush shape
;;; Purpose:
;;;   Convert a shape (number or name) to the corresponding number
;;; Produces:
;;;   num, a shape number
(define/contract brush-shape-to-number
  (-> brush-valid-shape? number?)
  (lambda (shape)
    (cond
      ([eq? shape 'circle] 0)
      ([eq? shape 'square] 1)
      ([eq? shape 'diamond] 2)
      (else shape))))

;(define brush-shape-to-number
;  (guard-unary-proc 'brush-shape-to-number
;                    _brush-shape-to-number
;                    'shape
;                    brush-valid-shape?))

;;; Procedure:
;;;   brush-shape-number-to-name
;;; Parameters:
;;;   shape, an integer
;;; Purpose:
;;;   Converts a shape number to a corresponding symbol
;;; Produces:
;;;   shapename, a symbol
(define/contract brush-shape-number-to-name
  (-> (and/c number? integer?) symbol?)
  (lambda (shape)
    (vector-ref #(circle square diamond) shape)))

;(define brush-shape-number-to-name
;  (guard-unary-proc 'brush-shape-number-to-name
;                    _brush-shape-number-to-name
;                    'shape
;                    (r-s member? '(0 1 2))))


; +----------+--------------------------------------------------------
; | Mutators |
; +----------+

;;; Procedure:
;;;   brush-make-editable
;;; Parameters:
;;;   name, a string
;;; Purpose:
;;;   Create an editable version of the give brun
;;; Produces:
;;;   editable, a string
;;; Preconditions:
;;;   (brush-name? name)
;;; Postconditions:
;;;   editable names an editable brush
(define/contract brush-make-editable
  (-> string? string?)
  (lambda (brush)
    (let ([editable (if (string-contains? brush " (editable)")
                        brush
                        (string-append brush " (editable)"))])
      (when (not (brush? editable))
        (let ([duplicate-brush (car (gimp-brush-duplicate brush))])
          (gimp-brush-rename duplicate-brush editable)))
      editable)))

;(define brush-make-editable
;  (guard-unary-proc 'brush-make-editable
;                    _brush-make-editable
;                    'brush-name
;                    brush-name?))

;;; Procedure:
;;;   brush-set-angle!
;;; Parameters:
;;;   brush, a brush
;;;   angle, a real number
;;; Purpose:
;;;   Sets the angle of the brush to angle (in degrees)
;;; Produces;
;;;   brush, the same brush
;;; Preconditions:
;;;   brush is a mutable brush
;;; Postconditions:
;;;   (brush-get-angle brush) correspondes to angle mod 180.
(define _brush-set-angle!
  (lambda (brush angle)
    (cond
      [(< angle 0)
       (_brush-set-angle! brush (+ angle 360))]
      [(brush-name? brush)
       (let ([editable (brush-make-editable brush)])
         (gimp-brush-set-angle brush angle)
         editable)]
      [(brush-info? brush)
       (vector-set! brush 7 angle)]
      [else
       (error/parameter-type 'brush-set-angle! 1 'brush (list brush angle))])))

(define brush-set-angle!
  (guard-proc 'brush-set-angle!
              _brush-set-angle!
              (list 'brush 'real)
              (list brush? real?)))

;;; Procedure:
;;;   brush-set-aspect-ratio
;;; Parameters:
;;;   brush, a brush
;;;   aspect-ratio, a real number
;;; Purpose:
;;;   Set the aspect ratio of brush to aspect-ratio
;;; Produces:
;;;   brush, the same brush
;;; Preconditions:
;;;   1 <= aspect-ratio <= 1000
;;;   brush is a mutable brush
;;; Postconditions:
;;;   (brush-get-aspect-ratio brush) is aspect-ratio
(define _brush-set-aspect-ratio!
  (lambda (brush aspect-ratio)
    (cond
      [(not (brush-valid-aspect-ratio aspect-ratio))
       (error/parameter-type 'brush-set-aspect-ratio! 
                             2
                             brush-valid-aspect-ratio
                             (list brush aspect-ratio))]
      [(brush-name? brush)
       (let ([editable (brush-make-editable brush)])
         (gimp-brush-set-aspect-ratio brush aspect-ratio)
         editable)]
      [(brush-info? brush)
       (vector-set! brush 6 aspect-ratio)]
      [else
       (error/parameter-type 'brush-set-aspect-ratio! 
                             1
                             'brush
                             (list brush aspect-ratio))])))

(define brush-set-aspect-ratio! _brush-set-aspect-ratio!)

;;; Procedure:
;;;   brush-set-radius!
;;; Parameters:
;;;   brush, a brush
;;;   radius, a real number
;;; Purpose:
;;;   Sets the radius of the brush to radius
;;; Produces;
;;;   brush, the same brush
(define _brush-set-radius!
  (lambda (brush radius)
    (cond
      [(brush-name? brush)
       (let ([editable (brush-make-editable brush)])
         (gimp-brush-set-radius brush radius)
         editable)]
      [(brush-info? brush)
       (vector-set! brush 3 radius)]
      [else
       (error/parameter-type 'brush-set-radius! 1 'brush (list brush radius))])))

(define brush-set-radius!
  (guard-proc 'brush-set-radius!
              _brush-set-radius!
              (list 'brush 'positive-real)
              (list brush? (^and real? positive?))))

;;; Procedure:
;;;   brush-set-shape!
;;; Parameters:
;;;   brush, a brush
;;;   shape, a shape
;;; Purpose:
;;;   Set the shape of the brush
;;; Produces:
;;;   brush, the same brush
;;; Preconditions:
;;;   shape is a valid shape (either 0, 1, 2, 'square, 'circle, or 'diamond)
;;;   brush is a mutable brush
;;; Postconditions:
;;;   (brush-get-shape brush) is equivalent to shape
(define _brush-set-shape!
  (lambda (brush shape)
    (cond
      [(not (brush-valid-shape? shape))
       (error error/parameter-type 'brush-set-shape! 2 'brush-shape
              (list brush shape))]
      [(brush-name? brush)
       (when (not (brush-generated? brush))
         (error "brush-set-shape!: Cannot set shape of brush " brush))
       (let ([editable (brush-make-editable brush)])
         (gimp-brush-set-shape brush shape)
         editable)]
      [(brush-info? brush)
       (vector-set! brush 2 shape)]
      [else
       (error/parameter-type 'brush-set-shape! 1 'brush 
                             (list brush shape))])))

(define brush-set-shape! _brush-set-shape!)

;;; Procedure:
;;;   brush-set-spikes!
;;; Parameters:
;;;   brush, a brush
;;;   spikes, a positive integer
;;; Purpose:
;;;   Set the number of spikes in the brush.
;;; Produces:
;;;   brush, the same brush
;;; Preconditions:
;;;   spikes >= 1
;;;   brush is a mutable brush
;;; Postconditions:
;;;   (brush-get-spikes brush) is spikes
(define _brush-set-spikes
  (lambda (brush spikes)
    (cond
      [(not (brush-valid-spikes? spikes))
       (error error/parameter-type 'brush-set-spikes! 2 brush-valid-spikes 
              (list brush spikes))]
      [(brush-name? brush)
       (let ([editable (brush-make-editable brush)])
         (gimp-brush-set-spikes brush spikes)
         editable)]
      [(brush-info? brush)
       (vector-set! brush 4 spikes)]
      [else
       (error/parameter-type 'brush-set-spikes! 1 'brush 
                             (list brush spikes))])))
