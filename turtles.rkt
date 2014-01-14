#lang racket

(require gigls/pdb-dbus)
(provide (all-defined-out))
(require gigls/rgb-core
         gigls/brushes
         gigls/colors
         gigls/context
         gigls/current-brush
         gigls/guard
         gigls/image
         gigls/point
         gigls/utils)

;;; Procedure:
;;;   turtle-new
;;; Parameters:
;;;   image, the id of an image
;;; Purpose:
;;;   Create a new turtle that 'lives' on image.
;;; Produces:
;;;   newturtle, a turtle.
;;; Preconditions:
;;;   image is a valid image id.
;;; Postconditions:
;;;   newturtle can be used for the various turtle operations (at least
;;;   until the image disappears).
;;; Phoo:
;;;   The body of the object should probably be generated automatically.

(define _turtle-new
  (lambda (image)
    (let ((world image)
          (col 0) 
          (row 0)
          (angle 0)
          (pen? #t)
          (color (rgb-new 0 0 0))
          (brush "2. Hardness 100")
          (brush-size 3)
          (visible? #f))
      (lambda (message . params)
        (cond
          ; Accessors
          ((eq? message ':type) 
           'turtle)
          ((eq? message ':string)
           "<turtle>")
          ((eq? message ':world)
           world)
          ((eq? message ':col)
           col)
          ((eq? message ':row)
           row)
          ((eq? message ':angle)
           angle)
          ((eq? message ':pen?)
           pen?)
          ((eq? message ':color)
           color)
          ((eq? message ':brush)
           brush)
          ((eq? message ':brush-size)
           brush-size)
          ((eq? message ':visible?)
           visible?)
          ; Mutators
          ((or (eq? message ':set-world) (eq? message ':set-world!))
           (set! world (car params)))
          ((eq? message ':set-col!)
           (set! col (car params)))
          ((eq? message ':set-row!)
           (set! row (car params)))
          ((eq? message ':set-angle!)
           (set! angle (car params)))
          ((eq? message ':set-color!)
           (set! color (car params)))
          ((eq? message ':set-brush!)
           (set! brush (car params)))
          ((eq? message ':set-brush-size!)
           (set! brush-size (car params)))
          ((eq? message ':up!)
           (set! pen? #f))
          ((eq? message ':down!)
           (set! pen? #t))
          ((eq? message ':show!)
           (set! visible? #t))
          ((eq? message ':hide!)
           (set! visible? #f))
          (else
           (error (string-append "<turtle>: invalid message \""
                                 (value->string message)
                                 "\""
                                 "\n  in "
                                 (value->string
                                  (cons "<turtle>"
                                        (cons message
                                              params)))))))))))

(define turtle-new
  (guard-unary-proc 'turtle-new
                    _turtle-new
                    'image
                    image?))


;;; Procedure:
;;;   turtle?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val can be interepreted as a turtle.
;;; Produces:
;;;   turtle-like?, a Boolean
;;; Ponderings:
;;;   Since we represent turtles as procedures, it is difficult to tell
;;;   if a particular procedure is really a turtle.  Hence, we use a bit
;;;   of a hack: We rely on PLT Scheme's rendering of anonymous procedures
;;;   as strings.  Two anonymous procedures created by the same procedure
;;;   will have the same string.  Two anonymous procedures created by
;;;   different procedures will have different strings.
(define _turtle?
  (let ((sample (value->string (_turtle-new 0))))
    (lambda (val)
      (and (procedure? val)
           (string=? (value->string val) sample)))))

(define turtle? _turtle?)

;;; Procedure:
;;;   turtle-clone
;;; Parameters:
;;;   turtle, a turtle
;;; Purpose:
;;;   Make a clone of turtle. 
;;; Produces:
;;;   clone, a turtle
;;; Postconditions:
;;;   clone is at the same position as turtle
;;;   clone is oriented in the same direction as turtle
;;;   clone has the same color and brush as turtle
;;;   clone is on the same image as turtle
(define _turtle-clone
  (lambda (turtle)
    (let ((clone (turtle-new (turtle ':world))))
      (clone ':set-col! (turtle ':col))
      (clone ':set-row! (turtle ':row))
      (clone ':set-angle! (turtle ':angle))
      (clone ':set-color! (turtle ':color))
      (clone ':set-brush! (turtle ':brush))
      (clone ':set-brush-size! (turtle ':brush-size))
      (if (turtle ':visible?)
          (clone ':show!)
          (clone ':hide!))
      clone)))

(define turtle-clone
  (guard-unary-proc 'turtle-clone
                    _turtle-clone
                    'turtle
                    turtle?))

;;; Procedure:
;;;   turtle-angle
;;; Parameters:
;;;   turtle, a turtle
;;; Purpose:
;;;   Determine the angle in which the turtle is facing.
;;; Produces:
;;;   angle, a real number.
(define _turtle-angle
  (lambda (turtle)
    (turtle ':angle)))

(define turtle-angle
  (guard-unary-proc 'turtle-angle _turtle-angle 'turtle turtle?))

;;; Procedure:
;;;   turtle-col
;;; Parameters:
;;;   turtle, a turtle
;;; Purpose:
;;;   Determine the column on which the turtle resides
;;; Produces:
;;;   col, a real number
(define _turtle-col
  (lambda (turtle)
    (turtle ':col)))

(define turtle-col
  (guard-unary-proc 'turtle-col _turtle-col 'turtle turtle?))

;;; Procedure:
;;;   turtle-display
;;; Parameters:
;;;   turtle, a turtle (created by turtle-new or turtle-clone)
;;;   type, a type of arrow (optional; pointy by default)
;;; Purpose:
;;;   Diplsyss the turtle on the screen
;;; Produces:
;;;   turtle, the same turtle
(define _turtle-display
  (let ((d2r (/ pi 180)))
    (lambda (turtle . rest)
      (let ((col (turtle ':col))
            (row (turtle ':row))
            (angle (turtle ':angle))
            (type (if (null? rest) 'pointy (car rest))))
        (let ((back-col (- col (cos (* d2r angle))))
              (back-row (- row (sin (* d2r angle)))))
          (let ((saved-color (context-get-fgcolor))
                (saved-brush (context-get-brush))
                (turtle-color (turtle ':color)))
            (when (not (equal? turtle-color saved-color))
              (context-set-fgcolor! turtle-color))
            (context-set-brush! "2. Hardness 100" 1)
            (image-draw-arrow! (turtle ':world)
                               type
                               back-col back-row
                               col row
                               15 10)
            (when (context-preserve?)
              (when (not (equal? turtle-color saved-color))
                (context-set-fgcolor! saved-color))
              (context-set-brush! saved-brush)))))
      turtle)))

(define turtle-display
  (lambda (turtle . rest)
    (cond
      ((not (turtle? turtle))
       (error/parameter-type 'turtle-display 1 'turtle (cons turtle rest)))
      ((and (not (null? rest)) (not (null? (cdr rest))))
       (error/arity 'turtle-display "one or two" (cons turtle rest)))
      ((and (not (null? rest)) (not (arrow-type? (car rest))))
       (error/parameter-type 'turtle-display 2 'arrow-type (cons turtle rest)))
      (else (apply _turtle-display (cons turtle rest))))))

;;; Procedure:
;;;   turtle-direction
;;; Parameters:
;;;   turtle, a turtle
;;; Purpose:
;;;   Determine the direction in which the turtle is facint
;;; Produces:
;;;   direction, a real
(define _turtle-direction
  (lambda (turtle)
    (turtle ':angle)))

(define turtle-direction
  (guard-unary-proc 'turtle-direction _turtle-direction 'turtle turtle?))

;;; Procedure:
;;;   turtle-down!
;;; Parameters:
;;;   turtle, a turtle
;;; Purpose:
;;;   Puts the turtle's brush down
;;; Produces:
;;;   turtle, the same turtle
(define _turtle-down!
  (lambda (turtle)
    (turtle ':down!)
    turtle))

(define turtle-down!
  (guard-unary-proc 'turtle-down!
                    _turtle-down!
                    'turtle
                    turtle?))

;;; Procedure:
;;;   turtle-face!
;;; Parameters:
;;;   turtle, a turtle
;;;   angle, an integer
;;; Purpose:
;;;   Make the turtle face in a particular direction.
;;; Produces:
;;;   [Nothing, called for the side effect.]
;;; Postconditions:
;;;   The turtle is now facing in the direction specified by angle
;;;   (clockwise from right).
(define _turtle-face!
  (lambda (turtle angle)
    (turtle ':set-angle! angle)
    (when (turtle ':visible?)
      (turtle-display turtle))
    turtle))

(define turtle-face!
  (guard-proc 'turtle-face!
              _turtle-face!
              (list 'turtle 'integer)
              (list turtle? integer?)))

;;; Procedure:
;;;   turtle-forward!
;;; Parameters:
;;;   turtle, a turtle (created by turtle-new or turtle-clone)
;;;   distance, a real number
;;; Purpose:
;;;   Moves the turtle forward by the given distance.
;;; Produces:
;;;   turtle, the same turtle
(define _turtle-forward!
  (let ((d2r (/ pi 180)))
    (lambda (turtle distance)
      (let ((col (turtle ':col))
            (row (turtle ':row))
            (angle (turtle ':angle)))
        (let ((newcol (+ col (* distance (cos (* d2r angle)))))
              (newrow (+ row (* distance (sin (* d2r angle))))))
          (when (turtle ':pen?)
            (let ((saved-brush (context-get-brush))
                  (saved-color (context-get-fgcolor))
                  (turtle-brush (turtle ':brush))
                  (turtle-brush-size (turtle ':brush-size))
                  (turtle-color (turtle ':color)))
              (if (brush-generated? turtle-brush)
                  (context-set-brush! turtle-brush turtle-brush-size)
                  (when (not (equal? saved-brush turtle-brush))
                    (context-set-brush! turtle-brush)))
              (when (not (equal? turtle-color saved-color))
                (context-set-fgcolor! turtle-color))
              (image-draw-line! (turtle ':world)
                                col row
                                newcol newrow)
              (when (context-preserve?) 
                (context-set-brush! saved-brush))
              (when (and (context-preserve?) 
                         (not (equal? turtle-color saved-color)))
                (context-set-fgcolor! saved-color))))
          (turtle ':set-col! newcol)
          (turtle ':set-row! newrow)
          (when (turtle ':visible?)
            (turtle-display turtle))))
      turtle)))

(define turtle-forward!
  (guard-proc 'turtle-forward!
              _turtle-forward!
              (list 'turtle 'real)
              (list turtle? real?)))

;;; Procedure:
;;;   turtle-hide!
;;; Parameters:
;;;   turtle, a turtle
;;; Purpose:
;;;   Stop showing the turtle.
;;; Produces:
;;;   turtle, the now hidden turtle
;;; Preconditions:
;;;   [No additional.]
;;; Postconditions:
;;;   The turtle is no longer shown.
(define _turtle-hide!
  (lambda (turtle)
    (turtle ':hide!)
    turtle))

(define turtle-hide!
  (guard-unary-proc 'turtle-hide!  _turtle-hide!  'turtle turtle?))

;;; Procedure:
;;;   turtle-point
;;; Parameters:
;;;   turtle, a turtle
;;; Purpose:
;;;   Get the point at which the turtle resides.
;;; Produces:
;;;   pt, a point
(define _turtle-point
  (lambda (turtle)
    (point (turtle ':col) (turtle ':row))))

(define turtle-point
  (guard-unary-proc 'turtle_point _turtle-point 'turtle turtle?))

;;; Procedure:
;;;   turtle-row
;;; Parameters:
;;;   turtle, a turtle
;;; Purpose:
;;;   Determine the row on which the turtle resides
;;; Produces:
;;;   row, a real number
(define _turtle-row
  (lambda (turtle)
    (turtle ':row)))

(define turtle-row
  (guard-unary-proc 'turtle-row _turtle-row 'turtle turtle?))

;;; Procedure:
;;;   turtle-set-brush!
;;; Parameters:
;;;   turtle, a turtle
;;;   brush, a string that names a brush
;;; Purpose:
;;;   Set the brush with which the turtle draws.
;;; Produces:
;;;   turtle, the same turtle
(define _turtle-set-brush! 
  (lambda (turtle brush . rest)
    (turtle ':set-brush! brush)
    (when (not (null? rest))
      (turtle ':set-brush-size! (car rest)))))

(define turtle-set-brush!
  (lambda (turtle brush . rest)
    (let ([params (cons turtle (cons brush rest))])
      (cond 
        [(not (turtle? turtle))
         (error/parameter-type 'turtle-set-brush! 1 'turtle params)]
        [(not (brush-name? brush))
         (error/parameter-type 'turtle-set-brush! 2 'brush params)]
        [(and (not (brush-generated? brush)) (not (null? rest)))
         (error "turtle-set-brush! cannot set size for a non-mutable brush")]
        [(and (not (null? rest))
              (or (not (real? (car rest)))
                  (not (integer? (car rest)))))
         (error/parameter-type 'turtle-set-brush! 3 'positive-real params)]
        [else
         (apply _turtle-set-brush! params)]))))

;;; Procedure:
;;;   turtle-set-color!
;;; Parameters:
;;;   turtle, a turtle
;;;   color, an rgb color
;;; Purpose:
;;;   Set the color with which the turtle draws.
;;; Produces:
;;;   turtle, the same turtle
(define _turtle-set-color!
  (lambda (turtle color)
    (turtle ':set-color! (color->rgb color))
    turtle))

(define turtle-set-color!
  (guard-proc 'turtle-set-color!
              _turtle-set-color!
              (list 'turtle 'color)
              (list turtle? color?)))


; Produces:
;   turtle, the original turtle
; Ponderings:
;   Probably not very useful, but included just in case I find
;   a reason for it.
(define _turtle-set-image!
  (lambda (turtle image)
    (turtle ':set-world! image)
    turtle))

(define turtle-set-image!
  (guard-proc 'turtle-set-image!
              _turtle-set-image!
              (list 'turtle 'image)
              (list turtle? image?)))

;;; Procedure:
;;;   turtle-show!
;;; Parameters:
;;;   turtle, a turtle
;;; Purpose:
;;;   Start showing the turtle.
;;; Produces:
;;;   turtle, the now visible turtle
;;; Preconditions:
;;;   [No additional.]
;;; Postconditions:
;;;   The turtle is shown after each call to forward, turn, teleport, and
;;;   such.
(define _turtle-show!
  (lambda (turtle)
    (turtle ':show!)
    (turtle-display turtle)
    turtle))

(define turtle-show!
  (guard-unary-proc 'turtle-show!  _turtle-show!  'turtle turtle?))

;;; Procedure:
;;;   turtle-teleport!
;;; Parameters:
;;;   turtle, a turtle
;;;   col, a real
;;;   row, a real
;;; Purpose:
;;;   Move the turtle to (col,row).
;;; Produces:
;;;   turtle, the same turtle
(define _turtle-teleport!
  (lambda (turtle col row)
    (turtle ':set-col! col)
    (turtle ':set-row! row)
    (when (turtle ':visible?)
      (turtle-display turtle))
    turtle))

(define turtle-teleport!
  (guard-proc 'turtle-teleport!
              _turtle-teleport!
              (list 'turtle 'real 'real)
              (list turtle? real? real?)))


;;; Procedure:
;;;   turtle-turn!
;;; Parameters:
;;;   turtle, a turtle (created by turtle-new or turtle-clone)
;;;   angle, a real number
;;; Purpose:
;;;   Rotate turtle clockwise by angle (expressed in degrees)
;;; Produces:
;;;   turtle, the same turtle
(define _turtle-turn!
  (letrec ((fixangle 
            (lambda (angle)
              (if (>= angle 360) (fixangle (- angle 360))
                  (if (< angle 0) (fixangle (+ angle 360))
                      angle)))))   
    (lambda (turtle angle)
      (turtle ':set-angle! 
              (fixangle (+ (turtle ':angle) angle)))
      (when (turtle ':visible?)
        (turtle-display turtle))
      turtle)))

(define turtle-turn!
  (guard-proc 'turtle-turn!
              _turtle-turn!
              (list 'turtle 'real)
              (list turtle? real?)))

;;; Procedure:
;;;   turtle-up!
;;; Parameters:
;;;   turtle, a turtle
;;; Purpose:
;;;   Puts the turtle's brush up
;;; Produces:
;;;   turtle, the same turtle
(define _turtle-up!
  (lambda (turtle)
    (turtle ':up!)
    turtle))

(define turtle-up!
  (guard-unary-proc 'turtle-up!
                    _turtle-up!
                    'turtle
                    turtle?))

;;; Procedure:
;;;   turtle-world
;;; Parameters:
;;;   turtle, a turtle
;;; Purpose:
;;;   Determine the world on which turtle resides
;;; Produces:
;;;   world, an image id
(define _turtle-world
  (lambda (turtle)
    (turtle ':world)))

(define turtle-world
  (guard-unary-proc 'turtle-world
                    _turtle-world
                    'turtle
                    turtle?))

;;; Procedure:
;;;   turtle-x
;;; Parameters:
;;;   turtle, a turtle
;;; Purpose:
;;;   Get the x coordinate of the turtle
;;; Produces:
;;;   x, a real number
(define _turtle-x
  (lambda (turtle)
    (turtle ':col)))

(define turtle-x
  (guard-unary-proc 'turtle-x _turtle-x 'turtle turtle?))
   
;;; Procedure:
;;;   turtle-y
;;; Parameters:
;;;   turtle, a turtle
;;; Purpose:
;;;   Get the y coordinate of the turtle
;;; Produces:
;;;   y, a real number
(define _turtle-y
  (lambda (turtle)
    (turtle ':row)))

(define turtle-y
  (guard-unary-proc 'turtle-y _turtle-y 'turtle turtle?))
