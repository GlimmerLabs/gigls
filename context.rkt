#lang racket
(provide (all-defined-out))
(require gigls/pdb-dbus)
(require louDBus/unsafe)
(require gigls/colors
         gigls/guard
         gigls/hacks
         gigls/list
         gigls/makers
         gigls/mgimp
         gigls/utils)

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
(define brush?
  (lambda (val)
    (and (string? val)
         (member val (context-list-brushes val))
         #t)))

;;; Procedure:
;;;   context-get-bgcolor
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Gets the current background color (as an RGB color).
;;; Produces:
;;;   color, an RGB color.
(define context-get-bgcolor
  (lambda ()
    (color->rgb (car (gimp-context-get-background)))))

; [From gimp/context/context-get-fgcolor.scm]

;;; Procedure:
;;;   context-get-fgcolor
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Gets the current foreground color (as an RGB color).
;;; Produces:
;;;   color, an RGB color.
(define context-get-fgcolor
  (lambda ()
    (color->rgb (car (gimp-context-get-foreground)))))

; [From gimp/context/context-get-brush.scm]

;;; Procedure:
;;;   context-get-brush
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Gets the current brush.
;;; Produces:
;;;   brush, a string
;;; Postconditions:
;;;   (brush? brush)
(define context-get-brush
  (lambda ()
    (car (gimp-context-get-brush))))


; +-------------------------------------------+---------------------------------
; | Setting Basic GIMP Contextual Information |
; +-------------------------------------------+

; [From gimp/context/context-set-bgcolor.scm]

;;; Procedure:
;;;   context-set-bgcolor!
;;; Parameters:
;;;   color, a color
;;; Purpose:
;;;   Set the background color.
;;; Produces:
;;;   [Nothing; called for the side effect]
;;; Preconditions:
;;;   color is a one of the valid forms of color.
;;; Postconditions:
;;;   The background color is now the specified color.
(define _context-set-bgcolor!
  (lambda (color)
    (gimp-context-set-background  color)))

(define context-set-bgcolor!
  (guard-unary-proc 'context-set-bgcolor!
                    _context-set-bgcolor!
                    'color
                    color?))

; [From gimp/context/context-set-fgcolor.scm]

;;; Procedure:
;;;   context-set-fgcolor!
;;; Parameters:
;;;   color, a color
;;; Purpose:
;;;   Set the foreground color.
;;; Produces:
;;;   Nothing.
;;; Preconditions:
;;;   color is one of the valid forms of color.
;;; Postconditions:
;;;   The foreground color is now the specified color.
(define _context-set-fgcolor!
  (lambda (color)
    (process-gimp-result
     (gimp-context-set-foreground color))))

(define context-set-fgcolor!
  (guard-unary-proc 'context-set-fgcolor!
                    _context-set-fgcolor!
                    'color
                    color?))

; [From gimp/context/context-set-brush.scm]

;;; Procedure
;;;   context-set-brush!
;;; Parameters
;;;   brush, a string
;;; Purpose
;;;   Change the gimp's currently selected brush
;;; Produces
;;;   brush, the provided brush
;;; Preconditions
;;;   (brush? brush)
;;; Postconditions
;;;   GIMP's current brush is now set to the given brush
(define _context-set-brush!
  (lambda (brush)
    (gimp-context-set-brush brush)
    brush))

(define context-set-brush!
  (lambda (brush)
    (cond
      ((not (string? brush))
       (error "context-set-brush!: expects a string as a parameter, received"
              brush))
      ((not (brush? brush))
       (error "context-set-brush!: invalid brush:" brush))
      (else 
       (_context-set-brush! brush)))))


; [From gimp/context/context-list-brushes.scm]

;;; Procedure:
;;;  context-list-brushes
;;; Parameters:
;;;   pattern, a string [optional]
;;; Purpose:
;;;   Get a vector of usable brushes.
;;; Produces:
;;;   brushes, a list of strings
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If no parameters are passed, brushes is a list of all brushes in 
;;;     the GIMP.
;;;   Otherwise, brushes is a list of all brushes in the GIMP whose name
;;;     contains the pattern.
;;;   For each reasonable i,
;;;     (brush? (list-ref brushes i))
(define _context-list-brushes
  (lambda restriction
    (let* ((pattern (if (null? restriction) "" (car restriction)))
           (brushes (cadr (gimp-brushes-get-list (string-escape pattern)))))
      (if (vector? brushes) 
          (vector->list brushes) 
          brushes))))

(define context-list-brushes
  (lambda restriction
    (let ((pattern (if (null? restriction) "" (car restriction))))
      (cond
        ((not (string? pattern))
         (error "context-list-brushes: Invalid restriction: " pattern))
        (else
         (apply _context-list-brushes restriction))))))



;;; Procedure:
;;;   context-list-color-names
;;; Parmeters:
;;;  none
;;; Purpose:
;;;   List all the colors contained in the GIMP
;;; Produces:
;;;   colors, a list of strings
;;; Preconditions:
;;;   [Standard]
;;; Postconditions:
;;;   All of the colors recognized by the GIMP
;;; NOTE:
;;;   Calls a function implimented as a GIMP plugin
(define context-list-color-names
  (lambda ()
    (cadr (loudbus-call gimp 'ggimp_rgb_list))))

; [From gimp/context/context-list-fonts.scm]

;;; Procedure:
;;;   context-list-fonts
;;; Parameters:
;;;   pattern, a string [optional]
;;; Purpose:
;;;   List all of the fonts (if no parameter is given) or get all of
;;;   the fonts whose name contains pattern.
;;; Produces:
;;;   font-list, a list of strings
(define _context-list-fonts
  (lambda restriction
    (cond
      ((null? restriction)
       (vector->list (cadr (gimp-fonts-get-list ""))))
      (else
       (vector->list (cadr (gimp-fonts-get-list (car restriction))))))))

(define context-list-fonts
  (lambda restriction
    (cond
      ((null? restriction)
       (_context-list-fonts))
      ((not (string? (car restriction)))
       (error "context-get-fonts: Pattern must be a string"))
      ((not (null? (cdr restriction)))
       (error "context-get-fonts: Only one pattern accepted"))
      (else
       (_context-list-fonts (car restriction))))))


; +------------------------------------------+----------------------------------
; | Miscellaneous Context-Related Procedures |
; +------------------------------------------+

; [From gimp/context/context-immediate-updates.scm]

;;; Procedure:
;;;   context-immediate-updates
;;; Parameters:
;;;   immediate?, an optional boolean
;;; Purpose:
;;;   Set or get a flag that specifies whether or not some operations
;;;   are requested to immediate-updates the context.
;;; Produces:
;;;   immediate-updates?, a Boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If called with no parameters, returns the current state of the
;;;     flag
;;;   If called with one parameter, sets the current state of the flag
;;;     to that parameter.
(define _context-immediate-updates
  (make-flag))

(define context-immediate-updates
  (guard-flag 'context-immediate-updates _context-immediate-updates))

; [From gimp/context/context-immediate-updates-p.scm]

;;; Procedure:
;;;   context-immediate-updates?
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Determine whether or not some operations should immediately
;;;   update the displays
;;; Produces:
;;;   immediate-updates?, a Boolean
;;; Preconditions:
;;;   [No additional]
(define _context-immediate-updates? _context-immediate-updates)

(define context-immediate-updates?
  (guard-proc 'context-immediate-updates?
              _context-immediate-updates?
              null
              null))

; [From gimp/context/context-immediate-updates-on.scm]

;;; Procedure:
;;;   context-immediate-updates-on!
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Indicate that varous operations should immediately update the
;;;   displays.
;;; Produces:
;;;   [Nothing; called for the side effect.]
(define _context-immediate-updates-on!
  (lambda ()
    (context-update-displays!)
    (context-immediate-updates #t)))

(define context-immediate-updates-on! _context-immediate-updates-on!)

; [From gimp/context/context-immediate-updates-off.scm]

;;; Procedure:
;;;   context-immediate-updates-off!
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Indicate that varous operations should not immediately update the
;;;   displays.  In most cases, this means that updates to an image will
;;;   not be visible until one calls (context-update-displays!)
;;; Produces:
;;;   [Nothing; called for the side effect.]
(define _context-immediate-updates-off!
  (lambda ()
    (context-immediate-updates #f)))

(define context-immediate-updates-off! _context-immediate-updates-off!)

; [From gimp/context/context-update-displays.scm]

;;; Procedure:
;;;   context-update-displays!
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Flush recent gimp image operations to the graphical user interface
;;;   (give gimp-displays-flush a 'nicer' name for students).
;;; Produces
;;;   [Nothing; called for the side effect]
;;; Preconditions
;;;   [None]
;;; Postconditions
;;;   All completed image operations should be visible.
(define context-update-displays! gimp-displays-flush)


; +------------------------------------------+----------------------------------
; | Miscellaneous Context-Related Procedures |
; +------------------------------------------+

; [From gimp/context/context-preserve.scm]

;;; Procedure:
;;;   context-preserve
;;; Parameters:
;;;   preserve?, an optional boolean
;;; Purpose:
;;;   Set or get a flag that specifies whether or not some operations
;;;   are requested to preserve the context.
;;; Produces:
;;;   preserved?, a Boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If called with no parameters, returns the current state of the
;;;     flag
;;;   If called with one parameter, sets the current state of the flag
;;;     to that parameter.
(define _context-preserve
  (make-flag))

(define context-preserve
  (guard-flag 'context-preserve _context-preserve))

; [From gimp/context/context-preserve-p.scm]

;;; Procedure:
;;;   context-preserve?
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Determine whether or not some operations are requested to preserve 
;;;   the context.
;;; Produces:
;;;   preserved?, a Boolean
;;; Preconditions:
;;;   [No additional]
(define _context-preserve? _context-preserve)

(define context-preserve?
  (guard-proc 'context-preserve?
              _context-preserve?
              null
              null))

; [From gimp/context/context-preserve-on.scm]

;;; Procedure:
;;;   context-preserve-on!
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Indicate that varous operations should preserve the color and brush.
;;; Produces:
;;;   [Nothing; called for the side effect.]
(define _context-preserve-on!
  (lambda ()
    (context-preserve #t)))

(define context-preserve-on! _context-preserve-on!)

; [From gimp/context/context-preserve-off.scm]

;;; Procedure:
;;;   context-preserve-off!
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Indicate that varous operations need not preserve the brush and
;;;   color.
;;; Produces:
;;;   [Nothing; called for the side effect.]
(define _context-preserve-off!
  (lambda ()
    (context-preserve #f)))

(define context-preserve-off! _context-preserve-off!)

; [From gimp/context/context-select-random-brush.scm]

;;; Procedure
;;;   context-select-random-brush!
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Select one of the brushes.
;;; Produces:
;;;   [Nothing; called for the side effect]
;;; Postconditions:
;;;   It is difficult to predict the brush.
(define context-select-random-brush!
  (lambda ()
    (context-set-brush! (list-random-element (context-list-brushes)))))
