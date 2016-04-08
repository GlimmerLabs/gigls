#lang racket

; gigls/current-brush.rkt
;   A collection of procedures for dealing with the current brush.
;   (These procedures could go in gigls/context.rkt, but that file
;   was getting long, and there are a lot of these.)
; 
;   Copyright (c) 2013 Samuel A. Rebelsky.  All rights reserved.
;
;   This file is part of gigls - the Glimmer Improved Gimp Library for
;   Scripting 
;
;   gigls is free software: you can redistribute it and/or modify it under the
;   terms of the GNU Lesser General Public License as published by the Free
;   Software Foundation, either version 3 of the License, or (at your option)
;   any later version.
;
;   gigls is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU Lesser General Public 
;   License along with giglsIf not, see <http://www.gnu.org/licenses/>.

(provide (all-defined-out))
(require gigls/brushes
         gigls/guard
         gigls/higher
         gigls/list
         gigls/makers
         gigls/pdb-dbus)

; +--------+----------------------------------------------------------
; | Values |
; +--------+

;;; Value:
;;;   context-current-brush
;;; Type:
;;;   brush-info
;;; Summary:
;;;   Information on the current brush
(define context-current-brush (brush-info (car (gimp-context-get-brush))))

; +-----------+-------------------------------------------------------
; | Observers |
; +-----------+

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
;;;   The commands that use a brush (e.g., image-draw-line!) will use brush.
(define context-get-brush
  (lambda ()
    (car (gimp-context-get-brush))))

;;; Procedure:
;;;   context-get-brush-name
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Gets the name of the current brush
;;; Produces:
;;;   brush, a string
;;; Postconditions:
;;;   (brush-name? brush)
;;;   The commands that use a brush (e.g., image-draw-line!) will use brush.
(define context-get-brush-name
  (lambda ()
    (car (gimp-context-get-brush))))

;;; Procedure:
;;;   context-get-brush-info
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Get information on the current brush
;;; Produces:
;;;   brushinfo, a vector of info on the current brush
;;;     Element 0: The symbol 'brush
;;;     Element 1: The name of the brush
;;;     Element 2: The shape of the brush (as an integer)
;;;     Element 3: The radius of the brush
;;;     Element 4: The number of spikes
;;;     Element 5: The hardness of the brush
;;;     Element 6: The aspect ratio of the brush
;;;     Element 7: The angle of the brush
;;;     Element 8: The spacing of the brush
(define context-get-brush-info
  (lambda ()
    context-current-brush))

;;; Procedure:
;;;   context-get-brush-radius
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Get the radius of the current brush
;;; Produces:
;;;   radius, a real
(define _context-get-brush-radius
  (lambda ()
    (if (_context-get-brush-info-locally)
        (brush-get-radius context-current-brush)
        (car (gimp-brush-get-radius (context-get-brush))))))

(define context-get-brush-radius _context-get-brush-radius)


; +----------------+--------------------------------------------------
; | Brush Mutators |
; +----------------+

;;; Procedure:
;;;   context-make-current-brush-editable!
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Ensure that the current brush is editable.
;;; Produces:
;;;   editable-brush, the name of the editable brush
;;; Preconditions:
;;;   The current brush is mutable.
;;; Postconditions:
;;;   editable-brush is the current brush.
;;;   editable-brush is editable.
;;;   editable-brush is a variant of the current brush.
;;; Problems:
;;;   If the current brush is not editable and an editable
;;;   version of the brush exists, should we make sure
;;;   all of its parameters match?
(define _context-make-current-brush-editable!
  (lambda ()
    (let* ([brush (context-get-brush)]
           [radius (brush-get-radius brush)]
           [editable (brush-make-editable brush)])
      (when (not (equal? editable brush))
        (vector-set! context-current-brush 1 editable)
        (context-set-brush! editable radius))
      editable)))

(define context-make-current-brush-editable!
  (lambda ()
    (context-verify-current-brush-mutable! 'context-make-current-brush-editable)
    (_context-make-current-brush-editable!)))

;;; Procedure:
;;;   context-refresh-current-brush!
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Resets the current brush to whatever it can get from the GIMP.
;;; Produces:
;;;   brushinfo, info on the current brush
(define context-refresh-current-brush!
  (lambda ()
    (let* ([name (car (gimp-context-get-brush))]
           [brush context-current-brush]
           [info (brush-info name)]
           [set-field! (lambda (i) (vector-set! brush i (vector-ref info i)))])
      (for-each set-field! '(1 2 3 4 5 6 7))
      brush)))

; +-----------------------+-------------------------------------------
; | Set Brush Information |
; +-----------------------+

;;; Procedure
;;;   context-set-brush!
;;; Parameters
;;;   brush, a string
;;;   sizea positive real (optional)
;;; Purpose
;;;   Change the gimp's currently selected brush
;;; Produces
;;;   brush, the provided brush
;;; Preconditions
;;;   (brush? brush)
;;; Postconditions
;;;   GIMP's current brush is now set to the given brush
(define/contract context-set-brush!
  (->* (string?) ((and/c real? positive?)) brush?)
  (lambda (brush . rest)
    (let ([radius (if (null? rest)
                      (brush-get-radius (brush-original brush))
                      (* 0.5 (car rest)))])
      (if (and (brush-mutable? brush)
               (not (equal? (brush-get-radius brush) radius)))
          (let ([editable (brush-make-editable brush)])
            (gimp-brush-set-radius editable radius)
            (vector-set! context-current-brush 1 editable)
            (gimp-context-set-brush editable)
            (context-refresh-current-brush!))
          (gimp-context-set-brush brush)))))

;(define context-set-brush!
;  (lambda (brush . rest)
;    (cond
;      [(not (string? brush))
;       (error "context-set-brush!: expects a string as a parameter, received"
;              brush)]
;      [(not (brush? brush))
;       (error "context-set-brush!: invalid brush:" brush)]
;      [(and (not (brush-generated? brush)) (not (null? rest)))
;       (error "context-set-brush!: cannot set radius for a non-mutable brush"
;              brush)]
;      [(and (not (null? rest)) 
;            (or (not (real? (car rest)))
;                (not (positive? (car rest)))))
;       (error/parameter-type 'context-set-brush! 
;                             1 
;                             'positive-real
;                             (cons brush rest))]
;      [else 
;       (apply _context-set-brush! (cons brush rest))])))

;;; Procedure:
;;;   context-set-brush-aspect-ratio!
;;; Parameters:
;;;   aspect-ratio, a real number
;;; Purpose:
;;;   Set the aspect ratio of the current brush (or an editable version
;;;   thereof)
;;; Produces:
;;;   brush, a string representing the current brush

;;; Procedure:
;;;   context-set-brush-angle!
;;; Parameters:
;;;   angle, a real number
;;; Purpose:
;;;   Set the angle of the current brush (or an editable version thereof).
;;; Produces:
;;;   brush, a string representing the current brush.
;;; Preconditions:
;;;   The current brush is 
(define/contract context-set-brush-angle!
  (-> real? brush?)
  (lambda (angle)
    (context-verify-current-brush-mutable! 'context-set-brush-angle!)
    (let ([brush (context-make-current-brush-editable!)])
      (brush-set-angle! brush angle)
      (brush-set-radius! context-current-brush (brush-get-angle brush))
      brush)))

;(define context-set-brush-angle!
;  (guard-unary-proc 'context-set-brush-angle!
;                    _context-set-brush-angle!
;                    "real number"
;                    real?))

;;; Procedure:
;;;   context-set-brush-radius!
;;; Parameters:
;;;   radius, a real number
;;; Purpose:
;;;   Set the radius of the current brush 
;;;   (or an editable version thereof).
;;; Produces:
;;;   brush, the current brush.
(define/contract context-set-brush-radius!
  (-> real? brush?)
  (lambda (radius)
    (context-verify-current-brush-mutable! 'context-set-brush-radius!)
    (let ([brush (context-make-current-brush-editable!)])
      (brush-set-radius! brush radius)
      (brush-set-radius! context-current-brush radius)
      brush)))

;(define context-set-brush-radius!
;  (guard-unary-proc 'context-set-brush-radius!
;                    _context-set-brush-radius!
;                    "positive real number"
;                    (^and real? positive?)))

;;; Procedure:
;;;   context-set-brush-size!
;;; Parameters:
;;;   size, a real number
;;; Purpose:
;;;   Set the size of the current brush (or an editable version thereof).
;;; Produces:
;;;   brush, the current brush.
(define/contract context-set-brush-size!
  (-> real? brush?)
  (lambda (size)
    (context-verify-current-brush-mutable! 'context-set-brush-size!)
    (context-set-brush-radius! (* 0.5 size))))

;(define context-set-brush-size!
;  (guard-unary-proc 'context-set-brush-size!
;                    _context-set-brush-size!
;                    'positive-real-number
;                    (^and real? positive?)))

; +---------------+---------------------------------------------------
; | Miscellaneous |
; +---------------+

;;; Procedure:
;;;   context-get-brush-info-locally
;;; Parameters:
;;;   local?, an optional boolean
;;; Purpose:
;;;   Set or get a flag that specifies whether or not the get operations
;;;   for the current brush are done locally (through cached info)
;;; Produces:
;;;   get-local?, a Boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If called with no parameters, returns the current state of the
;;;     flag
;;;   If called with one parameter, sets the current state of the flag
;;;     to that parameter.
(define _context-get-brush-info-locally
  (make-flag))

(define context-get-brush-info-locally
  (guard-flag 'context-get-brush-info-locally _context-get-brush-info-locally))

;;; Procedure:
;;;   context-verify-current-brush-mutable!
;;; Parameters
;;;   procname, a symbol
;;; Purpose:
;;;   Verify that the current brush is mutable. 
;;; Produces:
;;;   [Nothing; called for the side effect]
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If the current brush is mutable, does nothing.
;;;   If the current brush is not mutable, throws an error.
(define context-verify-current-brush-mutable!
  (lambda (procname)
    (let ((name (car (gimp-context-get-brush))))
      (when (not (brush-mutable? name))
        (error (string-append (symbol->string procname)
                              ": brush \""
                              name
                              "\" is not mutable"))))))

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
    (context-set-brush! (list-random-element (brushes-list)))))

;;; Procedure:
;;;   context-cleanup-brushes!
;;; Parameters:
;;;   [none, called for the side effects]
;;; Purpose:
;;;   Remove all of the editable copies of brushes.
;;; Produces:
;;;   [Nothing; Called for the side effect]
;;; Postconditions:
;;;   No more brushes with the name "(editable)" exist.
(define context-cleanup-brushes!
  (lambda () 
    (for-each (lambda (brush) (gimp-brush-delete brush))
              (brushes-list "(editable)"))))
