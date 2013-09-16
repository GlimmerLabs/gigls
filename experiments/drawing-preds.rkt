#lang racket

;;; gigls/experiments/drawing-preds.rkt
;;;   A quick experiment with the drawing predicates.

(require gigls/drawings)

(define expt-part
  (lambda (name pred? val)
    (display (list name val))
    (display ": ")
    (display (pred? val))
    (newline)))

(define expt
  (lambda (val)
    (expt-part 'drawing? drawing? val)
    (expt-part 'drawing-ellipse? drawing-ellipse? val)
    (expt-part 'drawing-rectangle? drawing-rectangle? val)
    (expt-part 'drawing-group? drawing-group? val)))

(expt 1)
(expt drawing-unit-circle)
(expt drawing-unit-square)
(expt (drawing-group drawing-unit-circle drawing-unit-square))
