#lang racket

;;; gigls/experiments/drawable
;;;   A quick experiment with drawables.

(require gigls/drawable)

(define expt
  (lambda (val)
    (display (list 'drawable? val))
    (display ": ")
    (display (drawable? val))
    (newline)))

(expt 1)
(expt 2)
(expt -3)
(expt 300)
(expt "Hello")
