#lang racket

(provide (all-defined-out))

; Sam's quick hacks to get RGB working.  This needs to be rewritten in C.

(define rgb? integer?)

(define rgb-new
  (lambda (r g b)
    (+ (* r 256 256) (* g 256) b)))

(define rgb-red
  (let ((scale (* 256 256)))
    (lambda (color)
      (quotient color scale))))

(define rgb-green
  (lambda (color)
    (remainder (quotient color 256) 256)))

(define rgb-blue
  (lambda (color)
    (remainder color 256)))