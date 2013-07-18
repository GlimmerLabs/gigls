#lang racket
(provide (all-defined-out))

(define rgb-new
  (lambda (r g b)
    (+ (* r 256 256) (* g 256) b)))

(define rgb-blue
  (lambda (color)
    (remainder color 256)))

(define rgb-green
  (lambda (color)
    (remainder (quotient color 256) 256)))

(define rgb-red
  (let ((scale (* 256 256)))
    (lambda (color)
      (quotient color scale))))