#lang racket

; experiments/image-transform.rkt
;   An experiment in how image-transform! works.  Assumes that
;   (1)tThe Gimp DBus server is running ; (2) image 1 is open.

(require gigls/simplify)

(define rotate-colors
  (lambda (color) (irgb-new (irgb-blue color)
                            (irgb-red color)
                            (irgb-green color))))

(image-transform! 1 rotate-colors)
(image-transform! 23 rotate-colors)

