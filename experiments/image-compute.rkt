#lang racket

(require gigls/tile)
(require gigls/irgb)
(require gigls/image)

(image-show (image-compute! (lambda (x y) (irgb-new x 0 y)) 300 300))
