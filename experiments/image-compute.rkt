#lang racket

(require LoudGimp/tile)
(require LoudGimp/irgb)
(require LoudGimp/image)

(image-show (image-compute! (lambda (x y) (irgb-new x 0 y)) 300 300))
