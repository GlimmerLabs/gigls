#lang racket

; experiments/drawable-recompute.rkt
;   An experiment in how drawable-recompute! works.  Assumes that
;   (1)tThe Gimp DBus server is running ; (2) image 1 with drawable 
;   2 is open

(require LoudGimp/tile)
(require LoudGimp/irgb)

(_drawable-recompute! 1 2 (lambda (x y) (irgb-new x 0 y)))
