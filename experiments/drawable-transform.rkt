#lang racket

; experiments/drawable-transform.rkt
;   An experiment in how drawable-transform! works.  Assumes that
;   (1)tThe Gimp DBus server is running ; (2) image 1 with drawaable 
;   2 is open

(require LoudGimp/tile)
(require LoudGimp/irgb)

(_drawable-transform! 1 2 (lambda (color) (irgb-new (irgb-blue color)
                                                    (irgb-red color)
                                                    (irgb-green color))))
