#lang racket

; experiments/drawable-fill.rkt
;   An experiment in how drawable-fill! works.  Assumes that
;   (1)tThe Gimp DBus server is running ; (2) image 1 with drawaable 
;   2 is open

(require LoudGimp/tile)
(require LoudGimp/irgb)

(drawable-fill! 1 2 (irgb-new 192 0 0))
(drawable-fill! "hello" 2 12321)
(drawable-fill! 1 "goodbye" 33333)
