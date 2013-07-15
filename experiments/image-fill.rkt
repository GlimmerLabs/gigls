#lang racket

; experiments/image-fill.rkt
;   An experiment in how image-fill works.  Assumes that
;     * The Gimp DBus server is running
;     * image 1 with drawaable 2 is open

(require LoudGimp/tile)
(require LoudGimp/rgb)

(image-fill! 1 2 (rgb-new 192 0 128))
