#lang racket

;;; tile.rkt
;;;   A wrapper for the tile functions

(require "tile-core")
(require louDBus/unsafe)
(provide (all-defined-out))

(define gimpplus (loudbus-proxy "edu.grinnell.cs.glimmer.GimpDBus"
                                "/edu/grinnell/cs/glimmer/gimp"
                                "edu.grinnell.cs.glimmer.gimpplus"))

(define image-fill!
  (lambda (image drawable color)
    (image-fill-core loudbus-call gimpplus image drawable color)))

