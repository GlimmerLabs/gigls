#lang racket

; LoudGimp/gimpplus.rkt
;   A simple module to provide access to the additionalGimp functions.

(require louDBus/unsafe)

(provide (all-defined-out))

(define gimpplus (loudbus-proxy "edu.grinnell.cs.glimmer.GimpDBus"
                                "/edu/grinnell/cs/glimmer/gimp"
                                "edu.grinnell.cs.glimmer.gimpplus"))
