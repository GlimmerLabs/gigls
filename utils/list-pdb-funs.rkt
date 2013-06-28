#lang racket
(require louDBus/unsafe)

(define gimp (loudbus-proxy "edu.grinnell.cs.glimmer.GimpDBus"
                            "/edu/grinnell/cs/glimmer/gimp"
                            "edu.grinnell.cs.glimmer.pdb"))
(define methods (loudbus-methods gimp))
(for-each (lambda (method)
           (display method)
           (newline))
         methods)