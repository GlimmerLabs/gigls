#lang racket
(require louDBus/unsafe)
(provide (all-defined-out))

(define altgimp (loudbus-proxy "edu.grinnell.cs.glimmer.GimpDBus"
                               "/edu/grinnell/cs/glimmer/gimp"
                               "edu.grinnell.cs.glimmer.gimpplus"))
(define alt-loudbus-helper
  (lambda (fun)
    (lambda args
      (apply loudbus-call (cons altgimp (cons fun args))))))

(define ggimp-about (alt-loudbus-helper 'ggimp_about))
(define ggimp-quit (alt-loudbus-helper 'ggimp_quit))
(define ggimp-rgb-red (alt-loudbus-helper 'ggimp_rgb_red))
