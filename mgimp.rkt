#lang racket

(provide (all-defined-out))

; Hack!
(define mgimp-get-color-names
  (lambda stuff
    (vector "red" "green" "blue")))


; List of color-name/RGB pairs created in mpltg with
; (define color-table
;   (list->vector
;     (map (lambda (cname)
;            (cons cname (list->vector (color->rgb-list cname))))
;          (vector->list (mgimp-get-color-names)))))


