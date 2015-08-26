#lang racket

; section.rkt
;   A simple mechanism for sectioning (or so I hope)
;   The cut operations, which is similar, is in  (require srfi/26)

(provide (all-defined-out))

;;; Macro
;;;   section
;;; Parameters:
;;;   proc, a procedure
;;;   value-or-underscore-1, a value or the underscore symbol (_)
;;;   value-or-underscore-2, a value or the underscore symbol (_)
;;;   ...
;;;   value-or-underscore-n, a value or the underscore symbol (_)
(define-syntax section
  (lambda (stx)
    (let ([info (syntax->datum stx)])
      (cond
        [(symbol? info)
         (datum->syntax stx '(quote <macro:section>))]
        [(null? (cdr info))
         (error "section: Requires a procedure")]
        [else
         (let ([sec (car info)]
               [proc (cadr info)]
               [params (cddr info)])
           (let kernel ([params params]
                        [formals null]
                        [actuals null])
             (cond
               [(null? params)
                (let ([code (list 'lambda (reverse formals)
                                  (cons proc (reverse actuals)))])
                  ; (write code) (newline) ; CHECKING
                  (datum->syntax stx code))]
               [(eq? (car params) '<>)
                (let ([formal (gensym)])
                  (kernel (cdr params)
                          (cons formal formals)
                          (cons formal actuals)))]
               [else 
                (kernel (cdr params)
                        formals
                        (cons (car params) actuals))])))]))))

