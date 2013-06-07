#lang racket
(provide integer->ordinal
         value->string)

;;; Procedure:
;;;   integer->ordinal
;;; Parameters:
;;;   n, an integer
;;; Purpose:
;;;   Build a string that simply describes n as an ordinal value
;;; Produces:
;;;   ordinal, a string
(define integer->ordinal
  (lambda (n)
    (let ((last-digit (modulo (abs n) 10))
          (last-two-digits (modulo (abs n) 100)))
      (string-append (number->string n)
                     (cond
                       ((= last-two-digits 11) "th")
                       ((= last-two-digits 12) "th")
                       ((= last-two-digits 13) "th")
                       ((= last-digit 1) "st")
                       ((= last-digit 2) "nd")
                       ((= last-digit 3) "rd")
                       (else "th"))))))

;;; Procedure:
;;;   value->string
;;; Parameters:
;;;   val, a Scheme value
;;;   maxlen, an optional integer
;;; Purpose:
;;;   convert val to a string
;;; Produces:
;;;   str, a string representation of value
(define value->string
  (letrec
       ; Convert a list of values to a string.
       ((list-to-string
         (lambda (lst)
           (if (null? lst) 
               "()"
               (string-append "("
                              (value->string (car lst))
                              (list-to-string-kernel (cdr lst))
                              ")"))))
        ; Convert the interior of a list to a string.
        (list-to-string-kernel
         (lambda (lst)
           (cond 
             ((null? lst) "")
             ((pair? lst) 
              (string-append " " (value->string (car lst))
                             (list-to-string-kernel (cdr lst))))
             (else 
              (string-append " . " (value->string lst))))))
        ; Convert a vector to a string
        (vector-to-string
         (lambda (vec)
           (string-append "#("
                          (vector-to-string-kernel vec 
                                                   (- (vector-length vec) 1))
                          ")")))
        ; Convert postions [0..pos] of a vector to a string
        ; for the interior of the vector
        (vector-to-string-kernel
         (lambda (vec pos)
           (cond 
             ((< pos 0) 
              "")
             ((= pos 0)
              (value->string (vector-ref vec 0)))
             (else
              (string-append (vector-to-string-kernel vec (- pos 1))
                             " "
                             (value->string (vector-ref vec pos)))))))
        ; Convert a value to an arbitrarily-long string.
        (kernel
         (lambda (val)
           (cond
             ((symbol? val)
              (symbol->string val))
             ((number? val)
              (number->string val))
             ((eq? val #t)
              "#t")
             ((eq? val #f)
              "#f")
             ((string? val)
              (string-append "\"" val "\""))
             ((procedure? val)
              "<procedure>")
             ((vector? val)
              (vector-to-string val))
             ((pair? val)
              (list-to-string val))
             (else
              "<value>")))))
    (lambda (val . maxlen)
      (let ((result (kernel val)))
        (if (and (not (null? maxlen))
                 (> (string-length result) (car maxlen)))
            (string-append
             (substring result 0 (- (car maxlen) 3))
             "...")
            result)))))
  
