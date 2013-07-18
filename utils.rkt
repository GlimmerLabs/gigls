#lang racket

(require LoudGimp/hacks
         LoudGimp/higher)
         
(provide (all-defined-out))

;;; Procedure:
;;;   all-integer?
;;; Parameters:
;;;   lst, a list
;;; Purpose:
;;;   Check if the contents of lst are all integers
;;; Produces
;;;   all-int?, a boolean value
;;; Postconditions
;;;   If there is an i for which (integer? (list-ref lst i)) returns false,
;;;     all-int? is false.
;;;   Otherwise, all-int? is true.
(define all-integer?
  (lambda (lst)
    (or (null? lst)
        (and (integer? (car lst))
             (all-integer? (cdr lst))))))

;;; Procedure:
;;;   check-list?
;;; Parameters:
;;;   preds, a list of unary predicates
;;;   vals, a list of values
;;; Purpose:
;;;   Checks each predicate against the corresponding value
;;; Produces:
;;;   check?, a Boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If (length params) != (length vals) then
;;;     check? is false
;;;   If there is an i such that ((list-ref params i) (list-ref vals i))
;;;   does not hold, then
;;;     check? is false
;;;   Otherwise,
;;;     check? is true
(define check-list?
  (lambda (preds vals)
    (or (and (null? preds) (null? vals))
        (and (pair? preds) (pair? vals)
             ((car preds) (car vals))
             (check-list? (cdr preds) (cdr vals))))))

;;; Procedure:
;;;   sequence-contains?
;;; Parameters:
;;;   sequence, a list or vector
;;;   val, a Scheme value
;;; Purpose:
;;;   Determines if vec contains val.
;;; Produces:
;;;   contained?, a boolean
(define sequence-contains?
  (lambda (sequence val)
    (or (and (vector? sequence) (vector-contains? sequence val))
        (member? sequence val))))

;;; Procedure:
;;;   sequence-ref
;;; Parameters:
;;;   sequence - a list or vector
;;;   n - a non-negative integer
;;; Purpose:
;;;   Grab the nth element of the list or vector
;;; Produces:
;;;   val, a value
;;; Preconditions:
;;;   0 <= n < (length of squence-ref)
(define sequence-ref
  (lambda (sequence n)
    (if (vector? sequence)
        (vector-ref sequence n)
        (list-ref sequence n))))


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
;;;   member?
;;; Parameters:
;;;   val, a Scheme value
;;;   lst, a list
;;; Purpose:
;;;   Determine if val is an element of lst.
;;; Produces:
;;;   is-member? a Boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If there is an i s.t. (equal? val (list-ref lst i))
;;;     then is-member? is true (#t)
;;;   Otherwise,
;;;     is-member? is false (#f)
(define member?
  (lambda (val lst)
    (and (member val lst) #t)))

;;; mod is my favorite shorthand for modulo
(define mod modulo)

;;; Procedure:
;;;   process-gimp-result
;;; Parameters:
;;;   result, a Scheme value
;;; Purpose:
;;;   Clean up the result of a GIMP PDB call for convenience.
;;; Produces:
;;;   clean-result, a "cleaner" version of result
;;; Philosophy:
;;;   Almost all of the standard gimp procedures return their result
;;;   as a list.  Users generally want just the value.  This handles
;;;   special cases.
;;; Problems:
;;;   * Colors appear to be the an exception to the standard GIMP return
;;;     policy.  For example, gimp-image-get-pixel returns a triplet.
;;;   * Some procedures return multiple values.  In those cases, it makes
;;;     sense to leave them as a list.
(define process-gimp-result
  (lambda (result)
    (if (and (pair? result) (null? (cdr result)))
        (car result)
        result)))


;;; Procedure:
;;;   selection-op
;;; Parameters:
;;;   op, a potential selection operation
;;; Purpose:
;;;   Convert op to one of the four selection operations.
;;; Produces:
;;;   selop, an integer
;;; Philosophy:
;;;   For some reason, Script-Fu defines SUBTRACT as 8 rather than 1.
;;;   This procedure helps us ensure that we get the right values.
(define selection-op
  (lambda (op)
    (cond
      ((and (integer? op) (= op SUBTRACT))
       CHANNEL-OP-SUBTRACT)
      ((equal? op "ADD")
       CHANNEL-OP-ADD)
      ((equal? op "SUBTRACT")
       CHANNEL-OP-SUBTRACT)
      ((equal? op "REPLACE")
       CHANNEL-OP-REPLACE)
      ((equal? op "INTERSECT")
       CHANNEL-OP-INTERSECT)
      (else
       op))))

;;; Procedure:
;;;   string-contains?
;;; Parameters:
;;;   str, a string
;;;   pattern, a string
;;; Purpose:
;;;   Determine if str contains pattern
;;; Produces:
;;;   contains?, a Boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If there is an i s.t. (substring str i (+ i (string-length pattern))) = pattern
;;;     then contains? is true (#t)
;;;   Otherwise, contains is false (#f)
;;; Ponderings:
;;;   This currently has a very straightforward implementation.  A KMP matcher
;;;   would likely be more efficient.  We could even make it slightly more efficient 
;;;   by restricting our seach to the positions in which the first character of
;;;   patterns occurs.
(define _string-contains?
  (lambda (str pattern)
    (or (string=? pattern "")
        (let ((str-len (string-length str))
              (pat-len (string-length pattern)))
          (let kernel ((start 0)
                       (after pat-len))
            (and (<= after str-len)
                 (or (string=? (substring str start after) pattern)
                     (kernel (+ start 1) (+ after 1)))))))))

(define string-contains?
  (lambda (str pattern)
    (cond
      ((not (string? str))
       (error "string-contains?: first parameter must be a string, received" str))
      ((not (string? pattern))
       (error "string-contains?: second parameters must be a string, received" pattern))
      (else
       (_string-contains? str pattern)))))

;;; Procedure:
;;;   string-escape
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   Escape a few special characters (e.g., open and close
;;;   paren) by prefixing them with a backslash.
;;; Produces:
;;;   escaped, a string
;;; Philosophy:
;;;   Some GIMP proceduers, such as gimp-brushes-get-list,
;;;   appear to need escaped characters.  For example,
;;;   > (gimp-brushes-get-list "Circle (09)")
;;;   (0 ())
;;;   > (gimp-brushes-get-list "Circle \\(09\\)")
;;;   (1 ("Circle (09)"))
(define string-escape
  (let ((escaped-chars (list #\( #\))))
    (letrec ((charlist-escape
              (lambda (lst)
                (cond
                  ((null? lst)
                   null)
                  ((member (car lst) escaped-chars)
                   (cons #\\ (cons (car lst) (charlist-escape (cdr lst)))))
                  (else
                   (cons (car lst) (charlist-escape (cdr lst))))))))
      (lambda (str)
        (list->string (charlist-escape (string->list str)))))))


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
  
;;; Procedure:
;;;   vector-contains?
;;; Parameters:
;;;   vec, a vector
;;;   val, a Scheme value
;;; Purpose:
;;;   Determines if vec contains val.
;;; Produces:
;;;   contained?, a boolean
(define vector-contains?
  (lambda (vec val)
    (let kernel ((pos (- (vector-length vec) 1)))
      (and (>= pos 0)
           (or (equal? (vector-ref vec pos) val)
               (kernel (- pos 1)))))))
