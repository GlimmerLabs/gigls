#lang racket

(require LoudGimp/hacks)

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