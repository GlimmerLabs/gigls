#lang racket

; gigls/rgb-list.rkt
;   Procedures for working with lists of red/green/blue components

(require gigls/guard
         gigls/utils)

(provide (all-defined-out))

;;; Procedure
;;;   rgb-list?
;;; Parameters
;;;   val, a scheme value
;;; Purpose
;;;   Check if val is an rgb color
;;; Produces
;;;   is-rgb-list, a boolean value
;;; Preconditions
;;;   [none]
;;; Postconditions
;;;   Returns #t if val is an rgb color represented as a list of
;;;     the three components.
;;;   Returns #f otherwise.
(define/contract rgb-list?
  (-> any/c boolean?)
  (lambda (val)
    (and (list? val) (equal? (length val) 3) (all-integer? val))))

;;; Procedure:
;;;   guard-rgb-list-proc
;;; Parameters:
;;;   procname, a symbol
;;;   proc, a one-parameter procedure that expects an RGB list
;;; Purpose:
;;;   Creates a version of proc that checks that its parameter
;;;   is an RGB list.
;;; Produces:
;;;   guarded-proc, a procedure
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If val is an RGB list, then
;;;     (guarded-proc val) = (proc val)
;;;   Otherwise
;;;     (guarded-proc val) reports an error
(define guard-rgb-list-proc
  (lambda (procname proc)
    (guard-unary-proc procname proc 'rgb-list rgb-list?)))

