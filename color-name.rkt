#lang racket

; gigls/color-name.rkt
;   Procedures for working with color names.

(require gigls/guard
         gigls/pdb-dbus
         gigls/utils)

(provide (all-defined-out))

;;; Name:
;;;   all-color-names
;;; Type:
;;;   list of strings
;;; Value:
;;;   All of the valid color names
(define all-color-names
  (sequence->list (cadr (ggimp-rgb-list))))

;;; Procedure:
;;;   color-name?
;;; Parameters:
;;;   val, a string
;;; Purpose:
;;;   Determines if val names a color.
;;; Produces:
;;;   is-color-name?, a boolean
(define color-name?
  (lambda (val)
    (and (string? val)
         (sequence-contains? all-color-names val))))

;;; Procedure:
;;;   guard-color-name-proc
;;; Parameters:
;;;   procname, a symbol
;;;   proc, a one-parameter procedure that expects a color name
;;; Purpose:
;;;   Creates a version of proc that checks that its parameter
;;;   is a color name.
;;; Produces:
;;;   guarded-proc, a procedure
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If val is a color name, then
;;;     (guarded-proc val) = (proc val)
;;;   Otherwise
;;;     (guarded-proc val) reports an error
(define guard-color-name-proc
  (lambda (procname proc)
    (guard-unary-proc procname proc 'color-name color-name?)))

