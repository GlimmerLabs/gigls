#lang racket

; gigls/color-name.rkt
;   Procedures for working with color names.

(require gigls/guard
         gigls/pdb-dbus
         gigls/utils)

(provide (all-defined-out))

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
         (sequence-contains? (context-get-color-names) val))))

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

;;; Procedure:
;;;   context-get-color-names
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Get a list of all the available color names.
;;; Produces:
;;;   names, a list of strings
;;; Partners:
;;;   (context-find-color-names "NAME")
;;;      Provides a way to find a list of names that include "NAME".
;;; Included in colors (not context) to avoid interdependencies
(define context-get-color-names 
  (lambda ()
    (cadr (ggimp-rgb-list))))

