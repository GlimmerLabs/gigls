#lang racket

(require gigls/pdb-dbus)

(provide (all-defined-out))

;;; Procedure:
;;;   drawable?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val is a valid drawable.
(define/contract drawable?
  (-> any/c boolean?)
  (lambda (val)
    (and (integer? val)
         (not (= 0 (car (gimp-item-is-valid val))))
	 (not (= 0 (car (gimp-item-is-drawable val)))))))
