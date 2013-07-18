#lang racket

(require LoudGimp/gimp-dbus)

(provide (all-defined-out))

;;; Procedure:
;;;   drawable?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val is a valid drawable.
(define drawable?
  (lambda (val)
    (and (integer? val)
         (not (= 0 (car (gimp-item-is-valid val))))
	 (not (= 0 (car (gimp-item-is-drawable val)))))))
