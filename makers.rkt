#lang racket
(require gigls/guard)
(provide (all-defined-out))

; [From iascm/hop/make-bi-cycle.scm]

;;; Procedure:
;;;   make-bi-cycle
;;; Purpose:
;;;   Make a bi-directional cycle procedure
;;; Parameters:
;;;   contents, a list
;;; Produces:
;;;   cycle!, a zero-parameter procedure
;;; Preconditions:
;;;   (length contents) > 2
;;; Postconditions:
;;;   The nth call to (cycle!) produces
;;;     (list-ref (append contents (cdr (reverse (cdr contents))))
;;;               (modulo n (+ -2 (* 2 (length contents)))))
;;; Practica:
;;;   > (define example! (make-bi-cycle 'a 'b 'c 'd))
;;;   > (repeat 20 (lambda () (display (example!))))
;;;   abcdcbabcdcbabcdcbab
(define/contract make-bi-cycle
  (-> (flat-named-contract
       'a-list-with-length-longer-than-2
       (lambda (lst)
         (> (length lst) 2)))
      (-> any/c))
  (lambda contents
    (let* ((stuff (if (null? (cdr contents)) (car contents) contents)))
      (make-cycle (append stuff (cdr (reverse (cdr stuff))))))))

;(define make-bi-cycle
;  (lambda contents
;    (cond
;      ((null? contents)
;       (error/arity 'make-bi-cycle 1 contents))
;      ((and (null? (cdr contents)) 
;            (not (list? (car contents))))
;       (error/parameter-type 'make-bi-cycle 1 'list contents))
;      ((and (null? (cdr contents))
;            (< (length (car contents)) 3))
;       (error/misc 'make-bi-cycle "requires at least three elements" contents))
;      ((and (not (null? (cdr contents)))
;            (< (length contents) 3))
;       (error/misc 'make-bi-cycle "requires at least three elements" contents))
;      (else
;       (apply _make-bi-cycle contents)))))
; [From iascm/hop/make-cycle.scm]

;;; Procedure:
;;;   make-cycle
;;; Parameters:
;;;   contents, a list
;;; Purpose:
;;;   Create a procedure that cycles through the values in contents.
;;; Produces:
;;;   cycle!, a zero-parameter procedure
;;; Preconditions:
;;;   (length contents) > 0
;;; Postconditions:
;;;   The nth call to (cycle!) returns
;;;     (list-ref contents (modulo n (length contents)))
;;; Practicum:
;;;   > (define simple! (make-cycle (list 1 2 3)))
;;;   > (simple!)
;;;   1
;;;   > (simple!)
;;;   2
;;;   > (simple!)
;;;   3
;;;   > (simple!)
;;;   1
(define/contract make-cycle
  (-> list? (-> any/c))
  (lambda contents
    (let* ((stuff (if (null? (cdr contents)) (car contents) contents))
           (vec (list->vector stuff))
           (len (vector-length vec))
           (pos -1))
      (lambda ()
        (set! pos (modulo (+ pos 1) len))
        (vector-ref vec pos)))))

;(define make-cycle
;  (lambda contents
;    (cond
;      ((null? contents)
;       (error/arity 'make-cycle 1 contents))
;      ((and (null? (cdr contents)) 
;            (or (null? (car contents))
;                (not (list? (car contents)))))
;       (error/parameter-type 'make-cycle 1 'non-empty-list contents))
;      (else
;       (apply _make-cycle contents)))))
; [From iascm/hop/make-flag.scm]

;;; Procedure:
;;;   make-flag
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Creates a new "flag" procedure that stores a modifiable Boolean
;;;   flag.
;;; Produces:
;;;   proc, a parameter of zero-or-one parameters
;;; Postconditions:
;;;   (proc val) sets the state to val and returns val.
;;;   (proc) gets the state.  That is, returns the last val for which
;;;     there was a call to (proc val).  If there was no such call,
;;;     returns #t.
; No contract for this one because Gemma doesn't know how to make contracts
; for functions that produce functions with varying inputs
(define/contract make-flag
  (-> (->* () (any/c) boolean?))
  (lambda ()
    (let ((flag #t))
      (lambda params
        (cond
          ((null? params)
           flag)
          (else
           (set! flag (and (car params) #t))
           flag))))))

; [From iascm/hop/make-state.scm]

;;; Procedure:
;;;   make-state
;;; Parameters:
;;;   init, a value
;;; Purpose:
;;;   Creates a new "state" procedure that keeps a single state value.
;;; Produces:
;;;   proc, a parameter of zero-or-one parameters
;;; Postconditions:
;;;   The initial state of proc is given by init.
;;;   (proc val) sets the state to val and returns the previous state.
;;;   (proc) gets the state.  That is, returns the last val for which
;;;     there was a call to (proc val).  If there was no such call,
;;;     returns init.
(define/contract make-state
  (-> any/c (->* () (any/c) any/c))
  (lambda (init)
    (let ((state init))
      (lambda params
        (cond
          ((null? params)
           state)
          (else
           (let ((oldstate state))
             (set! state (car params))
             oldstate)))))))
