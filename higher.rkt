#lang racket
(provide higher-and     ^and
         higher-false   ^false
         higher-if      ^if
         higher-not     ^not
         higher-or      ^or
         higher-true    ^true
         left-section   l-s
         right-section  r-s 
         all)


;;; Procedure:
;;;   all
;;; Parameters:
;;;   pred?, a unary predicate
;;;   lst, a list
;;; Purpose:
;;;   Determine if pred? holds for all the values in lst.
;;; Produces:
;;;   ok?, a Boolean
;;; Preconditions:
;;;   [Standard]
;;; Postconditions:
;;;   If there is an i such that (pred? (list-ref lst i))
;;;     fails to hold, then ok? is false.
;;;   Otherwise, ok? is true.
(define all
  (lambda (pred? lst)
    (or (null? lst)
        (and (pred? (car lst))
             (all pred? (cdr lst))))))

;;; Procedures:
;;;   higher-and
;;;   ^and
;;; Parameters:
;;;   f1, f2, ... fn, functions
;;; Purpose:
;;;   A higher-order version of and
;;; Produces:
;;;   fun, a function
;;; Preconditions:
;;;   f1, ..., fn accept the same number of parameters
;;; Postconditions:
;;;   (fun val1 ... valm) =
;;;     (and (f1 val1 ... valm) (f2 val1 ... valm) ... (fn val1 ... valm))
(define higher-and
  (letrec ((kernel
            (lambda (funs args)
              (or (null? funs)
                  (and (apply (car funs) args)
                       (kernel (cdr funs) args))))))
    (lambda funs
      (lambda args
        (kernel funs args)))))
(define ^and higher-and)

;;; Procedures:
;;;   higher-false
;;;   ^false
;;; Parameters:
;;;   v1, v2, ..., vn, some values
;;; Purpose:
;;;   A higher-order version of false (#f)
;;; Produces:
;;;   fun, a function
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (fun v1 ... vm) = #t
(define higher-false
  (lambda args #f))
(define ^false higher-false)

;;; Procedures:
;;;   higher-if
;;;   ^if
;;; Parameters:
;;;   test?, a predicate
;;;   yes, a function
;;;   no, a a function
;;; Purpose:
;;;   "Higher Order If": Builds a function that uses test? to choose
;;;   between yes and no.
;;; Produces:
;;;   fun, a function 
;;; Postconditions:
;;;   (chooser val1 ... valn) = 
;;;     (if (test? val1 ... valn) 
;;;         (yes val1 ... valn) 
;;;         (no val1 ... valn))
(define higher-if
  (lambda (test? yes no)
    (lambda params
      (if (apply test? params)
          (apply yes params)
          (apply no params)))))
(define ^if higher-if)

;;; Procedures:
;;;   higher-not 
;;;   ^not
;;; Parameters:
;;;   pred?, a predicate
;;; Purpose:
;;;   "Higher Order Not": Builds a function that returns the opposite
;;;   value of pred?.
;;; Produces:
;;;   not-pred?, a predicate
;;; Postconditions:
;;;   (not-pred? val1 ... valn) = (not (pred? val1 ... valn))
(define higher-not
  (lambda (pred?)
    (lambda params
      (not (apply pred? params)))))
(define ^not higher-not)

;;; Procedures:
;;;   higher-or
;;;   ^or
;;; Parameters:
;;;   f1, f2, ... fn, functions
;;; Purpose:
;;;   A higher-order version of or
;;; Produces:
;;;   fun, a function
;;; Preconditions:
;;;   f1, ..., fn accept the same number of parameters
;;; Postconditions:
;;;   (fun val1 ... valm) =
;;;     (or (f1 val1 ... valm) (f2 val1 ... valm) ... (fn val1 ... valm))
(define higher-or
  (letrec ((kernel
            (lambda (funs args)
              (and (not (null? funs))
                   (or (apply (car funs) args)
                       (kernel (cdr funs) args))))))
    (lambda funs
      (lambda args
        (kernel funs args)))))
(define ^or higher-or)

;;; Procedures:
;;;   higher-true 
;;;   ^true
;;; Parameters:
;;;   v1, v2, ..., vn, some values
;;; Purpose:
;;;   A higher-order version of true (#t)
;;; Produces:
;;;   fun, a function
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (fun v1 ... vm) = #t
(define higher-true
  (lambda args #t))
(define ^true higher-true)


;;; Procedures:
;;;   left-section 
;;;   l-s
;;; Parameters:
;;;   binproc, a two-parameter procedure
;;;   left, a value
;;; Purpose:
;;;   Creates a one-parameter procedure by filling in the first parameter
;;    of binproc. 
;;; Produces:
;;;   unproc, a one-parameter procedure 
;;; Preconditions:  
;;;   left is a valid first parameter for binproc.
;;; Postconditions:
;;;   (unproc right) = (binproc left right)
(define left-section
  (lambda (binproc left)
    (lambda (right) (binproc left right))))
(define l-s left-section)

;;; Procedures:
;;;   right-section 
;;;   r-s
;;; Parameters:
;;;   binproc, a two-parameter procedure
;;;   right, a value
;;; Purpose:
;;;   Creates a one-parameter procedure by filling in the second parameter
;;    of binproc. 
;;; Produces:
;;;   unproc, a one-parameter procedure 
;;; Preconditions:  
;;;   left is a valid first parameter for binproc.
;;; Postconditions:
;;;   (unproc left) = (binproc left right)
(define right-section
  (lambda (binproc right)
    (lambda (left) (binproc left right))))
(define r-s right-section)

