#lang racket
(provide (all-defined-out))

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

;;; Procedure:
;;;   any
;;; Parameters:
;;;   pred?, a unary predicate
;;;   lst, a list
;;; Purpose:
;;;   Determines if pred? holds for any of the values in lst
;;; Produces:
;;;   ok?, a Boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If there is an i s.t. (pred? (list-ref lst i)) holds, then
;;;     ok? is true.
;;;   If for all i, (pred? (list-ref list i)) does not hold, then
;;;     ok? is false.
(define any
  (lambda (pred? lst)
    (and (not (null? lst))
         (or (pred? (car lst))
	     (any pred? (cdr lst))))))

;;; Procedure:
;;;   range-checker
;;; Parameters:
;;;   lb, a real number
;;;   ub, a real number
;;; Purpose:
;;;   Create a procedure that determines if its parameter is between
;;;   lb and ub.
;;; Produces:
;;;   checker, a boolean
;;; Preconditions:
;;;   No additional
;;; Postconditions:
;;;   (checker val) holds iff (<= lb val ub)
(define _range-checker
  (lambda (lb ub)
    (lambda (val)
      (<= lb val ub))))

(define range-checker
  (lambda (lb ub)
    (cond
      [(not (real? lb))
       (error "range-checker expected real for param 1, received" lb)]
      [(not (real? ub))
       (error "range-checker expected real for param 2, received" ub)]
      [else (_range-checker lb ub)])))

;;; Procedure:
;;;   constant
;;; Parameters:
;;;   val, a value
;;; Purpose:
;;;   Build a constant function for val. The function always returns
;;;   val, no matter what parameters it is applied to.
;;; Produces:
;;;   constant func, a function
;;; Postconditions:
;;;   (constant func x) = val for all x.
(define constant
  (lambda (val)
    (lambda stuff val)))

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
;;;   'Higher Order If: Builds a function that uses test? to choose
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
;;;   'Higher Order Not': Builds a function that returns the opposite
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

;;; Procedure:
;;;   o
;;; Parameters:
;;;   fun1 ... funn
;;; Purpose:
;;;   Compose fun1 ... funn
;;; Produces:
;;;   fun, a function
;;; Preconditions:
;;;   Each function can be applied to the results of the subsequent
;;;   function.
;;; Postconditions:
;;;   (fun x) = (fun1 (fun2 (.... (funn x)...)))
(define o
  (lambda funs
    (lambda (x)
      (let kernel ((remaining (reverse funs))
                   (val x))
        (if (null? remaining)
            val
            (kernel (cdr remaining) ((car remaining) val)))))))

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
;;;    of binproc. 
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
;;;    of binproc. 
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

