#lang racket
(require LoudGimp/stringify)
(provide guard-proc
         guard-unary-proc)

;;; Procedure:
;;;   guard-proc
;;; Parameters:
;;;   name, a symbol
;;;   proc, a procedure 
;;;   param-types, a list of symbols of the form (sym1 ... symn)
;;;   param-preds, a list of predicates of the form (pred1 .. predn)
;;; Purpose:
;;;   Build a new version of proc that checks preconditions.
;;; Produces:
;;;   guarded, a procedure of the form 
;;; Preconditions:
;;;   (length param-types) = (length param-preds)
;;; Postconditions:
;;;   If proc is called correctly, 
;;;     (guarded val1 ... valn) = (proc val1 ... valn)
;;;  Otherwise
;;;     (guarded val1 ... valn) throws an appropriate error
;;;   Otherwise, (guarded-proc val) throws an appropriate error
(define guard-proc
  (lambda (name proc param-types param-preds)
    (lambda params
      (validate-params! name param-types param-preds params)
      (apply proc params))))

;;; Procedure:
;;;   guard-unary-proc
;;; Parameters:
;;;   name, a symbol
;;;   proc, a procedure of the form (lambda (val) ___)
;;;   param-type, a symbol
;;;   param-pred, a unary predicate
;;; Purpose:
;;;   Build a new version of proc that checks preconditions.
;;; Produces:
;;;   guarded-proc, a procedure of the form (lambda (val) _____)
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If (param-pred? val), (guarded-proc val) = (proc val)
;;;   Otherwise, (guarded-proc val) throws an appropriate error
(define guard-unary-proc
  (lambda (name proc param-type param-pred)
    (guard-proc name proc (list param-type) (list param-pred))))

;;; Procedure:
;;;   validate-param!
;;; Parameters:
;;;   proc-name, a symbol
;;;   type, a symbol
;;;   pred, a symbol
;;;   params, a list of length one
;;; Purpose:
;;;   Makes sure that the one paramemeter meets the corresponding predicate.
;;; Produces:
;;;   [Nothing, called for the side effect]
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If the predicate holds on the corresponding parameter, does nothing.
;;;   Otherwise, terminates with an error message.
(define validate-param!
  (lambda (proc-name type pred params)
     (cond
       ((not (pred (car params)))
        (error/parameter-type proc-name 1 type params)))))

;;; Procedure:
;;;   validate-params!
;;; Parameters:
;;;   proc-name, a symbol
;;;   param-types, a list of symbols
;;;   param-preds, a list of predicates
;;    params, a list of values
;;; Purpose:
;;;   Makes sure that all of the params meet the corresponding predicate
;;; Produces:
;;;   [Nothing, called for the side effect]
;;; Preconditions:
;;;   (length param-types) = (length paramx-predss) [Unverified]
;;; Postconditions:
;;;   If every predicate holds on the corresponding parameter, does nothing.
;;;   Otherwise, terminates with an error message.
(define validate-params!
  (lambda (proc-name param-types param-preds parameters)
    (let kernel ((number 0)
                 (types param-types)
                 (preds param-preds)
                 (params parameters))
      (cond
        ((and (null? types) (null? params))
         (void))
        ((and (null? types) (not (null? params)))
         (error/arity proc-name number parameters))
        ((and (null? params) (not (null? types)))
         (error/arity proc-name (length param-types) parameters))
        ((not ((car preds) (car params)))
         (error/parameter-type proc-name (+ 1 number) (car types) parameters))
        (else
         (kernel (+ number 1) (cdr types) (cdr preds) (cdr params)))))))

;;; Procedure:
;;;   error/arity
;;; Parameters:
;;;   proc-name, a symbol
;;;   arity, an integer
;;;   params, a list of values
;;; Purpose:
;;;   Report an arity error ("expects M parameters; called with N")
;;; Produces:
;;;   [Nothing, called for the side effect]
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   An appropriate error has been thrown
(define error/arity
  (lambda (proc-name arity params)
    (let* ((num-params (length params))
           (expected (if (and (number? arity) (= 1 arity))
                         ": expects 1 parameter"
                         (string-append ": expects "
                                        (if (string? arity) arity (value->string arity))
                                        " parameters"))))
      (error (string-append (symbol->string proc-name)
                            expected
                            ", given "
                            (number->string num-params)
                            "\n  in "
                            (value->string (cons proc-name params) 60))))))

;;; Procedure:
;;;   error/parameter-type
;;; Parameters:
;;;   proc-name, a symbol
;;;   param-num, a non-negative integer
;;;   param-type, a symbol
;;;   params, a list
;;; Purpose:
;;;   Reports a parameter type error
;;; Produces:
;;;   [Nothing!  This procedure never returns.]
(define error/parameter-type
  (lambda (proc-name param-num param-type params)
    (error (string-append (symbol->string proc-name)
                          ": expects type <"
                          (symbol->string param-type)
                          "> for "
                          (integer->ordinal param-num)
                          " parameter, given "
                          (value->string (list-ref params (- param-num 1)) 40)
                          "\n  in "
                          (value->string (cons proc-name params) 60)))))

