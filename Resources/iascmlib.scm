; Created by scmlib on Thu Aug 25 09:25:56 2011

;;; iascmlib.scm
;;;   A collection of Scheme procedures and values intended to make
;;;   Scheme a bit easier to use.
;;;     This file is constructed automatically from a variety of separate
;;;   files.  
;;; Authors:
;;;   Janet Davis
;;;   Samuel A. Rebelsky
;;;   A large cadre of Grinnell students
;;; Version:
;;;   0.0.1.10 [For MediaScript 0.0.1.17d1]

;;; Copyright (c) 2008-2011 Janet Davis, Samuel A. Rebelsky
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;; 
;;;   * Redistributions in binary form must reproduce the above copyright
;;;     notice, this list of conditions and the following disclaimer in the
;;;     documentation and/or other materials provided with the distribution.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(define iascmlib-version "0.0.1.6b")
(define mediascript-version "0.0.1.17d1")


; +------------------------------------+----------------------------------------
; | Helpers for Checking Preconditions |
; +------------------------------------+

; [From iascm/precond/check-list-p.scm]

;;; Procedure:
;;;   check-list?
;;; Parameters:
;;;   preds, a list of unary predicates
;;;   vals, a list of values
;;; Purpose:
;;;   Checks each predicate against the corresponding value
;;; Produces:
;;;   check?, a Boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If (length params) != (length vals) then
;;;     check? is false
;;;   If there is an i such that ((list-ref params i) (list-ref vals i))
;;;   does not hold, then
;;;     check? is false
;;;   Otherwise,
;;;     check? is true
(define check-list?
  (lambda (preds vals)
    (or (and (null? preds) (null? vals))
        (and (pair? preds) (pair? vals)
             ((car preds) (car vals))
             (check-list? (cdr preds) (cdr vals))))))

; [From iascm/precond/error-arity.scm]

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

; [From iascm/precond/error-misc.scm]

;;; Procedure:
;;;   error/misc
;;; Parameters:
;;;   proc-name, a symbol
;;;   message, a string
;;;   params, a list of values
;;; Purpose:
;;;   Report a miscellaneous error
;;; Produces:
;;;   [Nothing, called for the side effect]
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   An appropriate error has been thrown
(define error/misc
  (lambda (proc-name message params)
    (error (string-append (symbol->string proc-name)
                          ": "
                          message
                          "\n  in "
                          (value->string (cons proc-name params) 60)))))

; [From iascm/precond/error-parameter-type.scm]

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

; [From iascm/precond/guard-flag.scm]

;;; Procedure:
;;;   guard-flag
;;; Parameters:
;;;   flag-name, a symbol
;;;   flag-proc, a procedure created by make-flag
;;; Purpose:
;;;   Create a "guarded" version of flag-proc, one that checks the
;;;   preconditions:
;;; Produces:
;;;   proc, a version of flag-proc that checks preconditions.
(define guard-flag
  (lambda (flag-name flag-proc)
    (lambda params
      (cond
        ((null? params)
         (flag-proc))
        ((not (null? (cdr params)))
         (error/arity flag-name "zero or one" params))
        ((not (boolean? (car params)))
         (error/parameter-type flag-name 1 'boolean params))
        (else
         (flag-proc (car params)))))))

; [From iascm/precond/guard-proc.scm]

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

; [From iascm/precond/guard-unary-proc.scm]

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

; [From iascm/precond/validate-param.scm]

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

; [From iascm/precond/validate-params.scm]

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

; [From iascm/precond/validate-unary.scm]

;;; Procedure:
;;;   validate-unary!
;;; Parameters:
;;;   name, a symbol
;;;   params, a list of values
;;; Purpose:
;;;   Verifies that params is of length 1 and produces an appropriate
;;;   error message if not.
;;; Produces:
;;;   [Nothing; Called for the side effect]
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If (length params) = 1, returns an unspecified value
;;;   Otherwise, does not return, but instead throws an appropriate error
(define validate-unary!
  (lambda (name params)
    (cond
      ((null? params)
       (error (string-append (symbol->string name) 
                             ": expects 1 parameter, given 0")))
      ((not (null? (cdr params)))
       (error (string-append (symbol->string name) 
                             ": expects 1 parameter, given "
                             (number->string (length params))
                             ":")
              params)))))

; [From iascm/precond/validate-binary.scm]

;;; Procedure:
;;;   validate-binary!
;;; Parameters:
;;;   name, a symbol
;;;   params, a list of values
;;; Purpose:
;;;   Verifies that params is of length 2 and produces an appropriate
;;;   error message if not.
;;; Produces:
;;;   [Nothing; Called for the side effect]
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If (length params) = 2, returns an unspecified value.
;;;   Otherwise, throws an appropriate error
(define validate-binary!
  (lambda (name params)
    (cond
      ((null? params)
       (error (string-append (symbol->string name) 
                             ": expects 2 parameters, given 0")))
      ((null? (cdr params))
       (error (string-append (symbol->string name) 
                             ": expects 2 parameters, given 1:")
              (car params)))
      ((not (null? (cddr params)))
       (error (string-append (symbol->string name) 
                             ": expects 2 parameters, given "
                             (number->string (length params))
                             ":")
              params)))))


; +-------------------------------------+---------------------------------------
; | Traditional Higher-Order Procedures |
; +-------------------------------------+

; [From iascm/hop/all.scm]

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

; [From iascm/hop/any.scm]

;;; Procedure:
;;;   any
;;; Parameters:
;;;   pred?, a unary predicate
;;;   lst, a list
;;; Purpose:
;;;   Determine if pred? holds for any of the values in lst.
;;; Produces:
;;;   ok?, a Boolean
;;; Preconditions:
;;;   [Standard]
;;; Postconditions:
;;;   If there is an i such that (pred? (list-ref lst i))
;;;     holds, then ok? is true.
;;;   Otherwise, ok? is false.
;;;   Alternately, suppose lst has the form (v0 v1 v2 ... vn)
;;;     ok? = (or (pred? v0) (pred? v1) (pred? v2) ... (pred? vn))
(define any
  (lambda (pred? lst)
    (and (not (null? lst))
         (or (pred? (car lst))
              (any pred? (cdr lst))))))

; [From iascm/hop/compose.scm]

;;; Procedure:
;;;   compose
;;; Parameters:
;;;   f, a procedure
;;;   g, a procedure
;;; Purpose:
;;;   Compose f and g.
;;; Produces:
;;;   fog, a procedure
;;; Preconditions:
;;;   f can be applied to the results returned by g.
;;; Postconditions:
;;;   (fog x) = (f (g x))
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

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
(define _make-bi-cycle
  (lambda contents
    (let* ((stuff (if (null? (cdr contents)) (car contents) contents)))
      (make-cycle (append stuff (cdr (reverse (cdr stuff))))))))

(define make-bi-cycle
  (lambda contents
    (cond
      ((null? contents)
       (error/arity 'make-bi-cycle 1 contents))
      ((and (null? (cdr contents)) 
            (not (list? (car contents))))
       (error/parameter-type 'make-bi-cycle 1 'list contents))
      ((and (null? (cdr contents))
            (< (length (car contents)) 3))
       (error/misc 'make-bi-cycle "requires at least three elements" contents))
      ((and (not (null? (cdr contents)))
            (< (length contents) 3))
       (error/misc 'make-bi-cycle "requires at least three elements" contents))
      (else
       (apply _make-bi-cycle contents)))))
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
(define _make-cycle
  (lambda contents
    (let* ((stuff (if (null? (cdr contents)) (car contents) contents))
           (vec (list->vector stuff))
           (len (vector-length vec))
           (pos -1))
      (lambda ()
        (set! pos (modulo (+ pos 1) len))
        (vector-ref vec pos)))))

(define make-cycle
  (lambda contents
    (cond
      ((null? contents)
       (error/arity 'make-cycle 1 contents))
      ((and (null? (cdr contents)) 
            (or (null? (car contents))
                (not (list? (car contents)))))
       (error/parameter-type 'make-cycle 1 'non-empty-list contents))
      (else
       (apply _make-cycle contents)))))
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
(define make-flag
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
(define make-state
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

; [From iascm/hop/o.scm]

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

; [From iascm/hop/left-section.scm]

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

; [From iascm/hop/repeat.scm]

;;; Procedure:
;;;   repeat!
;;; Parameters:
;;;   i, a non-negative integer
;;;   proc!, an n-ary procedure
;;;   v1 ... vn, 0 or more optional parameters
;;; Purpose:
;;;   Calls (proc! v1 ... vn) i times.
;;; Produces:
;;;   [Nothing; called for the side effect]
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   The procedure has been applied the specified number of times.
(define _repeat
  (lambda (i proc! . args)
    (let kernel ((i i))
      (cond 
        ((positive? i)
         (apply proc! args)
         (kernel (- i 1)))))))

(define repeat
  (lambda (i proc! . args)
    (let ((params (cons i (cons proc! args))))
      (cond
        ((or (not (integer? i)) (negative? i))
         (error/parameter-type 'repeat 1 'positive-integer params))
        ((not (procedure? proc!))
         (error/parameter-type 'repeat 2 'procedure params))
        (else 
         (apply _repeat params))))))

(define repeat! repeat)

; [From iascm/hop/right-section.scm]

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

; [From iascm/hop/sequence.scm]

;;; Procedure:
;;;   sequence
;;; Parameters:
;;;   proc1! proc2! ... procn!, procedures
;;; Purpose:
;;;   Create a procedure that applies proc1 through procn to its parameters
;;; Produces:
;;;   sequenced!, a procedure
;;; Preconditions:
;;;   proc1! ... procn! all accept the same kinds of parameters
;;; Postconditions:
;;;   (sequenced! val1 ... valm) =
;;;     (begin (proc1 val1 ... valm) 
;;;            (proc2 val1 ... valm)
;;;            ...
;;;            (procn val1 ... valm)
;;;            (void)))
(define _sequence
  (lambda procs
    (lambda params
      (let kernel ((remaining procs))
        (when (not (null? remaining))
          (apply (car remaining) params)
          (kernel (cdr remaining)))))))

(define sequence
  (lambda procs
    (cond
      ((not (all procedure? procs))
       (error/misc 'sequence "received non-procedure as parameter" procs))
      (else (apply _sequence procs)))))

          
; [From iascm/hop/swap-params.scm]

;;; Procedure:
;;;   swap-params
;;; Parameters:
;;;   binfunc, a two-parameter procedure
;;; Purpose:
;;;   Swap the two parameters
;;; Produces:
;;;   newfunc, a two-parameter procedure
;;; Postconditions:
;;;   (newfunc left right) = (binfunc right left)
(define swap-params
  (lambda (binfunc)
    (lambda (left right)
      (binfunc right left))))

; [From iascm/hop/constant.scm]

;;; Procedure:
;;;   constant
;;; Parameters:
;;;   c, a value
;;; Purpose:
;;;   Build a constant function for c.  The function always returns
;;;   c, no matter what parameters it is applied to.
;;; Produces:
;;;   constant-function, a function
;;; Postconditions:
;;;   (constant-function x) = c for all x.
(define constant
  (lambda (c)
    (lambda params c)))

; [From iascm/hop/param.scm]

;;; Procedure:
;;;   param
;;; Parameters:
;;;   i, an integer
;;; Purpose:
;;;   Build a function that extracts its ith parameter
;;; Produces:
;;;   ith, a multi-ary function
;;; Preconditions:
;;;   i >= 0
;;; Postconditions:
;;;   (ith v0 v1 v2 ... vi ... vn) = vi, provided i <= n
;;;   (ith v0 ... vn) is undefined if i > n
(define param
  (lambda (i)
    (lambda params
      (list-ref params i))))

; [From iascm/hop/lfold.scm]

;;; Procedure:
;;;   lfold
;;; Parameters
;;;   op, a binary procedure
;;;   lst, a list of the form (v0 v1 ... vn)
;;; Purpose:
;;;   Insert op between every pair of values in the list, grouping the
;;;   computation to the left.
;;; Produces:
;;;   result, a value
;;; Preconditions:
;;;   lst is nonempty.
;;;   op can be applied to the values in lst.
;;;   op can be applied to the results of op.
;;; Postconditions:
;;;   result = (op (... (op (op v0 v1) v2) ...) vn)
(define lfold
  (lambda (op lst)
    (let kernel ((result (car lst))
                 (remaining (cdr lst)))
      (if (null? remaining)
          result
          (kernel (op result (car remaining))
                  (cdr remaining))))))

; [From iascm/hop/rfold.scm]

;;; Procedure:
;;;   rfold
;;; Parameters
;;;   op, a binary procedure
;;;   lst, a list of the form (v0 v1 ... vn)
;;; Purpose:
;;;   Insert op between every pair of values in the list, grouping the
;;;   computation to the left.
;;; Produces:
;;;   result, a value
;;; Preconditions:
;;;   lst is nonempty.
;;;   op can be applied to the values in lst.
;;;   op can be applied to the results of op.
;;; Postconditions:
;;;   result = (op v1 (op v2 (op v3 ... (op vm vn) ...)))
(define rfold
  (lambda (op lst)
    (if (null? (cdr lst))
        (car lst)
        (op (car lst) (rfold op (cdr lst))))))
        

; [From iascm/hop/higher-conditionals.scm]

; +---------------------------------------------------------+-------------------
; | Higher-Order Versions of Conditional/Boolean Operations |
; +---------------------------------------------------------+

; [From iascm/hop/higher-and.scm]

;;; Procedure:
;;;   higher-and (aka ^and)
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

; [From iascm/hop/higher-if.scm]

;;; Procedure:
;;;   higher-if (aka ^if)
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

; [From iascm/hop/higher-not.scm]

;;; Procedure:
;;;   higher-not (aka ^not)
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

; [From iascm/hop/higher-or.scm]

;;; Procedure:
;;;   ^or (aka higher-or)
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

; [From iascm/hop/higher-true.scm]

;;; Procedure:
;;;   higher-true (aka ^true)
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

; [From iascm/hop/higher-false.scm]

;;; Procedure:
;;;   higher-false (aka ^false)
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


; +-------+---------------------------------------------------------------------
; | Lists |
; +-------+

; [From iascm/list/list-drop.scm]

;;; Procedure:
;;;   list-drop
;;; Parameters:
;;;   lst, a list
;;;   n, a non-negative integer
;;; Purpose:
;;;   Builds a new list consisting by removing the first n elements of lst.
;;; Produces:
;;;   newlst, a list
;;; Preconditions:
;;;   (length lst) >= n
;;; Postconditions:
;;;   (length newlst) = (length lst) - n
;;;   For each i, 0 <= i < (length newlst)
;;;     (list-ref newlst i) = (list-ref lst (+ n i))
(define _list-drop
  (lambda (lst n)
    (cond
      ((zero? n)
       lst)
      ((null? lst)
       null)
      (else
       (_list-drop (cdr lst) (- n 1))))))

(define list-drop
  (lambda (lst n)
    (cond
      ((not (list? lst))
       (error/parameter-type 'list-drop 1 'list (list lst n)))
      ((or (not (integer? n)) (negative? n))
       (error/parameter-type 'list-drop 2 'non-negative-integer (list lst n)))
      ((< (length lst) n)
       (error/parameter-type 'list-drop 1 'list-of-length-at-least-n (list lst n)))
      (else
       (_list-drop lst n)))))

; [From iascm/list/list-foreach.scm]

;;; Procedure:
;;;   list-foreach!
;;; Parameters:
;;;   lst, a list
;;;   proc!, a unary procedure called only for its side effects
;;; Purpose:
;;;   To call proc! for each element of lst.
;;; Produces:
;;;   [Nothing; called for the side effect]
;;; Preconditions:
;;;   proc! can be applied ot each element of x.
;;; Postconditions:
;;;   For each element x of lst, (proc! x) has been called.
(define list-foreach! 
  (lambda (lst proc!)
    (cond 
      ((null? lst)
       (void))
      (else
       (proc! (car lst))
       (list-foreach! (cdr lst) proc!)))))

; [From iascm/list/list-filter-out.scm]

;;; Procedure:
;;;   list-filter-out
;;; Parameters:
;;;   lst, a list of values
;;;   pred?, a unary predicate
;;; Purpose:
;;;   Remove all the values in lst for which pred? holds.
;;; Produces:
;;;   filtered, a list.
;;; Preconditions:
;;;   pred? can be applied to each value in lsts.
;;; Postconditions:
;;;   Every element of filtered appears in lst and in the same order.
;;;   If (not (pred? (list-ref lst i))) holds for some value in
;;;     lst, then (list-ref lst i) appears in filtered.
(define list-filter-out
  (lambda (pred? lst)
    (cond
      ((null? lst)
       null)
      ((pred? (car lst))
       (list-filter-out (cdr lst) pred?))
      (else
       (cons (car lst) (list-filter-out (cdr lst) pred?))))))

; [From iascm/list/list-index.scm]

;;; Procedure:
;;;   list-index
;;; Parameters:
;;;   lst, a list
;;;   val, a Scheme value
;;; Purpose:
;;;   Get the index of val in lst
;;; Produces:
;;;   index, a non-negative integer (or #f)
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If there is an i s.t.
;;;     (list-ref lst i) = val
;;;     then index is an integer.
;;;   Otherwise, index is #f
;;;   If index is an integer
;;;   (list-ref lst index) = val
(define _list-index
  (lambda (lst val)
    (let kernel ((remaining lst)
                 (position 0))
      (cond
        ((null? remaining) 
        #f)
        ((not (pair? remaining))
         (error/parameter-type 'list-index 1 'list (list lst val)))
        ((equal? (car remaining) val)
         position)
        (else
         (kernel (cdr remaining) (+ position 1)))))))

(define list-index _list-index)

; [From iascm/list/list-last.scm]

;;; Procedure:
;;;   list-last
;;; Parameters:
;;;   lst, a list
;;; Purpose:
;;;   Get the last element in lst
;;; Produces:
;;;   last, a value
;;; Preconditions:
;;;   (length lst) > 0
;;; Postconditions:
;;;   (list-ref lst (- (length lst) 1)) = last
(define list-last
  (lambda (lst)
    (if (null? (cdr lst))
        (car lst)
        (list-last (cdr lst)))))

; [From iascm/list/list-random.scm]

;;; Procedure:
;;;   list-random
;;; Parameters:
;;;   n, a non-negative integer
;;;   generate-random-value, a zero-ary function (aka "a thunk")
;;; Purpose:
;;;   Build an unpredictable list of n elements.
;;; Produces:
;;;   values, a list
;;; Preconditions:
;;;   It is difficult to predict the value generated by generate-random-value.
;;; Postconditions:
;;;   (length values) = n
;;;   Each value in values was generated by (generate-random-value)
(define list-random
  (lambda (n generate-random-value)
    (cond
      ((not (integer? n))
       (error "list-random: Expects <integer> for parameter 1, given" n))
      ((negative? n)
       (error "list-random: Expects <positive integer> for parameter 1, given"
              n))
      (else
       (_list-random n generate-random-value)))))

(define _list-random
  (lambda (n generate-random-value)
    (if (zero? n)
        null
        (cons (generate-random-value)
              (_list-random (- n 1) generate-random-value)))))

; [From iascm/list/list-random-element.scm]

;;; Procedure:
;;;   list-random-element
;;; Parameters:
;;;   values, a list
;;; Purpose:
;;;   Randomly select an element of values.
;;; Produces:
;;;   value, a value
;;; Preconditions:
;;;   values is nonempty.
;;; Postconditions:
;;;   value is an element of values.
;;;   value is equally likely to be any element of values.
(define list-random-elemeent
  (lambda (values)
    (list-ref values (random (length values)))))

; [From iascm/list/list-select.scm]

;;; Procedure:
;;;   list-select
;;; Parameters:
;;;   lst, a list of values
;;;   pred?, a unary predicate
;;; Purpose:
;;;   Select all the values in lst for which pred? holds.
;;; Produces:
;;;   selected, a list.
;;; Preconditions:
;;;   pred? can be applied to each value in lsts.
;;; Postconditions:
;;;   Every element of selected appears in lst and in the same order.
;;;   If (pred? (list-ref lst i)) holds for some value in
;;;     lst, then (list-ref lst i) appears in filtered.
(define list-select
  (lambda (lst pred?)
    (cond
      ((null? lst)
       null)
      ((pred? (car lst))
       (cons (car lst) (list-select (cdr lst) pred?)))
      (else
       (list-select (cdr lst) pred?)))))

; [From iascm/list/list-take.scm]

;;; Procedure:
;;;   list-take
;;; Parameters:
;;;   lst, a list
;;;   n, a non-negative integer
;;; Purpose:
;;;   Builds a new list consisting of the first n elements of lst.
;;; Produces:
;;;   newlst, a list
;;; Preconditions:
;;;   (length lst) >= n
;;; Postconditions:
;;;   (length newlst) = n
;;;   For each i, 0 <= i < n
;;;     (list-ref newlst i) = (list-ref lst i)
(define _list-take
  (lambda (lst n)
    (cond
      ((zero? n)
       null)
      ((null? lst)
       null)
      (else
       (cons (car lst) (_list-take (cdr lst) (- n 1)))))))

(define list-take 
  (lambda (lst n)
    (cond
      ((not (list? lst))
       (error/parameter-type 'list-take 1 'list (list lst n)))
      ((or (not (integer? n)) (negative? n))
       (error/parameter-type 'list-take 2 'non-negative-integer (list lst n)))
      ((< (length lst) n)
       (error/parameter-type 'list-take 1 'list-of-length-at-least-n (list lst n)))
      (else
       (_list-take lst n)))))

; [From iascm/list/make-list.scm]

;;; Procedure:
;;;   make-list
;;; Parameters:
;;;   n, a non-negative exact integer
;;;   val, a Scheme value
;;; Purpose:
;;;   Create a list of n copies of val.
;;; Produces:
;;;   lst, a list
;;; Preconditions:
;;;   [No additional]
;;: Postconditions:
;;;   (length lst) = n
;;;   For all i, 0 <= i < n
;;;     (list-ref lst i) = val
(define _make-list
  (lambda (n val)
    (if (zero? n)
        null
	(cons val (_make-list (- n 1) val)))))

(define make-list
  (guard-proc 'make-list
              _make-list
	      (list 'non-negative-exact-integer 'value)
	      (list (^and integer? (^not negative?) exact?) ^true)))



; +----------------------------------+------------------------------------------
; | Traditional List Procedure Names |
; +----------------------------------+

; While we've tried to use a <type>-<action> naming strategy, there are
; a number of traditional procedures that work with lists that (a) do
; not follow that strategy and (b) do not necessarily take a list as the
; first parameter. 

; [From iascm/list/foreach.scm]

;;; Procedure:
;;;   foreach!
;;; Parameters:
;;;   proc!, a unary procedure called only for its side effects
;;;   lst, a list
;;; Purpose:
;;;   To call proc! for each element of lst.
;;; Produces:
;;;   [Nothing, called for the side effect]
;;; Preconditions:
;;;   proc! can be applied to each element of lst.
;;; Postconditions:
;;;   For each element, x, of lst, (proc! x) has been called.
(define foreach! 
  (lambda (proc! lst)
    (cond
      ((null? lst)
       (void))
      (else
       (proc! (car lst))
       (foreach! proc! (cdr lst))))))

; [From iascm/list/last.scm]

;;; Procedure:
;;;   last
;;; Parameters:
;;;   lst, a nonempty list
;;; Purpose:
;;;   Get the last element in lst
;;; Produces:
;;;   lastval, a value
;;; Preconditions:
;;;   (length lst) > 0
;;; Postconditions:
;;;   lasval = (list-ref lst (- (length lst) 1)) 
(define last
  (lambda (lst)
    (if (null? (cdr lst))
        (car lst)
        (last (cdr lst)))))

; [From iascm/list/member-p.scm]

;;; Procedure:
;;;   member?
;;; Parameters:
;;;   val, a Scheme value
;;;   lst, a list
;;; Purpose:
;;;   Determine if val is an element of lst.
;;; Produces:
;;;   is-member? a Boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If there is an i s.t. (equal? val (list-ref lst i))
;;;     then is-member? is true (#t)
;;;   Otherwise,
;;;     is-member? is false (#f)
(define member?
  (lambda (val lst)
    (and (member val lst) #t)))

; [From iascm/list/range.scm]

;;; Procedure:
;;;   range
;;; Parameters:
;;;   lower, an integer
;;;   upper, an integer
;;; Purpose:
;;;   Create a list of integers representing [lower .. upper)
;;; Produces:
;;;   r, a list of integers
;;; Preconditions:
;;;   lower <= upper
;;; Postconditions:
;;;   r contains all the integers i, s.t. lower <= i < upper.
;;;   The elements of r are in sorted order from smallest to
;;;   largest.
(define _range
  (lambda (lower upper)
    (if (< lower upper)
        (cons lower (_range (+ lower 1) upper))
        null)))

(define range
  (guard-proc 'range
              _range
              (list 'integer 'integer)
              (list integer? integer?)))

; [From iascm/list/select.scm]

;;; Procedure:
;;;   select
;;; Parameters:
;;;   pred?, a unary predicate
;;;   lst, a list of values
;;; Purpose:
;;;   Select all the values in lst for which pred? holds.
;;; Produces:
;;;   selected, a list.
;;; Preconditions:
;;;   pred? can be applied to each value in lsts.
;;; Postconditions:
;;;   Every element of select appears in lst and in the same order.
;;;   If (pred? (list-ref lst i)) holds for some value in
;;;     lst, then (list-ref lst i) appears in filtered.
(define select
  (lambda (pred? lst)
    (cond
      ((null? lst)
       null)
      ((pred? (car lst))
       (cons (car lst) (select pred? (cdr lst))))
      (else
       (select pred? (cdr lst))))))

; [From iascm/list/tally.scm]

;;; Procedure:
;;;   tally
;;; Parameters:
;;;   pred?, a unary predicate
;;;   lst, a list
;;; Purpose:
;;;   Count the number of values in the list for which pred? holds.
;;; Produces:
;;;   count, an integer
;;; Preconditions:
;;;   pred? can be applied to each value in lst.
;;; Postconditions:
;;;   There exist exactly count values in lst for which
;;;     (pred? val) holds.
(define tally
  (lambda (pred? lst)
    (let kernel ((count 0)
                 (remaining lst))
      (cond
        ((null? remaining)
         count)
        ((pred? (car remaining))
         (kernel (+ count 1)
                 (cdr remaining)))
        (else
         (kernel count
                 (cdr remaining)))))))


; +---------------------------+-------------------------------------------------
; | Additional Math Functions |
; +---------------------------+

; [From iascm/math/bound.scm]

;;; Procedure:
;;;   bound
;;; Parameters:
;;;   val, a number
;;;   lower, a number
;;;   upper, a number
;;; Purpose:
;;;   bound val to be in the range [lower...upper]
;;; Produces:
;;;   bounded, a value
;;; Preconditions:
;;;   lower < upper
(define bound
  (lambda (val lower upper)
    (min (max val lower) upper)))

; [From iascm/math/decrement.scm]

;;; Procedure:
;;;   decrement
;;; Parameters:
;;;   val, a number
;;; Purpose:
;;;   Subtract 1 from val
;;; Produces:
;;;   decremented, a number
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   incremented = (+ 1 val)
;;; Ponderings:
;;;   An obvious procedure, but one that is often useful, particularly
;;;   for beginners.
(define decrement (r-s - 1))

; [From iascm/math/increment.scm]

;;; Procedure:
;;;   increment
;;; Parameters:
;;;   val, a number
;;; Purpose:
;;;   Add 1 to val
;;; Produces:
;;;   incremented, a number
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   incremented = (+ 1 val)
;;; Ponderings:
;;;   An obvious procedure, but one that is often useful.
(define increment (l-s + 1))

; [From iascm/math/integer-to-ordinal.scm]

;;; Procedure:
;;;   integer-to-ordinal
;;; Parameters:
;;;   n, an integer
;;; Purpose:
;;;   Build a string that simply describes n as an ordinal value
;;; Produces:
;;;   ordinal, a string
(define integer->ordinal
  (lambda (n)
    (let ((last-digit (modulo (abs n) 10))
          (last-two-digits (modulo (abs n) 100)))
      (string-append (number->string n)
                     (cond
                       ((= last-two-digits 11) "th")
                       ((= last-two-digits 12) "th")
                       ((= last-two-digits 13) "th")
                       ((= last-digit 1) "st")
                       ((= last-digit 2) "nd")
                       ((= last-digit 3) "rd")
                       (else "th"))))))

; [From iascm/math/iota.scm]

;;; Procedure:
;;;   iota
;;; Parameters:
;;;   n, a positive integer
;;; Purpose:
;;;   Create a list of all non-negative integers less than n.
;;; Produces:
;;;   ints, a list of integers
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (length ints) = n
;;;   ints has the form (0 1 2 3 ...)
(define _iota
  (lambda (n)
    (let kernel ((i 0))
      (if (= i n)
          null
          (cons i (kernel (+ i 1)))))))

(define iota 
  (lambda (n)
    (validate-param! 'iota 'non-negative-integer (^and integer? (^not negative?)) (list n))
    (_iota n)))

; [From iascm/math/pi.scm]

(define pi 3.141592653589793)

; [From iascm/math/square.scm]

;;; Procedure:
;;;   square
;;; Parameters:
;;;   n, a number
;;; Purpose:
;;;   Computes n squared
;;; Produces:
;;;   nsquared, a number
;;; Preconditions:
;;;   [No additional preconditions]
;;; Postconditions:
;;;   nsquared = n*n
(define square
  (lambda (n)
    (* n n)))


; +---------------------------+-------------------------------------------------
; | Additional I/O Procedures |
; +---------------------------+

; [From iascm/io/eol-char-p.scm]

;;; Procedure:
;;;   eol-char?
;;; Parameters:
;;;   ch, a character
;;; Purpose:
;;;   Determines if the character is the end-of-line character.
;;; Produces:
;;;   is-eol?, a boolean.
(define eol-char? 
    (lambda (ch)
      (equal? ch #\newline)))

; [From iascm/io/read-line.scm]

;;; Procedure:
;;;   read-line
;;; Parameters:
;;;   input-port, an input port
;;; Purpose:
;;;   Reads one line from the input port.
;;; Produces:
;;;   line, a string.
;;; Preconditions:
;;;   input-port is open for reading.
;;; Postconditions:
;;;   One line has been read.
(define read-line
  (let ((cr (string-ref "\r" 0)))
    ; The kernel reads a list of characters until hitting the newline.
    (letrec ((kernel
              (lambda (input-port)
                (let ((ch (read-char input-port)))
                  (cond
                    ; At the end of line, there are no more characters.
                    ((or (eof-object? ch) (eol-char? ch))
                     null)
                    ; Drop carriage returns.  (Macs and PCs tend to insert
                    ; them before newlines.)
                    ((equal? ch cr) (kernel input-port))
                    (else (cons ch (kernel input-port))))))))
      (lambda (input-port)
        (list->string (kernel input-port))))))

; [From iascm/io/read-lines.scm]

;;; Procedure:
;;;   read-lines
;;; Parameters:
;;;   fname, a string
;;; Purpose:
;;;   Reads all the lines from the file.
;;; Produces:
;;;   lines, a list of strings 
;;; Preconditions:
;;;   fname names a readable file.
;;; Postconditions:
;;;   (list-ref lines i) represents line i of file fname.
(define read-lines
  (letrec ((kernel 
            (lambda (input-port)
              (if (eof-object? (peek-char input-port))
                  (begin
                    (close-input-port input-port)
                    null)
                  (cons (read-line input-port)
                        (kernel input-port))))))
    (lambda (fname)
      (kernel (open-input-file fname)))))


; +------------------------------+----------------------------------------------
; | Additional String Operations |
; +------------------------------+

; [From iascm/string/string-contains-p.scm]

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
(define string-contains?
  (lambda (str pattern)
    (cond
      ((not (string? str))
       (error "string-contains?: first parameter must be a string, received" str))
      ((not (string? pattern))
       (error "string-contains?: second parameters must be a string, received" pattern))
      (else
       (_string-contains? str pattern)))))

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

; [From iascm/string/string-escape.scm]

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


; +------------------------------+----------------------------------------------
; | Additional Vector Operations |
; +------------------------------+

; [From iascm/vector/vector-append.scm]

;;; Procedure:
;;;   vector-append
;;; Parameters:
;;;   left, a vector
;;;   right, a vector
;;; Purpose:
;;;   Create a new vector by appending left and right
(define vector-append
  (lambda (left right)
    (let* ((left-length (vector-length left))
           (right-length (vector-length right))
           (newvec (make-vector (+ left-length right-length))))
      (vector-slurp! newvec 0 left-length left 0)
      (vector-slurp! newvec left-length right-length right 0))))


; [From iascm/vector/vector-contains-p.scm]

;;; Procedure:
;;;   vector-contains?
;;; Parameters:
;;;   vec, a vector
;;;   val, a Scheme value
;;; Purpose:
;;;   Determines if vec contains val.
;;; Produces:
;;;   contained?, a boolean
(define vector-contains?
  (lambda (vec val)
    (let kernel ((pos (- (vector-length vec) 1)))
      (and (>= pos 0)
           (or (equal? (vector-ref vec pos) val)
               (kernel (- pos 1)))))))

; [From iascm/vector/vector-map.scm]

;;; Procedure:
;;;   vector-map
;;; Parmeters:
;;;   proc, a unary function
;;;   vec, a vector
;;; Purpose:
;;;   To apply proc to all values in vec.
;;; Produces:
;;;   newvec, a vector
;;; Preconditions:
;;;   proc must be applicable to all values in vec
;;; Postconditions:
;;;   (vector-length newvec) = (vector-length vec)
;;;   For all reasonable i,
;;;     (vector-ref newvec i) = (proc (vector-ref vec i))
(define vector-map
  (lambda (proc vec)
    (let* ((len (vector-length vec))
           (newvec (make-vector len)))
      (let kernel ((pos 0))
        (if (equal? pos len)
            newvec
            (begin
              (vector-set! newvec pos (proc (vector-ref vec pos)))
              (kernel (+ pos 1))))))))

; [From iascm/vector/vector-random-element.scm]

;;; Procedure:
;;;   vector-random-element
;;; Parameters:
;;;   values, a vector
;;; Purpose:
;;;   Randomly select an element of values.
;;; Produces:
;;;   value, a value
;;; Preconditions:
;;;   values is nonempty.
;;; Postconditions:
;;;   value is an element of values.
;;;   value is equally likely to be any element of values.
(define vector-random-element
  (lambda (values)
    (vector-ref values (random (vector-length values)))))

; [From iascm/vector/vector-slurp.scm]

;;; Procedure:
;;;   vector-slurp!
;;; Parameters:
;;;   target, a vector
;;;   target-pos, an integer
;;;   len, an integer
;;;   source, an integer
;;;   source-pos, an integer
;;; Purpose:
;;;   Copy len characters from source (starting at position start-pos)
;;;;  To target (starting at postition target-pos).
;;; Produces:
;;;   target, the updated version of the target
;;; Preconditions:
;;;   0 <= target-pos
;;;   0 <= source-pos
;;;   0 <= len
;;;   (+ target-pos len) <= (vector-length target)
;;;   (+ source-pos len) <= (vector-length source)
;;; Postconditions:
;;;   source is unchanged
;;;   For each i, 0 <= i < len
;;;     (vector-ref target (+ target-pos i)) = 
;;;       (vector-ref source (+ source-pos i))
(define vector-slurp!
  (lambda (target target-pos len source source-pos)
    (if (= 0 len)
        target
        (begin
          (vector-set! target target-pos (vector-ref source source-pos))
          (vector-slurp! target (+ target-pos 1) (- len 1)
                         source (+ source-pos 1))))))

; [From iascm/vector/vector-transform.scm]

;;; Procedure:
;;;   vector-transform!
;;; Parmeters:
;;;   vec, a vector
;;;   proc, a unary function
;;; Purpose:
;;;   To apply proc to all values in vec.
;;; Produces:
;;;   vec, the same vector
;;; Preconditions:
;;;   proc must be able to operate on all values in vec
;;; Postconditions:
;;;   Let VEC be the original version of vec
;;;     (vector-ref vec i) = (proc (vector-ref VEC i))
(define vector-transform!
  (lambda (proc vec)
    (let ((len (vector-length vec)))
      (let kernel ((pos 0))
        (if (equal? pos len)
            vec
            (begin
              (vector-set! vec pos (proc (vector-ref vec pos)))
              (kernel (+ pos 1))))))))


; +-----------------------------------+-----------------------------------------
; | Miscellaneous IaScheme Procedures |
; +-----------------------------------+

; [From iascm/misc/all-integer-p.scm]

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

; [From iascm/misc/id.scm]

;;; Procedure:
;;;   id
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Do nothing
;;; Produces:
;;;   val
(define id
  (lambda (val) val))
