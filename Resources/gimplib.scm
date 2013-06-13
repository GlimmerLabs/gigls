; Created by scmlib on Thu Aug 25 09:25:56 2011

;;; gimplib.scm
;;;   A collection of Scheme procedures and values intended to make
;;;   the GIMP a bit easier for novices to use.
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

(define gimplib-version "0.0.1.9b")
(define mediascript-version "0.0.1.17d1")


; +-------------------+---------------------------------------------------------
; | Preliminary Notes |
; +-------------------+

; Organization
;   There are, in effect, three sections to this library.
;     iascm: General extensions to the Scheme language
;     mscm: "Media"-specific extensions, generally focusing on images
;     gimp: GIMP-specific exteinsions
;   
;   Within each section, procedures and values are generally broken down 
;   by category and/or name.

; Naming
;   For many procedures, particularly procedures directly related to the
;   GIMP, we've tried to be careful about precondition checking.  However,
;   if every procedure does this checking, the program can waste a lot of
;   time doing duplicated checks.  Hence, we are transitioning to a model
;   in which there are two versions of most procedures: The "normal" version
;   of the procedure checks preconditions, the "unsafe" version does not.
;   Unsafe versions have the same name as do normal versions, except that
;   the name is preceded by an underscore.


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


; +-------------------------------------------+---------------------------------
; | Representation-Independent Color Routines |
; +-------------------------------------------+

; [From mscm/color/color-p.scm]

;;; Procedure:
;;;   color?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determines if val is a color in one of the valid representations.
;;; Produces:
;;;   is-color?, a Boolean
;;; Preconditions
;;;   [No additional]
;;; Postconditions
;;;   returns #t if val is a valid kind of color, and #f otherwise.
(define color? 
  (lambda (val)
    (or (rgb? val) 
        (rgb-list? val) 
        (hsv? val))))

; [From mscm/color/color-representation.scm]

;;; Procedure:
;;;   color-representation
;;; Parameters:
;;;   color, a color
;;; Purpose:
;;;   Determine what representation is used for color
;;; Produces:
;;;   representation, a symbol (or #f)
(define color-representation
  (lambda (color)
    (cond
      ((rgb? color) 'RGB)
      ((rgb-list? color) 'RGB-LIST)
      ((hsv? color) 'HSV)
      (else #f))))

; [From mscm/color/color-to-hsv.scm]

;;; Procedure:
;;;   color->hsv
;;; Parameters:
;;;   color, one of the many permitted forms of colors.
;;; Purpose:
;;;   Convert color to an hsv color.
;;; Produces:
;;;   hsv, an HSV color
(define color->hsv
  (lambda (color)
    (cond
      ((not (color? color))
       (error "color->hsv: invalid color" color))
      ((hsv? color)
       color)
      (else
       (rgb->hsv (color->rgb color))))))

; [From mscm/color/color-to-rgb.scm]

;;; Procedure:
;;;   color->rgb
;;; Parameters:
;;;   color, a color
;;; Purpose:
;;;   Convert any form of color to rgb.
;;; Produces:
;;;   rgb-color, a color
;;; Preconditions:
;;;   color must be a valid color (color-name, rgb, rgb-list, ...)
;;; Postconditions:
;;;   rgb-color has the same components as color
(define color->rgb
  (lambda (color)
    (cond
      ((rgb? color) color)
      ((hsv? color) (hsv->rgb color))
      ((rgb-list? color) (rgb-list->rgb color))
      (else
       (error "Unknown type of color" color)))))

; [From mscm/color/color-to-rgb-list.scm]

;;; Procedure:
;;;   color->rgb-list
;;; Parameters:
;;;   color, a color
;;; Purpose:
;;;   Convert a color (in either list or vector form) to list form. 
;;; Produces:
;;;   rgb-list, a list of three values
;;; Preconditions:
;;;  color is a valid color [unverified]
(define color->rgb-list
  (lambda (color)
    (if (rgb-list? color) 
        color
        (rgb->rgb-list (color->rgb color)))))

; [From mscm/color/color-to-string.scm]

;;; Procedure:
;;;   color->string
;;; Parameters:
;;;   color, a color [verified]
;;; Purpose:
;;;   Convert color to a string easy for a novice to read
;;; Produces:
;;;   colorstring, a string of the form R/G/B
;;; Preconditions:
;;;   color is a valid rgb color.  That is, (rgb? color) holds.
;;; Postconditions:
;;;   R is (rgb.red color), G is (rgb.green color), B is (rgb.blue color)
(define color->string
  (lambda (color)
    (rgb->string (color->rgb color))))


; +---------------------------------------------+-------------------------------
; | Representing Colors as Encoded RGB Triplets |
; +---------------------------------------------+

; We generaly deal with colors in the GIMP as RGB triplets.  For efficiency,
; we encode these triplets as integers.

; In MediaScheme RGB, colors serve as the canonical representation of
; colors.  For every other representation of colors, we provide methods
; that convert to and from RGB colors.

; [From mscm/rgb/rgb-new.scm]

;;; Procedure:
;;;   rgb-new
;;; Parameters:
;;;   red, a real number
;;;   green, a real number
;;;   blue, a real number
;;; Purpose:
;;;   Create a real number
;;; Produces:
;;;   rgb, an RGB color
(define rgb-new
  (guard-proc 'rgb-new
              _rgb-new
              (list 'real 'real 'real)
              (list real? real? real?)))

; [From mscm/rgb/rgb-p.scm]

;;; Procedure
;;;   rgb?
;;; Parameters:
;;;   val, a value
;;; Purpose:
;;;   Determines if val can be interpreted as an RGB color
;;; Produces:
;;;   is-rgb?, a Boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If we can apply the various RGB procedures to val, then
;;;     is-rgb? is true (#t)
;;;   Otherwise, is_rgb? is false (#f)
(define rgb? _rgb?)

; [From mscm/rgb/rgb-red.scm]

;;; Procedure:
;;;   rgb-red
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   Extract the red component from an RGB color.
;;; Produces:
;;;   red, an integer
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   0 <= red < 256
;;;   rgb = (rgb-new (rgb-red rgb) (rgb-green rgb) (rgb-blue rgb))
(define rgb-red
  (guard-unary-proc 'rgb-red _rgb-red 'rgb rgb?))

; [From mscm/rgb/rgb-green.scm]

;;; Procedure:
;;;   rgb-green
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   Extract the green component from an RGB color.
;;; Produces:
;;;   green, an integer
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   0 <= green < 256
;;;   rgb = (rgb-new (rgb-red rgb) (rgb-green rgb) (rgb-blue rgb))
(define rgb-green
  (guard-unary-proc 'rgb-green _rgb-green 'rgb rgb?))

; [From mscm/rgb/rgb-blue.scm]

;;; Procedure:
;;;   rgb-blue
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   Extract the blue component from an RGB color.
;;; Produces:
;;;   blue, an integer
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   0 <= blue < 256
;;;   rgb = (rgb-new (rgb-red rgb) (rgb-green rgb) (rgb-blue rgb))
(define rgb-blue
  (guard-unary-proc 'rgb-blue _rgb-blue 'rgb rgb?))

; [From mscm/rgb/guard-rgb-proc.scm]

;;; Procedure:
;;;   guard-rgb-proc
;;; Parameters:
;;;   name, a symbol
;;;   proc, a procedure of the form (lambda (rgb) ___)
;;; Purpose:
;;;   Build a new version of proc that checks preconditions.
;;; Produces:
;;;   guarded-proc, a procedure of the form (lambda (rgb) _____)
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If (rgb? val), (guarded-proc val) = (proc val)
;;;   Otherwise, (guarded-proc val) throws an appropriate error
(define guard-rgb-proc
  (lambda (name proc)
    (lambda params
      (validate-unary! name params)
      (validate-param! name 'rgb rgb? params)
      (proc (car params)))))

; [From mscm/rgb/rgb-distance-squared.scm]

;;; Procedure:
;;;   rgb-distance-squared
;;; Parameters:
;;;   rgb1, an RGB color
;;;   rgb2, an RGB color
;;; Purpose:
;;;   Compute the distance squared between rgb1 and rgb2.
;;; Produces:
;;;   dsquared, an integer
;;; Preconditions:
;;;   None.
;;; Postconditions:
;;;   dsquared is the square of the Cartesian difference in a 3D colorspace.
;;; Philosophy:
;;;   It's easy to compute the squared distance.  It takes additional
;;;   (and unnecessary) power to compute the normal distance, so we
;;;   usually just use the squared distance.
(define _rgb-distance-squared
  (let ((square (lambda (x) (* x x))))
    (lambda (rgb1 rgb2)
      (+ (square (- (rgb-red rgb1) (rgb-red rgb2)))
         (square (- (rgb-green rgb1) (rgb-green rgb2)))
         (square (- (rgb-blue rgb1) (rgb-blue rgb2)))))))

(define rgb-distance-squared
  (let ((proc-name 'rgb-distance-squared)
        (param-types (list 'rgb 'rgb))
        (param-preds (list rgb? rgb?)))
    (lambda params
      (validate-binary! proc-name params)
      (validate-params! proc-name param-types param-preds params)
      (apply _rgb-distance-squared params))))

; [From mscm/rgb/rgb-interpolate.scm]

;;; Procedure:
;;;   rgb-interpolate
;;; Parameters:
;;;   c1, an RGB color
;;;   c2, an RGB color
;;;   percent, a real number between 0 and 1 (inclusive)
;;; Purpose:
;;;   Compute the color that is percent of the way from c1 to c2.
;;; Produces:
;;;   newcolor, an RGB color
(define _rgb-interpolate
  (lambda (c1 c2 percent)
    (rgb-new (+ (* percent (rgb-red c1)) (* (- 1 percent) (rgb-red c2)))
             (+ (* percent (rgb-green c1)) (* (- 1 percent) (rgb-green c2)))
             (+ (* percent (rgb-blue c1)) (* (- 1 percent) (rgb-blue c2))))))

(define rgb-interpolate
  (let ((proc-name 'rgb-interpolate)
        (param-types (list 'rgb 'rgb 'real))
        (param-preds (list rgb? rgb? real?)))
    (lambda (c1 c2 percent)
      (validate-params! proc-name param-types param-preds (list c1 c2 percent))
      (_rgb-interpolate c1 c2 percent))))

; [From mscm/rgb/rgb-map.scm]

;; Procedure:
;;;   rgb-map
;;; Parameters:
;;;   rgb, an RGB color
;;;   func, a function from components (integers in the range [0..255]) to
;;;     components
;;; Purpose:
;;;   Create a new RGB color by applying func to each component.
;; Produces:
;;;   new-rgb an RGB color
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (rgb? new-rgb)
;;;   (rgb-red new-rgb) = (func (rgb-red rgb))
;;;   (rgb-green new-rgb) = (func (rgb-green rgb))
;;;   (rgb-blue new-rgb) = (func (rgb-blue rgb))
(define _rgb-map
  (lambda (rgb func)
    (rgb-new (func (rgb-red rgb))
             (func (rgb-green rgb))
             (func (rgb-blue rgb)))))

(define rgb-map _rgb-map)

; [From mscm/rgb/rgb-transformer.scm]

;;; Procedure:
;;;   rgb-transformer
;;; Parameters:
;;;   redfunc, a function from colors to integers in the range [0..255].
;;;   greenfunc, a function from colors to integers in the range [0..255].
;;;   bluefunc, a function from colors to integers in the range [0..255].
;;; Purpose:
;;;   Combine the three functions into a color transformer.
;;; Produces:
;;;   transform, a function from colors to colors.
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (transform color) = 
;;;     (rgb-new (redfunc color) (greenfunc color) (bluefunc color)))
(define _rgb-transformer
  (lambda (redfunc greenfunc bluefunc)
    (lambda (color)
      (rgb-new (redfunc color) (greenfunc color) (bluefunc color)))))

(define rgb_transformer _rgb-transformer)
(define rgb-t _rgb-transformer)

; [From mscm/rgb/rgb-redder.scm]

;;; Procedure:
;;;   rgb-redder
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   Produce a redder version of rgb
;;; Produces:
;;;   redder, an RGB color
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (rgb-red redder) >= (rgb-red rgb)
(define _rgb-redder
  (lambda (color)
    (rgb-new (min 255 (+ 32 (rgb-red color)))
             (rgb-green color)
             (rgb-blue color))))

(define rgb-redder (guard-rgb-proc 'rgb-redder _rgb-redder))

; [From mscm/rgb/rgb-greener.scm]

;;; Procedure:
;;;   rgb-greener
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   Produce a greener version of rgb
;;; Produces:
;;;   greener, an RGB color
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (rgb-green greener) >= (rgb-green rgb)
(define _rgb-greener
  (lambda (color)
    (rgb-new (rgb-red color)
             (min 255 (+ 32 (rgb-green color)))
             (rgb-blue color))))

(define rgb-greener (guard-rgb-proc 'rgb-greener _rgb-greener))

; [From mscm/rgb/rgb-bluer.scm]

;;; Procedure:
;;;   rgb-bluer
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   Produce a bluer version of rgb
;;; Produces:
;;;   bluer, an RGB color
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (rgb-blue bluer) >= (rgb-blue rgb)
(define _rgb-bluer
  (lambda (color)
    (rgb-new (rgb-red color)
             (rgb-green color)
             (min 255 (+ 32 (rgb-blue color))))))

(define rgb-bluer (guard-rgb-proc 'rgb-bluer _rgb-bluer))

; [From mscm/rgb/rgb-flatten.scm]

;;; Procedure:
;;;   rgb-flatten
;;; Parameters:
;;;   rgb, an RGB color
;;;   unit, a positive integer
;;; Purpose:
;;;   "Flatten" rgb to a more limited color space
;;; Produces:
;;;   flattened, an RGB color
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (rgb-red flattened) is evenly divisible by unit
;;;   (rgb-green flattened) is evenly divisible by unit
;;;   (rgb-blue flattened) is evenly divisible by unit
;;;   (rgb-red flattened) is within unit of (rgb-red rgb)
;;;   (rgb-green flattened) is within unit of (rgb-green rgb)
;;;   (rgb-blue flattened) is within unit of (rgb-blue rgb)
(define _rgb-flatten
  (lambda (rgb unit)
    (rgb-map (o (l-s * unit) floor (r-s / unit)) rgb)))

(define rgb-flatten
  (let ((proc-name 'rgb-flatten)
        (param-types (list 'rgb 'integer))
        (param-preds (list rgb? integer?)))
    (lambda params
      (validate-binary! proc-name params)
      (validate-params! proc-name param-types param-preds params)
      (apply _rgb-flatten params))))

; [From mscm/rgb/rgb-greyscale.scm]

;;; Procedures:
;;;   rgb-greyscale
;;; Parameters:
;;;   rgb, an rgb color
;;; Purpose: 
;;;   Convert rgb to an appropriate shade of grey
;;; Produces:
;;;   grey, an RGB color.
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (rgb-red grey) = (rgb-green grey) = (rgb-blue grey)
;;;   grey has a similar brightness to rgb
(define _rgb-greyscale
  (lambda (rgb)
    (let ((ave (+ (* 0.30 (rgb-red rgb)) 
                  (* 0.59 (rgb-green rgb)) 
                  (* 0.11 (rgb-blue rgb)))))
      (rgb-new ave ave ave))))

(define rgb-greyscale (guard-rgb-proc 'rgb-greyscale _rgb-greyscale))

; [From mscm/rgb/rgb-rotate.scm]

;;; Procedure:
;;;   rgb-rotate
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   Compute a 'rotated' version of rgb
;;; Produces:
;;;   rotated, an RGB color
(define _rgb-rotate
  (lambda (rgb)
    (rgb-new (rgb-green rgb)
             (rgb-blue rgb)
             (rgb-red rgb))))

(define rgb-rotate (guard-rgb-proc 'rgb-rotate _rgb-rotate))

; [From mscm/rgb/rgb-darker.scm]

;;; Procedure:
;;;   rgb-darker
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   Compute a darker version of rgb
;;; Produces:
;;;   darker, an RGB color.
(define _rgb-darker
  (r-s rgb-map (o (l-s max 0) (r-s - 16))))

(define rgb-darker (guard-rgb-proc 'rgb-darker _rgb-darker))

; [From mscm/rgb/rgb-lighter.scm]

;;; Procedure:
;;;   rgb-lighter
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   Compute a lighter version of rgb
;;; Produces:
;;;   lighter, an RGB color.
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   lighter is likely to be interpreted as similar to, but lighter than
;;;   rgb.
(define _rgb-lighter
  (r-s rgb-map (o (l-s min 255) (r-s + 16))))

(define rgb-lighter (guard-rgb-proc 'rgb-lighter _rgb-lighter))

; [From mscm/rgb/rgb-complement.scm]

;;; Procedure:
;;;   rgb-complement
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   Compute the pseudo-complement of rgb
;;; Produces:
;;;   complement, an RGB color
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (+ (rgb-red rgb) (rgb-red complement)) = 255
;;;   (+ (rgb-green rgb) (rgb-green complement)) = 255
;;;   (+ (rgb-blue rgb) (rgb-blue complement)) = 255
(define _rgb-complement
  (r-s rgb-map (l-s - 255)))

(define rgb-complement (guard-rgb-proc 'rgb-complement _rgb-complement))

; [From mscm/rgb/rgb-phaseshift.scm]

;;; Procedure:
;;;   rgb-phaseshift
;;; Parameters:
;;;   rgb, an RGB color
;;; Purpose:
;;;   "Phase shift" rgb by adding 128 to components less than or equal 
;;;   to 128 and subtracting 128 from components greater than 128.
;;; Produces:
;;;   shifted, an RGB color
(define _rgb-phaseshift
  (r-s rgb-map (o (r-s modulo 256) (l-s + 128))))

(define rgb-phaseshift (guard-rgb-proc 'rgb-phaseshift _rgb-phaseshift))

; [From mscm/rgb/rgb-to-hsv.scm]

;;; Procedure:
;;;   rgb->hsv
;;; Parmeters:
;;;   rgb, an rgb color
;;; Purpose:
;;;   To convert an rgb color into an hsv color.
;;; Produces:
;;;   hsv, a three-element list containing hue, saturation and value.
;;; Preconditions(d
;;;   rgb must be a valid rgb color.
;;; Postconditions
;;;   hsvcolor contains three floats which (with rounding) roughly correspond 
;;;     to the hsv values given for that color in GIMP.  Values are not 
;;;     rounded to preserve color fidelity when converting from rgb to hsv 
;;;     and then back to rgb again.
;;;  (hsv->rgb hsv) should produce color.
(define _rgb->hsv
  (lambda (color)
    (list (_rgb->hue color) (_rgb->saturation color) (_rgb->value color))))

(define rgb->hsv (guard-rgb-proc 'rgb->hsv _rgb->hsv))

; [From mscm/rgb/rgb-to-rgb-list.scm]
(d(define
; [From mscm/rgb/rgb-to-string.scm]

;;; Procedure:
;;;   rgb->string
;;; Parameters:
;;;   color, an rgb color [verified]
;;; Purpose:
;;;   Convert color to a string easy for a novice to read
;;; Produces:
;;;   colorstring, a string of the form R/G/B
;;; Preconditions:
;;;   color is a valid rgb color.  That is, (rgb? color) holds.
;;; Postconditions:
;;;   R is (rgb.red color), G is (rgb.green color), B is (rgb.blue color)
(define _rgb->string
  (lambda (color)
    (string-append (number->string (rgb-red color)) 
                   "/"
		   (number->string (rgb-green color))
		   "/"
		   (number->string (rgb-blue color)))))

(define rgb->string (guard-rgb-proc 'rgb->string _rgb->string))

; [From mscm/rgb/rgb-to-hue.scm]

;;; Procedure
;;;  rgb->hue
;;; Parmeters:
;;;  col, an rgb color
;;; Purpose:
;;;  Compute the hue of the co(de(Define all-itnlor, in degrees on the color wheel.
;;; Produces:
;;;  hue, a float between 0 and 360.
;;; Preconditions
;;;  color must be a valid rgb color
;;; Postconditions
;;;  After rounding, hue should correspond to the Gimp's hue while 
;;;    examining color.
(define _rgb->hue
  (lambda (rgb)
    (let* ((components (rgb->rgb-list rgb))
           (cmax (apply max components))
           (cmin (apply min components))
           (r (rgb-red rgb))
           (g (rgb-green rgb))
           (b (rgb-blue rgb)))
      (cond
        ((equal? cmax cmin) 0)
        ((and (equal? cmax r) (>= g b)) (* 60 (/ (- g b) (- cmax cmin))))
        ((and (equal? cmax r) (< g b)) (+ (* 60 (/ (- g b) (- cmax cmin))) 360))
        ((equal? cmax g) (+ (* 60 (/ (- b r) (- cmax cmin))) 120))
        ((equal? cmax b) (+ (* 60 (/ (- r g) (- cmax cmin))) 240))))))

(define rgb->hue (guard-rgb-proc 'rgb->hue _rgb->hue))

; [From mscm/rgb/rgb-to-saturation.scm]

;;; Procedure
;;;  rgb->saturation
;;; Parmeters:
;;;  col, an rgb color
;;; Purpose:
;;;  Compute the saturation of the color.
;;; Produces:
;;;  saturation, a float between 0 and 1.
;;; Preconditions
;;;  color must be a valid rgb color.
;;; Postconditions
;;;  After multiplying by 100 and rounding, saturation should correspond 
;;;    to the Gimp's saturation while examining color.
(define _rgb->saturation
  (lambda (col)
    (let* ((color (color->rgb-list col))
          (cmax (apply max color))
          (cmin (apply min color)))
      (if (equal? cmax 0)
          0
          (- 1 (/ cmin cmax))))))

(define rgb->saturation (guard-rgb-proc 'rgb->saturation _rgb->saturation))

; [From mscm/rgb/rgb-to-value.scm]

;;; Procedure
;;;   rgb->value
;;; Parmeters:
;;;   col, an rgb color
;;; Purpose:
;;;   To return the value (as in the V in (HSV) of the color.
;;; Produces:
;;;   value, a float between 0 and 1.
;;; Preconditions
;;;   color must be a valid rgb color.
;;; Postconditions
;;;    After multiplying by 100 and rounding, value should correspond to 
;;;    the Gimp's value while examining color.
(define _rgb->value
  (lambda (col)
    (let ((color (color->rgb-list col)))
      (/ (apply max color) 255))))

(define rgb->value (guard-rgb-proc 'rgb->value _rgb->value))

; [From mscm/rgb/rgb-random.scm]

;;; Procedure:
;;;   rgb-random
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Generate an unpredictable RGB color
;;; Produces:
;;;   rgb, an RGB color
;;; Postconditions:
;;;   Every rgb color is equally likely.
(define rgb-random
  (lambda ()
    (rgb-new (random 256) (random 256) (random 256))))

; [From mscm/rgb/rgb-random-websafe.scm]

;;; Procedure:
;;;   rgb-random-websafe
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Produces an unpredictable "web safe" color.
;;; Produces:
;;;   rgb, and RGB color
;;; Postconditions:
;;;   rgb is a "web safe" color.  (Formally, each component is a multiple of 
;;;   51.  See the reading on pallettes for more details.)
(define rgb-random-websafe
  (lambda ()
    (rgb-new (* 51 (random 6)) (* 51 (random 6)) (* 51 (random 6)))))

; [From mscm/rgb/rgb-random-grey.scm]

;;; Procedure:
;;;   rgb-random-grey
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Choose an unpredictable shade of grey.
;;; Produces:
;;;   grey, a color
;;; Postconditions:
;;;   grey is a shade of grey.
;;;   It is difficult to predict what shade it is.
;;; Philosophy:
;;;   We consider white and black shades of grey, too.
(define rgb-random-grey
  (lambda ()
    (let ((component (random 255)))
      (rgb-new component component component))))


; +-----------------------------------------------------------+-----------------
; | Representing Colors as Lists of the form (Red Green Blue) |
; +-----------------------------------------------------------+

; [From mscm/rgb-list/rgb-list-to-rgb.scm]

;;; Procedure:
;;;   rgb-list->rgb
;;; Parameters:
;;;   color, an rgb-list
;;; Purpose:
;;;   Convert color to an rgb color.
;;; Preconditions:
;;;   color must be an rgb-list.  That is, it must be a list of three
;;;     integers, all in the range [0..255].
;;; Postconditions:
;;;   rgb represents the same color as color.
(define rgb-list->rgb
  (lambda (color)
    (rgb-new (car color) (cadr color) (caddr color))))

; [From mscm/rgb-list/rgb-list-p.scm]

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
(define rgb-list?
  (lambda (val)
    (and (list? val) (equal? (length val) 3) (all-integer? val))))


; +-----------------------------------+-----------------------------------------
; | Representing Images as "Drawings" |
; +-----------------------------------+

; Sometimes it is helpful to represent images as collections of basic
; drawing primitives (rectangles, ellipses, etc.).  These routines
; provide some support for that representation.  Different applications
; can then provide alternative mechanisms for rendering these drawings
; as images.

; [From mscm/drawing/drawing-types.scm]

;;; Value:
;;;   drawing-types
;;; Type:
;;;   List of symbols
;;; Description:
;;;   All of the valid types of drawings
(define drawing-types
  (list 'blank 'ellipse 'group 'line 'rectangle 'rule))

; [From mscm/drawing/drawing-format.scm]

;;; Procedure:
;;;   drawing-format
;;; Parameters:
;;;   type, a valid drawing type
;;; Purpose:
;;;   Shows the format of drawing values of the same type as drawing
;;; Produces:
;;;   format, a list
(define _drawing-format
  (lambda (type)
    (cond
      ((eq? type 'blank)
       '(drawing blank))
      ((eq? type 'group)
       '(drawing group (drawing1 drawing2)))
      ((eq? type 'elllipse)
       '(drawing ellipse color brush left top width height))
      ((eq? type 'line)
       '(drawing line color col1 row col2 row2 hscale vscale))
      ((eq? type 'rectangle)
       '(drawing rectangle color brush left top width height))
      ((eq? type 'rule)
       '(drawing rule color col1 row1 col2 row2))
      (else
       (error "drawing-format: unknown type of drawing: " type)))))

(define drawing-format _drawing-format)

; [From mscm/drawing/drawing-p.scm]

;;; Procedure:
;;;   drawing?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;    Determine whether val can be appropriately interpreted as a
;;;   drawing
;;; Produces:
;;;   is-drawing?, a Boolean
(define drawing? 
  (lambda (val)
    (or (drawing-blank? val)
        (drawing-group? val)
        (drawing-line? val)
        (drawing-rule? val)
        (drawing-shape? val))))

; [From mscm/drawing/guard-drawing-proc.scm]

;;; Procedure:
;;;   guard-drawing-proc
;;; Parameters:
;;;   name, a symbol
;;;   proc, a procedure of the form (lambda (drawing) ___)
;;; Purpose:
;;;   Build a new version of proc that checks preconditions.
;;; Produces:
;;;   guarded, a procedure of the form (lambda (drawing) _____)
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If (drawing? val), (guarded val) = (proc val)
;;;   Otherwise, (guarded val) throws an appropriate error
(define guard-drawing-proc
  (lambda (name proc)
    (guard-unary-proc name proc 'drawing drawing?)))

; [From mscm/drawing/drawing-type.scm]

;;; Procedure:
;;;   drawing-type
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the type of drawing.
;;; Produces:
;;;   type, a symbol.
(define _drawing-type cadr)

(define drawing-type
  (guard-drawing-proc 'drawing-type _drawing-type))

; [From mscm/drawing/drawing-info.scm]

;;; Procedure:
;;;   drawing-info
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Generate a list of basic information about drawing
;;; Produces:
;;;   info, a list
(define _drawing-info
  (lambda (drawing)
    (list 'type (_drawing-type drawing)
          'color (_drawing-color drawing)
          'left (_drawing-left drawing)
          'top (_drawing-top drawing)
          'right (_drawing-right drawing)
          'bottom (_drawing-bottom drawing)
          'width (_drawing-width drawing)
          'height (_drawing-height drawing)
          )))

(define drawing-info (guard-drawing-proc 'drawing-info _drawing-info))

; [From mscm/drawing/drawing-type-p.scm]

;;; Procedure:
;;;   drawing-type?
;;; Parameters:
;;;   type, a symbol
;;; Purpose:
;;;   Determine if type is a valid drawing type
;;; Produces:
;;;   is-drawing-type?, a Boolean
(define _drawing-type?
  (r-s member? drawing-types))

(define drawing-type? _drawing-type?)

; +--------------------+--------------------------------------------------------
; | Drawing Predicates |
; +--------------------+

; [From mscm/drawing/drawing-blank-p.scm]

;;; Procedure:
;;;   drawing-blank?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val is a  blank drawing.
;;; Produces:
;;;   is-blank?, a Boolean
(define _drawing-blank?
  (lambda (val)
    (check-list? (list (l-s eq? 'drawing) 
                       (l-s eq? 'blank))
                 val)))

(define drawing-blank? _drawing-blank?)

; [From mscm/drawing/drawing-group-p.scm]

;;; Procedure:
;;;   drawing-group?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val is a drawing-group
;;; Produces:
;;;   is-group?, a Boolean
(define _drawing-group?
  (lambda (val)
    (check-list? (list (l-s eq? 'drawing) 
                       (l-s eq? 'group)
                       (l-s all drawing?))
                 val)))

(define drawing-group? _drawing-group?)

; [From mscm/drawing/drawing-line-p.scm]

;;; Procedure:
;;;   drawing-line?
;;; Parameters:
;;;   val, a value
;;; Purpose:
;;;   Determine if val can be interpreted as a line in a drawing
;;; Produces:
;;;   is-line?, a boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If val can be used with the various drawing-line-PROC procedures,
;;;     is-line? is true.
;;;   Otherwise is-line? is false.
(define _drawing-line?
  (let* ((nnr? (^and real? (^not negative?))))
    (lambda (val)
      (check-list? (list (l-s eq? 'drawing) (l-s eq? 'line)
                         color?
                         real? real? real? real?
                         nnr? nnr?)
                   val))))

(define drawing-line? _drawing-line?)

; [From mscm/drawing/drawing-rule-p.scm]

;;; Procedure:
;;;   drawing-rule?
;;; Parameters:
;;;   val, a value
;;; Purpose:
;;;   Determine if val can be interpreted as a rule in a drawing
;;; Produces:
;;;   is-rule?, a boolean
(define _drawing-rule?
  (let* ((nnr? (^and real? (^not negative?)))
         (types (list (l-s eq? 'drawing) (l-s eq? 'rule)
                      real? real? real? real?)))
    (lambda (val)
      (check-list? (list (l-s eq? 'drawing) (l-s eq? 'rule)
                         color?
                         real? real? real? real?)
                   val))))

(define drawing-rule? _drawing-rule?)

; [From mscm/drawing/drawing-shape-p.scm]

;;; Procedure:
;;;   drawing-shape?
;;; Parameters:
;;;   val, a value
;;; Purpose:
;;;   Determine if val is one of the drawing shapes
;;; Produces:
;;;   is-shape?, a Boolean
(define _drawing-shape?
  (let* ((nnr? (^and real? (^not negative?))))
    (lambda (val)
      (check-list? (list (l-s eq? 'drawing) 
                         (r-s member '(ellipse rectangle))
                         color?         ; color
                         string?        ; brush
                         real?          ; left
                         real?          ; right
                         nnr?           ; width
                         nnr?           ; height
                         )
                    val))))

(define drawing-shape? _drawing-shape?)

; [From mscm/drawing/drawing-ellipse-p.scm]

;;; Procedure:
;;;   drawing-ellipse?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;    Determine whether val can be appropriately interpreted as a
;;;   rectagular drawing (e.g., built by scaling, shifting, and/or
;;;   recoloring the unit circle).
;;; Produces:
;;;   is-drawing?, a Boolean
(define _drawing-ellipse?
  (^and drawing-shape? (o (l-s eq? 'ellipse) drawing-type)))

(define drawing-ellipse? _drawing-ellipse?)

; [From mscm/drawing/drawing-rectangle-p.scm]

;;; Procedure:
;;;   drawing-rectangle?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;    Determine whether val can be appropriately interpreted as a
;;;   rectagular drawing (e.g., built by scaling, shifting, and/or
;;;   recoloring the unit square).
;;; Produces:
;;;   is-drawing?, a Boolean
(define _drawing-rectangle?
  (^and drawing-shape? (o (l-s eq? 'rectangle) drawing-type)))

(define drawing-rectangle? _drawing-rectangle?)


; +---------------------------+-------------------------------------------------
; | Core Drawing Constructors |
; +---------------------------+

; [From mscm/drawing/drawing-blank.scm]

;;; Value:
;;;   drawing-blank
;;; Description:
;;;   A blank drawing.  Included for the sake of completeness, and (more
;;;   importantly) to provide a base case for recursion.
(define drawing-blank (list 'drawing 'blank))

; [From mscm/drawing/drawing-line-core.scm]

;;; Procedure:
;;;   drawing-line-core
;;; Parameters:
;;;   color, a color
;;;   c1, a real
;;;   r1, a real
;;;   c2, a real
;;;   r2, a real
;;;   hstretch, a real
;;;   vstretch, a real
;;; Purpose:
;;;   Create a 'line" from (c1,r1) to (c2,r2)
;;; Produces:
;;;   line, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   When rendered, line will be a line from (c1,r1) to (c2,r2),
;;;     in color color, and "stretched" horizontally by hstretch
;;;     and vertically by vstretch
(define _drawing-line-core
  (lambda (color c1 r1 c2 r2 hstretch vstretch)
    (list 'drawing 'line                     ; 0, 1
           color                             ; 2
           c1 r1 c2 r2                       ; 3, 4, 5, 6
           (abs hstretch) (abs vstretch))))  ; 7, 8

(define drawing-line-core
  (lambda params
    (validate-params! 'drawing-line-core
                      (list 'color 'real 'real 'real 'real 'real 'real)
                      (list color? real? real? real? real? real? real?)
                      params)
    (apply _drawing-line-core params)))

; [From mscm/drawing/drawing-rule-core.scm]

;;; Procedure:
;;;   drawing-rule-core
;;; Parameters:
;;;   color, a color
;;;   c1, a real
;;;   r1, a real
;;;   c2, a real
;;;   r2, a real
;;; Purpose:
;;;   Creates a drawing of a rule from (c1,r1) to (c2,r2)
;;; Produces:
;;;   rule, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   When rendered, rule is a rule from (c1,r1) to (c2,r2)
(define _drawing-rule-core
  (lambda (color c1 r1 c2 r2)
    (list 'drawing 'rule color c1 r1 c2 r2)))

(define drawing-rule-core
  (lambda params
    (validate-params! 'drawing-rule-core
                      (list 'color 'real 'real 'real 'real)
                      (list color? real? real? real? real?)
                      params)
    (apply _drawing-rule-core params)))

; [From mscm/drawing/drawing-shape.scm]

;;; Procedure:
;;;   drawing-shape
;;; Parameters:
;;;   type, a symbol
;;;   color, a color
;;;   brush, a string
;;;   left, a real
;;;   top, a real
;;;   width, a real
;;;   height, a real
;;; Purpose:
;;;   Create one of the shape drawings
;;; Produces:
;;;   shape, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (drawing-type drawing) = type
;;;   (drawing-color drawing) = color
;;;   (drawing-brush drawing) = brush
;;;   (drawing-left drawing) = left
;;;   (drawing-top drawing) = top
;;;   (drawing-width drawing) = width
;;;   (drawing-height drawing) = height
(define _drawing-shape
  (lambda (type color brush left top width height)
    (list 'drawing type color brush left top width height)))

(define drawing-shape
  (lambda params
    (validate-params! 'drawing-shape
                      (list 'symbol 'color 'brush 'real 'real 'real 'real)
                      (list symbol? color? string? real? real? real? real?)
                      params)
    (apply _drawing-shape params)))

; [From mscm/drawing/drawing-group.scm]

;;; Procedure:
;;;   drawing-group
;;; Parameters:
;;;   drawing1 ... drawingn, n drawings 
;;; Purpose:
;;;   Create a new drawing by composing drawing1 ... drawingn.
;;; Produces:
;;;   grouped, a drawing
;;; Preconditions:
;;;   There is at least one parameter.
(define _drawing-group
  (lambda drawings
    (list 'drawing 'group drawings)))

(define drawing-group
  (lambda drawings
    (cond
      ((null? drawings)
       (error "drawing-group: expects at least one parameter"))
      ((not (all drawing? drawings))
       (error "drawing-group: called with at least one non-drawing"))
      (else (apply _drawing-group drawings)))))


; +---------------------------------+-------------------------------------------
; | Additional Drawing Constructors |
; +---------------------------------+

; [From mscm/drawing/drawing-compose.scm]

;;; Procedure:
;;;   drawing-compose
;;; Parameters:
;;;   drawings, a list of drawings
;;; Purpose:
;;;   Create a new drawing by composing the drawings in drawings
;;; Produces:
;;;   composed, a drawing
;;; Preconditions:
;;;   (length drawings) >= 1
(define _drawing-compose 
  (lambda (drawings)
    (apply _drawing-group drawings)))

(define drawing-compose
  (lambda (drawings)
    (cond
      ((null? drawings)
       (error "drawing-compose: expects at least one parameter"))
      ((not (all drawing? drawings))
       (error "drawing-compose: called with at least one non-drawing"))
      (else (_drawing-compose drawings)))))

; [From mscm/drawing/drawing-circle.scm]

;;; Procedure:
;;;   drawing-circle
;;; Purpose:
;;;   Create a new drawing that represents a circle.
;;; Parameters:
;;;   col, a real number
;;;   row, a real number
;;;   radius, a positive real number
;;; Produces:
;;;   circle, a drawing.
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   circle will be rendered as a circle of radius radius.
(define _drawing-circle
  (lambda (col row radius)
    (drawing-ellipse (- col radius) (- row radius) 
                     (* 2 radius) (* 2 radius))))

(define drawing-circle
  (guard-proc 'drawing-circle
              _drawing-circle
              (list 'real 'real 'positive-real)
              (list real? real? (^and real? positive?))))

; [From mscm/drawing/drawing-ellipse.scm]

;;; Procedure:
;;;   drawing-ellipse
;;; Purpose:
;;;   Create a new drawing that represents an ellipse
;;; Parameters:
;;;   left, a real
;;;   top, a real
;;;   width, a postitive real
;;;   height, a positive real
;;; Produces:
;;;   ellipse, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   When rendered, ellipse will be drawn as a filled ellipse, 
;;;   with the specified left margin, top margin, width, and
;;;   height.
(define _drawing-ellipse
  (lambda (left right width height)
    (_drawing-shape 'ellipse (rgb-new 0 0 0) "" left right width height)))

(define drawing-ellipse
  (guard-proc 'drawing-ellipse
              _drawing-ellipse
              (list 'real 'real 'positive-real 'positive-real)
              (list real? real? (^and real? positive?) (^and real? positive?))))

; [From mscm/drawing/drawing-grid.scm]

;;; Procedure:
;;;   drawing-grid
;;; Parameters:
;;;   width, an integer
;;;   height, an integer
;;; Purpose:
;;;   Create a drawing that represents a simple grid.
;;; Produces:
;;;   grid, a grid
(define drawing-grid
  (lambda (width height)
    (drawing-group (apply drawing-group
                          (map (lambda (col) (drawing-rule col 0 col height))
                               (iota (+ 1 width))))
                   (apply drawing-group
                          (map (lambda (row) (drawing-rule 0 row width row))
                               (iota (+ 1 height)))))))

; [From mscm/drawing/drawing-join.scm]

;;; Procedure:
;;;   drawing-join
;;; Parameters:
;;;   drawing1, a drawing
;;;   drawing2, a drawing
;;; Purpose:
;;;   Create a new drawing by joinging drawing1 and drawing2.
;;; Produces:
;;;   joined, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   When rendered, joined is equivalent to rendering drawing1 and
;;;   then drawing 2
(define _drawing-join
  (lambda (drawing1 drawing2)
    (drawing-group drawing1 drawing2)))

(define drawing-join
  (guard-proc 'drawing-join
              _drawing-join
              (list 'drawing 'drawing)
              (list drawing? drawing?)))

; [From mscm/drawing/drawing-line.scm]

;;; Procedure:
;;;   drawing-line
;;; Parameters:
;;;   c1, a real
;;;   r1, a real
;;;   c2, a real
;;;   r2, a real
;;; Purpose:
;;;   Create a drawing of a black line from (c1,r1) to (c2,r2)
;;; Produces:
;;;   line, a drawing
(define _drawing-line
  (lambda (c1 r1 c2 r2)
    (let* ((delta-col (- c2 c1))
           (delta-row (- r2 r1))
           (dist (sqrt (+ (square delta-col) (square delta-row)))))
      (drawing-line-core (rgb-new 0 0 0)
                         c1 r1 c2 r2
                         (abs (/ delta-row dist))
                         (abs (/ delta-col dist))))))

(define drawing-line
  (guard-proc 'drawing-line
              _drawing-line
              (list 'real 'real 'real 'real)
              (list real? real? real? real?)))

; [From mscm/drawing/drawing-rectangle.scm]

;;; Procedure:
;;;   drawing-rectangle
;;; Purpose:
;;;   Create a new drawing that represents an rectangle
;;; Parameters:
;;;   left, a real number
;;;   top, a real number
;;;   width, a positive real number
;;;   height, a positive real number
;;; Produces:
;;;   rectangle, a drawing
;;; Preconditions:
;;;   width > 0.
;;;   height > 0.
;;; Postconditions:
;;;   When rendered, rectangle will be drawn as a filled rectangle, 
;;;   with the specified left margin, top margin, width, and
;;;   height.
(define _drawing-rectangle
  (lambda (left right width height)
    (_drawing-shape 'rectangle (rgb-new 0 0 0) "" left right width height)))

(define drawing-rectangle
  (guard-proc 'drawing-rectangle
              _drawing-rectangle
              (list 'real 'real 'positive-real 'positive-real)
              (list real? real? 
                    (^and real? positive?) (^and real? positive?))))

; [From mscm/drawing/drawing-rule.scm]

;;; Procedure:
;;;   drawing-rule
;;; Parameters:
;;;   color, a color
;;;   c1, a real
;;;   r1, a real
;;;   c2, a real
;;;   r2, a real
;;; Purpose:
;;;   Creates a drawing of a rule form (c1,r1) to (c2,r2)
;;; Produces:
;;;   rule, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   When rendered, rule is a rule from (c1,r1) to (c2,r2)
(define _drawing-rule
  (lambda (c1 r1 c2 r2)
    (drawing-rule-core (rgb-new 0 0 0) c1 r1 c2 r2)))

(define drawing-rule
  (guard-proc 'drawing-rule
              _drawing-rule
              (list 'real 'real 'real 'real)
              (list real? real? real? real?)))

; [From mscm/drawing/drawing-unit-circle.scm]

;;; Value:
;;;   drawing-unit-circle
;;; Description:
;;;   A circle with diameter 1, centered at 0,0
(define drawing-unit-circle (drawing-ellipse -0.5 -0.5 1 1))

; [From mscm/drawing/drawing-unit-square.scm]

;;; Value:
;;;   drawing-unit-square
;;; Description:
;;;   A circle with edge length 1, centered at 0,0
(define drawing-unit-square (drawing-rectangle -0.5 -0.5 1 1))


; +-------------------+---------------------------------------------------------
; | Drawing Accessors |
; +-------------------+

; [From mscm/drawing/drawing-bottom.scm]

;;; Procedure:
;;;   drawing-bottom
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Find the bottom edge of drawing
;;; Produces:
;;;   bottom, a real
(define _drawing-bottom
  (lambda (drawing)
    (let ((type (_drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         0)
        ((eq? type 'group)
         (apply max (map _drawing-bottom (_drawing-members drawing))))
        ((eq? type 'line)
         (_drawing-line-bottom drawing))
        ((eq? type 'rule)
         (_drawing-rule-bottom drawing))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (_drawing-shape-bottom drawing))
        (else
         (error "drawing-bottom: Unknown drawing type: " type))))))

(define drawing-bottom
  (guard-drawing-proc 'drawing-bottom _drawing-bottom))

; [From mscm/drawing/drawing-brush.scm]

;;; Procedure:
;;;   drawing-brush [DEPRECATED]
;;; Parameter:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the brush associated with the drawing
;;; Produces:
;;;   brush, a string
(define _drawing-brush
  (lambda (drawing)
    (let ((type (_drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         "")
        ((eq? type 'group)
         "")
        ((eq? type 'line)
         "")
        ((eq? type 'rule)
         "")
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (_drawing-shape-brush drawing))
        (else
         (error "drawing-brush: unknown drawing type" type))))))

(define drawing-brush
  (guard-drawing-proc 'drawing-brush _drawing-brush))

; [From mscm/drawing/drawing-color.scm]

;;; Procedure:
;;;   drawing-color
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the color of drawing.
;;; Produces:
;;;   type, a color
(define _drawing-color 
  (lambda (drawing)
    (let ((type (_drawing-type drawing)))
      (cond 
        ((eq? type 'blank)
         rgb-transparent)
        ((eq? type 'group)
         (rgb-new 0 0 0))
        ((eq? type 'line)
         (_drawing-line-color drawing))
        ((eq? type 'rule)
         (_drawing-rule-color drawing))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (_drawing-shape-color drawing))
        (else
         (error "drawing-color: unknown drawing type" type))))))

(define drawing-color
  (guard-drawing-proc 'drawing-color _drawing-color))

; [From mscm/drawing/drawing-filled-p.scm]

;;; Procedure:
;;;   drawing-filled?
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Determine if drawing is filled.
;;; Produces:
;;;   filled?, a Boolean
(define _drawing-filled?
  (lambda (drawing)
    (let ((type (_drawing-type drawing)))
      (and (or (eq? type 'ellipse) (eq? type 'rectangle))
           (string=? "" (drawing-brush drawing))))))

(define drawing-filled? 
  (guard-drawing-proc 'drawing-filled? _drawing-filled?))

; [From mscm/drawing/drawing-height.scm]

;;; Procedure:
;;;   drawing-height
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the height of drawing.
;;; Produces:
;;;   height, a real
(define _drawing-height
  (lambda (drawing)
    (let ((type (_drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         0)
        ((eq? type 'group)
         (- (_drawing-bottom drawing) (_drawing-top drawing)))
        ((eq? type 'line)
         (_drawing-line-height drawing))
        ((eq? type 'rule)
         (_drawing-rule-height drawing))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (_drawing-shape-height drawing))
        (else
         (error "drawing-height: Unknown drawing type: " type))))))

(define drawing-height
  (guard-drawing-proc 'drawing-height _drawing-height))

; [From mscm/drawing/drawing-left.scm]

;;; Procedure:
;;;   drawing-left
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Find the left edge of drawing
;;; Produces:
;;;   left, a real
(define _drawing-left
  (lambda (drawing)
    (let ((type (_drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         0)
        ((eq? type 'group)
         (apply min (map _drawing-left (_drawing-members drawing))))
        ((eq? type 'line)
         (_drawing-line-left drawing))
        ((eq? type 'rule)
         (_drawing-rule-left drawing))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (_drawing-shape-left drawing))
        (else
         (error "drawing-left: Unknown drawing type: " type))))))

(define drawing-left
  (guard-drawing-proc 'drawing-left _drawing-left))

; [From mscm/drawing/drawing-members.scm]

;;; Procedure:
;;;   drawing-members
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get a list of all the sub-drawings in drawing.
;;; Produces:
;;;   sub-drawings, a list of drawings
(define _drawing-members
  (lambda (drawing)
    (if (eq? (_drawing-type drawing) 'group)
        (caddr drawing)
        (list drawing))))

(define drawing-members 
  (guard-drawing-proc 'drawing-members _drawing-members))

; [From mscm/drawing/drawing-right.scm]

;;; Procedure:
;;;   drawing-right
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Find the right edge of drawing
;;; Produces:
;;;   right, a real
(define _drawing-right
  (lambda (drawing)
    (let ((type (_drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         0)
        ((eq? type 'group)
         (apply max (map _drawing-right (_drawing-members drawing))))
        ((eq? type 'line)
         (_drawing-line-right drawing))
        ((eq? type 'rule)
         (_drawing-rule-right drawing))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (_drawing-shape-right drawing))
        (else
         (error "drawing-right: Unknown drawing type: " type))))))

(define drawing-right
  (guard-drawing-proc 'drawing-right _drawing-right))

; [From mscm/drawing/drawing-top.scm]

;;; Procedure:
;;;   drawing-top
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Find the top edge of drawing
;;; Produces:
;;;   top, a real
(define _drawing-top
  (lambda (drawing)
    (let ((type (_drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         0)
        ((eq? type 'group)
         (apply min (map drawing-top (_drawing-members drawing))))
        ((eq? type 'line)
         (_drawing-line-top drawing))
        ((eq? type 'rule)
         (_drawing-rule-top drawing))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (_drawing-shape-top drawing))
        (else
         (error "drawing-top: unknown drawing type: " type))))))

(define drawing-top
  (guard-drawing-proc 'drawing-top _drawing-top))

; [From mscm/drawing/drawing-width.scm]

;;; Procedure:
;;;   drawing-width
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the width of drawing
;;; Produces:
;;;   width, a real
(define _drawing-width
  (lambda (drawing)
    (let ((type (_drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         drawing)
        ((eq? type 'group)
         (- (_drawing-right drawing) (_drawing-left drawing)))
        ((eq? type 'line)
         (_drawing-line-width drawing))
        ((eq? type 'rule)
         (_drawing-rule-width drawing))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (_drawing-shape-width drawing))
        (else
         (error "drawing-width: Unknown drawing type: " type))))))

(define drawing-width
  (guard-drawing-proc 'drawing-width _drawing-width))


; +---------------------------+-------------------------------------------------
; | Building Drawing Variants |
; +---------------------------+

; [From mscm/drawing/drawing-fill.scm]

;;; Procedure:
;;;   drawing-fill [DEPRECATED]
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Create a new version of drawing that is filled.
;;; Produces:
;;;   filled, a drawing
;;; Postconditions:
;;;   filled is the same overall "shape" and color as drawing,
;;;   but filled, instead of outlined.
(define _drawing-fill
  (lambda (drawing)
    (let ((type (_drawing-type drawing)))
      (cond
        ((eq? type 'group)
         (apply _drawing-group
                (map drawing-fill (_drawing-members drawing))))
        ((or (eq? type 'line) (eq? type 'rule))
         drawing)
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-group type
                        (_drawing-shape-color drawing) 
                        ""
                        (_drawing-shape-left drawing) 
                        (_drawing-shape-top drawing)
                        (_drawing-shape-width drawing) 
                        (_drawing-shape-height drawing)))
        (else
         (error "drawing-fill: unknown drawing type" type))))))

(define drawing-fill
  (guard-drawing-proc 'drawing-fill _drawing-fill))

; [From mscm/drawing/drawing-hscale.scm]

;;; Procedure:
;;;   drawing-hscale
;;; Parameters:
;;;   drawing, a drawing
;;;   factor, a real number
;;; Purpose:
;;;   Create a new version of drawing that is horizontally scaled by the
;;;   specified factor.
;;; Produces:
;;;   scaled, a drawing
;;; Postconditions:
;;;   scaled is the same color as drawing, but the width is
;;;   scaled by factor (as is the left side).
(define _drawing-hscale
  (lambda (drawing factor)
    (let ((type (_drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         drawing)
        ((eq? type 'group)
         (apply drawing-group
                (map (r-s _drawing-hscale factor)
                     (_drawing-members drawing))))
        ((eq? type 'line)
         (drawing-line-hscale drawing factor))
        ((eq? type 'rule)
         (drawing-rule-core (_drawing-rule-color drawing)
                            (* factor (_drawing-rule-left drawing))
                            (_drawing-rule-top drawing)
                            (* factor (_drawing-rule-right drawing))
                            (_drawing-rule-bottom drawing)))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape type
                        (_drawing-color drawing) 
                        (_drawing-brush drawing)
                        (* factor (_drawing-left drawing))
                        (_drawing-top drawing)
                        (* factor (_drawing-width drawing)) 
                        (_drawing-height drawing)))
        (else
         (error "drawing-hscale: unknown drawing type" type))))))

(define drawing-hscale
  (guard-proc 'drawing-hscale
              _drawing-hscale
              (list 'drawing 'real)
              (list drawing? real?)))

; [From mscm/drawing/drawing-hshift.scm]

;;; Procedure:
;;;   drawing-hshift
;;; Parameters:
;;;   drawing, a drawing
;;;   amt, a real number
;;; Purpose:
;;;   Create a new version of drawing that is horizontally shifted 
;;;   by the specified amt.
;;; Produces:
;;;   shifted , a drawing
;;; Postconditions:
;;;   scaled is the same overall "shape", color, and size as
;;;   drawing, but shifted to the right by amt (or to the left
;;;   by |amt|, if amt is negative).
(define _drawing-hshift
  (lambda (drawing amt)
    (let ((type (_drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         drawing)
        ((eq? type 'group)
         (apply drawing-group
                (map (r-s drawing-hshift amt)
                     (drawing-members drawing))))
        ((eq? type 'line)
         (_drawing-line-hshift drawing amt))
        ((eq? type 'rule)
         (drawing-rule-core (_drawing-rule-color drawing)
                            (+ amt (_drawing-rule-left drawing))
                            (_drawing-rule-top drawing)
                            (+ amt (_drawing-rule-right drawing))
                            (_drawing-rule-bottom drawing)))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape type
                        (_drawing-shape-color drawing) 
                        (_drawing-shape-brush drawing)
                        (+ amt (_drawing-shape-left drawing)) 
                        (_drawing-shape-top drawing)
                        (_drawing-shape-width drawing) 
                        (_drawing-shape-height drawing)))
        (else
         (error "drawing-hshift: unknown drawing type" type))))))

(define drawing-hshift
  (guard-proc 'drawing-hshift
              _drawing-hshift
              (list 'drawing 'real)
              (list drawing? real?)))

; [From mscm/drawing/drawing-outline.scm]

;;; Procedure:
;;;   drawing-outline [DEPRECATED]
;;; Parameters:
;;;   drawing, a drawing
;;;   brush, a string
;;; Purpose:
;;;   Create a new version of drawing that is outlined by the given
;;;   brush.
;;; Produces:
;;;   outlined, a drawing
;;; Preconditions:
;;;   brush names a valid brush
;;; Postconditions:
;;;   outlined is the same overall "shape" and color as drawing,
;;;   but is outlined in the given brush (rather than being outlined
;;;   by another brush or filled).
(define _drawing-outline
  (lambda (drawing brush)
    (let ((type (_drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         drawing)
        ((eq? type 'group)
         (apply drawing-group
                (map (r-s drawing-outline brush)
                     (_drawing-members drawing))))
        ((or (eq? type 'line) (eq? type 'rule))
         drawing)
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape type
                        (_drawing-color drawing) brush
                        (_drawing-left drawing) (_drawing-top drawing)
                        (_drawing-width drawing) (_drawing-height drawing)))
        (else
         (error "drawing-outline: unknown drawing type" type))))))

(define drawing-outline
  (guard-proc 'drawing-outline
              _drawing-outline
              (list 'drawing 'string)
              (list drawing? string?)))

; [From mscm/drawing/drawing-recolor.scm]

;;; Procedure:
;;;   drawing-recolor
;;; Parameters:
;;;   drawing, a drawing
;;;   color, a color
;;; Purpose:
;;;   Create a new version of drawing that is colored by the
;;;   given color
;;; Produces:
;;;   recolored, a drawing
;;; Preconditions:
;;;   color is a valid color.
;;; Postconditions:
;;;   recolored is the same overall "shape" but is colored the given color.
(define _drawing-recolor
  (lambda (drawing color)
    (let ((type (_drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         drawing)
        ((eq? type 'group)
         (apply drawing-group
                (map (r-s drawing-recolor color)
                     (_drawing-members drawing))))
        ((eq? type 'line)
         (_drawing-line-recolor drawing color))
        ((eq? type 'rule)
         (drawing-rule-core color
                            (_drawing-rule-left drawing)
                            (_drawing-rule-top drawing)
                            (_drawing-rule-right drawing)
                            (_drawing-rule-bottom drawing)))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape type
                        (color->rgb color) 
                        (_drawing-brush drawing)
                        (_drawing-left drawing) 
                        (_drawing-top drawing)
                        (_drawing-width drawing) 
                        (_drawing-height drawing)))
        (else
         (error "drawing-recolor: unknown drawing type" type))))))

; Because color? may get redefined, we don't use guard-drawing-proc
(define drawing-recolor
  (lambda params
    (validate-params! 'drawing-recolor
                      (list 'drawing 'color)
                      (list drawing? color?)
                      params)
    (apply _drawing-recolor params)))


; [From mscm/drawing/drawing-scale.scm]

;;; Procedure:
;;;   drawing-scale
;;; Parameters:
;;;   drawing, a drawing
;;;   factor, a real number
;;; Purpose:
;;;   Create a new version of drawing that is scaled by the
;;;   specified factor.
;;; Produces:
;;;   scaled, a drawing
;;; Postconditions:
;;;   scaled is the same overall "shape" and color as drawing, but
;;;   is larger or smaller, based on the scale factor.
(define _drawing-scale
  (lambda (drawing factor)
    (let ((type (_drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         drawing)
        ((eq? type 'group)
         (apply drawing-group
                (map (r-s drawing-scale factor)
                     (_drawing-members drawing))))
        ((eq? type 'line)
         (_drawing-line-scale drawing factor))
        ((eq? type 'rule)
         (drawing-rule-core (_drawing-rule-color drawing)
                            (* factor (_drawing-rule-left drawing))
                            (* factor (_drawing-rule-top drawing))
                            (* factor (_drawing-rule-right drawing))
                            (* factor (_drawing-rule-bottom drawing))))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape type
                        (_drawing-color drawing) 
                        (_drawing-brush drawing)
                        (* factor (_drawing-left drawing))
                        (* factor (_drawing-top drawing))
                        (* factor (_drawing-width drawing)) 
                        (* factor (_drawing-height drawing))))
        (else
         (error "drawing-scale: unknown drawing type" type))))))

(define drawing-scale
  (guard-proc 'drawing-scale
              _drawing-scale
              (list 'drawing 'real)
              (list drawing? real?)))

; [From mscm/drawing/drawing-shift.scm]

;;; Procedure:
;;;   drawing-shift
;;; Parameters:
;;;   drawing, a drawing
;;;   h-offset, a real
;;;   v-offset, a real
;;; Purpose:
;;;   Shift drawing horizontally by h-offset and vertically by
;;;   v-offset.
;;; Produces:
;;;   shifted, a drawing
(define _drawing-shift
  (lambda (drawing h-offset v-offset)
    (drawing-hshift (drawing-vshift drawing v-offset)
                    h-offset)))

(define drawing-shift
  (guard-proc 'drawing-shift 
              _drawing-shift
              (list 'drawing 'real 'real)
              (list drawing? real? real?)))

; [From mscm/drawing/drawing-vscale.scm]

;;; Procedure:
;;;   drawing-vscale
;;; Parameters:
;;;   drawing, a drawing
;;;   factor, a number
;;; Purpose:
;;;   Create a new version of drawing that is vertically scaled by the
;;;   specified factor.
;;; Produces:
;;;   scaled, a drawing
;;; Postconditions:
;;;   scaled has the same width, color, and form (filled or outlined)
;;;   as drawing, but the height is scaled by factor (as is the top
;;;   margin).
(define _drawing-vscale
  (lambda (drawing factor)
    (let ((type (_drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         drawing)
        ((eq? type 'group)
         (apply drawing-group 
                (map (r-s _drawing-vscale factor)
                     (_drawing-members drawing))))
        ((eq? type 'line)
         (_drawing-line-vscale drawing factor))
        ((eq? type 'rule)
         (drawing-rule-core (_drawing-rule-color drawing)
                            (_drawing-rule-left drawing)
                            (* factor (_drawing-rule-top drawing))
                            (_drawing-rule-right drawing)
                            (* factor (_drawing-rule-bottom drawing))))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape type
                        (_drawing-color drawing) 
                        (_drawing-brush drawing)
                        (_drawing-left drawing)
                        (* factor (_drawing-top drawing))
                        (_drawing-width drawing)
                        (* factor (_drawing-height drawing))))
        (else
         (error "drawing-vscale: unknown drawing type" type))))))

(define drawing-vscale
  (guard-proc 'drawing-vscale
              _drawing-vscale
              (list 'drawing 'real)
              (list drawing? real?)))

; [From mscm/drawing/drawing-vshift.scm]

;;; Procedure:
;;;   drawing-vshift
;;; Parameters:
;;;   drawing, a drawing
;;;   amt, a number
;;; Purpose:
;;;   Create a new version of drawing that is shifted vertically by
;;;   the specified amt.
;;; Produces:
;;;   shifted , a drawing
;;; Postconditions:
;;;   scaled is the same overall "shape", color, and size as
;;;   drawing, but shifted down by amt (or up by |amt|, if 
;;;   amt is negative).
(define _drawing-vshift
  (lambda (drawing amt)
    (let ((type (_drawing-type drawing)))
      (cond
        ((eq? type 'blank)
         drawing)
        ((eq? type 'group)
         (apply drawing-group
                (map (r-s _drawing-vshift amt)
                     (_drawing-members drawing))))
        ((eq? type 'line)
         (_drawing-line-vshift drawing amt))
        ((eq? type 'rule)
         (drawing-rule-core (_drawing-rule-color drawing)
                            (_drawing-rule-left drawing)
                            (+ amt (_drawing-rule-top drawing))
                            (_drawing-rule-right drawing)
                            (+ amt (_drawing-rule-bottom drawing))))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (drawing-shape type
                        (_drawing-color drawing) 
                        (_drawing-brush drawing)
                        (_drawing-left drawing)
                        (+ amt (_drawing-top drawing))
                        (_drawing-width drawing) 
                        (_drawing-height drawing)))
        (else
         (error "drawing-vshift: unknown drawing type" type))))))

(define drawing-vshift
  (guard-proc 'drawing-vshift
              _drawing-vshift
              (list 'drawing 'real)
              (list drawing? real?)))


; +---------------------------------------+-------------------------------------
; | Basic Shapes: Ellipses and Rectangles |
; +---------------------------------------+

; [From mscm/drawing/drawing-shape-bottom.scm]

;;; Procedure:
;;;   drawing-shape-bottom
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the bottom edge of the drawing
;;; Produces:
;;;   bottom, a real
;;; Preconditions:
;;;   (drawing-shape? drawing)
(define _drawing-shape-bottom 
  (lambda (drawing)
    (+ (_drawing-shape-top drawing) (_drawing-shape-height drawing))))

(define drawing-shape-bottom 
  (guard-unary-proc 'drawing-shape-bottom 
                    _drawing-shape-bottom
                    'drawing-shape 
                    drawing-shape?))

; [From mscm/drawing/drawing-shape-brush.scm]

;;; Procedure:
;;;   drawing-shape-brush 
;;; Parameter:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the brush associated with the drawing
;;; Produces:
;;;   brush, a string
;;; Preconditions:
;;;   (drawing-shape? drawing)
(define _drawing-shape-brush (r-s list-ref 3))

(define drawing-shape-brush
  (guard-unary-proc 'drawing-shape-brush
                    _drawing-shape-brush
                    'drawing-shape
                    drawing-shape?))

; [From mscm/drawing/drawing-shape-color.scm]

;;; Procedure:
;;;   drawing-shape-color 
;;; Parameter:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the color associated with the drawing
;;; Produces:
;;;   color, a color
;;; Preconditions:
;;;   (drawing-shape? drawing)
(define _drawing-shape-color (r-s list-ref 2))

(define drawing-shape-color
  (guard-unary-proc 'drawing-shape-color
                    _drawing-shape-color
                    'drawing-shape
                    drawing-shape?))

; [From mscm/drawing/drawing-shape-height.scm]

;;; Procedure:
;;;   drawing-shape-height
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the height of the drawing
;;; Produces:
;;;   height, a real
;;; Preconditions:
;;;   (drawing-shape? drawing)
(define _drawing-shape-height  (r-s list-ref 7))

(define drawing-shape-height 
  (guard-unary-proc 'drawing-shape-height 
                    _drawing-shape-height
                    'drawing-shape 
                    drawing-shape?))

; [From mscm/drawing/drawing-shape-left.scm]

;;; Procedure:
;;;   drawing-shape-left
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the left edge of the shape
;;; Produces:
;;;   left, a real
;;; Preconditions:
;;;   (drawing-shape? drawing)
(define _drawing-shape-left  (r-s list-ref 4))

(define drawing-shape-left 
  (guard-unary-proc 'drawing-shape-left 
                    _drawing-shape-left
                    'drawing-shape 
                    drawing-shape?))

; [From mscm/drawing/drawing-shape-right.scm]

;;; Procedure:
;;;   drawing-shape-right
;;; Parameters:
;;;   drawing, a drawing shape
;;; Purpose:
;;;   Grab the right edge of the shape
;;; Produces:
;;;   right, a real
;;; Preconditions:
;;;   (drawing-shape? drawing)
(define _drawing-shape-right 
  (lambda (drawing)
    (+ (_drawing-shape-left drawing) (_drawing-shape-width drawing))))

(define drawing-shape-right 
  (guard-unary-proc 'drawing-shape-right 
                    _drawing-shape-right
                    'drawing-shape 
                    drawing-shape?))

; [From mscm/drawing/drawing-shape-top.scm]

;;; Procedure:
;;;   drawing-shape-top
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the top edge of the drawing
;;; Produces:
;;;   top, a real
;;; Preconditions:
;;;   (drawing-shape? drawing)
(define _drawing-shape-top  (r-s list-ref 5))

(define drawing-shape-top 
  (guard-unary-proc 'drawing-shape-top 
                    _drawing-shape-top
                    'drawing-shape 
                    drawing-shape?))

; [From mscm/drawing/drawing-shape-width.scm]

;;; Procedure:
;;;   drawing-shape-width
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the width of the drawing
;;; Produces:
;;;   width, a real
;;; Preconditions:
;;;   (drawing-shape? drawing)
(define _drawing-shape-width  (r-s list-ref 6))

(define drawing-shape-width 
  (guard-unary-proc 'drawing-shape-width 
                    _drawing-shape-width
                    'drawing-shape 
                    drawing-shape?))


; +-------+---------------------------------------------------------------------
; | Lines |
; +-------+

; [From mscm/drawing/drawing-line-bottom.scm]

;;; Procedure:
;;;   drawing-line-bottom
;;; Parameters:
;;;   line, a drawing line
;;; Purpose:
;;;   Get the bottom edge of the drawing
;;; Produces:
;;;   bottom, a real
(define _drawing-line-bottom 
  (lambda (line)
    (max (list-ref line 4) (list-ref line 6))))

(define drawing-line-bottom 
  (guard-unary-proc 'drawing-line-bottom 
                    _drawing-line-bottom
                    'drawing-line 
                    drawing-line?))

; [From mscm/drawing/drawing-line-color.scm]

;;; Procedure:
;;;   drawing-line-color
;;; Parameters:
;;;   line, a drawing line
;;; Purpose:
;;;   Get the color of the line
;;; Produces:
;;;   color, a color
(define _drawing-line-color 
  (r-s list-ref (list-index (drawing-format 'line) 'color)))

(define drawing-line-color 
  (guard-unary-proc 'drawing-line-color 
                    _drawing-line-color
                    'drawing-line 
                    drawing-line?))

; [From mscm/drawing/drawing-line-height.scm]

;;; Procedure:
;;;   drawing-line-height
;;; Parameters:
;;;   line, a drawing line
;;; Purpose:
;;;   Get the height of the drawing
;;; Produces:
;;;   height, a real
(define _drawing-line-height
  (lambda (line)
    (abs (- (list-ref line 4) (list-ref line 6)))))

(define drawing-line-height 
  (guard-unary-proc 'drawing-line-height 
                    _drawing-line-height
                    'drawing-line 
                    drawing-line?))

; [From mscm/drawing/drawing-line-hscale.scm]

;;; Procedure:
;;;   drawing-line-hscale
;;; Parameters:
;;;   line, a drawing line
;;:   factor, a real number
;;; Purpose:
;;;   Create a new version of line, scaled horizontally by factor
;;; Produces:
;;;   scaled, a drawing
(define _drawing-line-hscale
  (lambda (line factor)
    (drawing-line-core (_drawing-line-color line)
                       (* factor (_drawing-line-left line))
                       (_drawing-line-top line)
                       (* factor (_drawing-line-right line))
                       (_drawing-line-bottom line)
                       (* factor (_drawing-line-hstretch line))
                       (_drawing-line-vstretch line))))

(define drawing-line-hscale
  (guard-proc 'drawing-line-hscale
              _drawing-line-hscale
              (list 'drawing-line 'real)
              (list drawing-line? real?)))

; [From mscm/drawing/drawing-line-hshift.scm]

;;; Procedure:
;;;   drawing-line-hshift
;;; Parameters:
;;;   line, a drawing line
;;:   amt, a real number
;;; Purpose:
;;;   Create a new version of line, shifted horizontally by amt
;;; Produces:
;;;   shifted, a drawing
(define _drawing-line-hshift
  (lambda (line amt)
    (drawing-line-core (_drawing-line-color line)
                       (+ amt (_drawing-line-left line))
                       (_drawing-line-top line)
                       (+ amt (_drawing-line-right line))
                       (_drawing-line-bottom line)
                       (_drawing-line-hstretch line)
                       (_drawing-line-vstretch line))))

(define drawing-line-hshift
  (guard-proc 'drawing-line-hshift
              _drawing-line-hshift
              (list 'drawing-line 'real)
              (list drawing-line? real?)))

; [From mscm/drawing/drawing-line-hstretch.scm]

;;; Procedure:
;;;   drawing-line-hstretch
;;; Parameters:
;;;   line, a drawing line
;;; Purpose:
;;;   Get the horizontal "stretch" of the line
;;; Produces:
;;;   stretch, a real
(define _drawing-line-hstretch 
  (r-s list-ref 7))

(define drawing-line-hstretch 
  (guard-unary-proc 'drawing-line-hstretch 
                    _drawing-line-hstretch
                    'drawing-line 
                    drawing-line?))

; [From mscm/drawing/drawing-line-left.scm]

;;; Procedure:
;;;   drawing-line-left
;;; Parameters:
;;;   line, a drawing line
;;; Purpose:
;;;   Get the left edge of the drawing
;;; Produces:
;;;   left, a real
;;; Preconditions:
;;;   (drawing-line? line)
(define _drawing-line-left 
  (lambda (line)
    (min (list-ref line 3) (list-ref line 5))))

(define drawing-line-left 
  (guard-unary-proc 'drawing-line-left 
                    _drawing-line-left
                    'drawing-line 
                    drawing-line?))

; [From mscm/drawing/drawing-line-recolor.scm]

;;; Procedure:
;;;   drawing-line-recolor
;;; Parameters:
;;;   line, a line
;;;   color, a color
;;; Purpose:
;;;   Recolor line in color
;;; Produces:
;;;   newline, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   When rendered, newline is in the same place as line, but
;;;   is in the new color.
(define _drawing-line-recolor
  (lambda (line color)
    (drawing-line-core color
                       (_drawing-line-left line)
                       (_drawing-line-top line)
                       (_drawing-line-right line)
                       (_drawing-line-bottom line)
                       (_drawing-line-hstretch line)
                       (_drawing-line-vstretch line))))

(define drawing-line-recolor
  (guard-proc 'drawing-line-recolor 
              _drawing-line-recolor
              (list 'drawing-line 'color)
              (list drawing-line? color?)))

; [From mscm/drawing/drawing-line-right.scm]

;;; Procedure:
;;;   drawing-line-right
;;; Parameters:
;;;   line, a drawing
;;; Purpose:
;;;   Get the right edge of the drawing
;;; Produces:
;;;   right, a real
;;; Preconditions:
;;;   (drawing-line? line)
;;; Postcondition:
;;;   The right edge of drawing
(define _drawing-line-right
  (lambda (drawing)
    (max (list-ref drawing 3) (list-ref drawing 5))))

(define drawing-line-right 
  (guard-unary-proc 'drawing-line-right _drawing-line-right
                    'drawing/line drawing-line?))

; [From mscm/drawing/drawing-line-scale.scm]

;;; Procedure:
;;;   drawing-line-scale
;;; Parameters:
;;;   line, a drawing line
;;:   factor, a real number
;;; Purpose:
;;;   Create a new version of line, scaled by factor
;;; Produces:
;;;   scaled, a drawing
(define _drawing-line-scale
  (lambda (line factor)
    (drawing-line-core (_drawing-line-color line)
                       (* factor (_drawing-line-left line))
                       (* factor (_drawing-line-top line))
                       (* factor (_drawing-line-right line))
                       (* factor (_drawing-line-bottom line))
                       (* factor (_drawing-line-hstretch line))
                       (* factor (_drawing-line-vstretch line)))))

(define drawing-line-scale
  (guard-proc 'drawing-line-scale
              _drawing-line-scale
              (list 'drawing-line 'real)
              (list drawing-line? real?)))

; [From mscm/drawing/drawing-line-top.scm]

;;; Procedure:
;;;   drawing-line-top
;;; Parameters:
;;;   line, a drawing line
;;; Purpose:
;;;   Get the top edge of the drawing
;;; Produces:
;;;   top, a real
(define _drawing-line-top 
  (lambda (line)
    (min (list-ref line 4) (list-ref line 6))))

(define drawing-line-top 
  (guard-unary-proc 'drawing-line-top 
                    _drawing-line-top
                    'drawing-line 
                    drawing-line?))

; [From mscm/drawing/drawing-line-vscale.scm]

;;; Procedure:
;;;   drawing-line-vscale
;;; Parameters:
;;;   line, a drawing line
;;:   factor, a real number
;;; Purpose:
;;;   Create a new version of line, scaled vertically by factor
;;; Produces:
;;;   scaled, a drawing
(define _drawing-line-vscale
  (lambda (line factor)
    (drawing-line-core (_drawing-line-color line)
                       (_drawing-line-left line)
                       (* factor (_drawing-line-top line))
                       (_drawing-line-right line)
                       (* factor (_drawing-line-bottom line))
                       (_drawing-line-hstretch line)
                       (* factor (_drawing-line-vstretch line)))))

(define drawing-line-vscale
  (guard-proc 'drawing-line-vscale
              _drawing-line-vscale
              (list 'drawing-line 'real)
              (list drawing-line? real?)))

; [From mscm/drawing/drawing-line-vshift.scm]

;;; Procedure:
;;;   drawing-line-vshift
;;; Parameters:
;;;   line, a drawing line
;;:   amt, a real number
;;; Purpose:
;;;   Create a new version of line, shifted vertically by amt
;;; Produces:
;;;   shifted, a drawing
(define _drawing-line-vshift
  (lambda (line amt)
    (drawing-line-core (_drawing-line-color line)
                       (_drawing-line-left line)
                       (+ amt (_drawing-line-top line))
                       (_drawing-line-right line)
                       (+ amt (_drawing-line-bottom line))
                       (_drawing-line-hstretch line)
                       (_drawing-line-vstretch line))))

(define drawing-line-vshift
  (guard-proc 'drawing-line-vshift
              _drawing-line-vshift
              (list 'drawing-line 'real)
              (list drawing-line? real?)))

; [From mscm/drawing/drawing-line-vstretch.scm]

;;; Procedure:
;;;   drawing-line-vstretch
;;; Parameters:
;;;   line, a drawing line
;;; Purpose:
;;;   Get the horizontal "stretch" of the line
;;; Produces:
;;;   stretch, a real
(define _drawing-line-vstretch 
  (r-s list-ref 8))

(define drawing-line-vstretch 
  (guard-unary-proc 'drawing-line-vstretch 
                    _drawing-line-vstretch
                    'drawing-line 
                    drawing-line?))

; [From mscm/drawing/drawing-line-width.scm]

;;; Procedure:
;;;   drawing-line-width
;;; Parameters:
;;;   line, a drawing line
;;; Purpose:
;;;   Get the width of the drawing
;;; Produces:
;;;   width, a real
(define _drawing-line-width
  (lambda (line)
    (abs (- (list-ref line 3) (list-ref line 5)))))

(define drawing-line-width 
  (guard-unary-proc 'drawing-line-width 
                    _drawing-line-width
                    'drawing-line 
                    drawing-line?))


; +-------+---------------------------------------------------------------------
; | Rules |
; +-------+

; Rules are thin lines, typically used for drawing a grid for figures.

; [From mscm/drawing/drawing-rule-bottom.scm]

;;; Procedure:
;;;   drawing-rule-bottom
;;; Parameters:
;;;   drawing, a drawing rule
;;; Purpose:
;;;   Get the bottom edge of the drawing
;;; Produces:
;;;   bottom, a real
;;; Preconditions:
;;;   (drawing-rule? drawing)
(define _drawing-rule-bottom 
  (lambda (drawing)
    (max (list-ref drawing 4) (list-ref drawing 6))))

(define drawing-rule-bottom 
  (guard-unary-proc 'drawing-rule-bottom 
                    _drawing-rule-bottom
                    'drawing-rule 
                    drawing-rule?))

; [From mscm/drawing/drawing-rule-color.scm]

;;; Procedure:
;;;   drawing-rule-color
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the color of the rule
;;; Produces:
;;;   color, a color
;;; Preconditions:
;;;   (drawing-rule? drawing)
(define _drawing-rule-color 
  (r-s list-ref (list-index (drawing-format 'rule) 'color)))

(define drawing-rule-color 
  (guard-unary-proc 'drawing-rule-color 
                    _drawing-rule-color
                    'drawing-rule 
                    drawing-rule?))

; [From mscm/drawing/drawing-rule-height.scm]

;;; Procedure:
;;;   drawing-rule-height
;;; Parameters:
;;;   drawing, a drawing rule
;;; Purpose:
;;;   Get the height of the drawing
;;; Produces:
;;;   height, a real
;;; Preconditions:
;;;   (drawing-rule? drawing)
(define _drawing-rule-height
  (lambda (drawing)
    (abs (- (list-ref drawing 4) (list-ref drawing 6)))))

(define drawing-rule-height 
  (guard-unary-proc 'drawing-rule-height 
                    _drawing-rule-height
                    'drawing-rule 
                    drawing-rule?))

; [From mscm/drawing/drawing-rule-left.scm]

;;; Procedure:
;;;   drawing-rule-left
;;; Parameters:
;;;   drawing, a drawing rule
;;; Purpose:
;;;   Get the left edge of the drawing
;;; Produces:
;;;   left, a real
;;; Preconditions:
;;;   (drawing-rule? drawing)
(define _drawing-rule-left 
  (lambda (drawing)
    (min (list-ref drawing 3) (list-ref drawing 5))))

(define drawing-rule-left 
  (guard-unary-proc 'drawing-rule-left 
                    _drawing-rule-left
                    'drawing-rule 
                    drawing-rule?))

; [From mscm/drawing/drawing-rule-right.scm]

;;; Procedure:
;;;   drawing-rule-right
;;; Parameters:
;;;   drawing, a drawing rule
;;; Purpose:
;;;   Get the right edge of the drawing
;;; Produces:
;;;   right, a real
;;; Preconditions:
;;;   (drawing-rule? drawing)
(define _drawing-rule-right 
  (lambda (drawing)
    (max (list-ref drawing 3) (list-ref drawing 5))))

(define drawing-rule-right 
  (guard-unary-proc 'drawing-rule-right 
                    _drawing-rule-right
                    'drawing-rule 
                    drawing-rule?))

; [From mscm/drawing/drawing-rule-top.scm]

;;; Procedure:
;;;   drawing-rule-top
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the top edge of the drawing
;;; Produces:
;;;   top, a real
;;; Preconditions:
;;;   (drawing-rule? drawing)
(define _drawing-rule-top 
  (lambda (drawing)
    (min (list-ref drawing 4) (list-ref drawing 6))))

(define drawing-rule-top 
  (guard-unary-proc 'drawing-rule-top 
                    _drawing-rule-top
                    'drawing-rule 
                    drawing-rule?))

; [From mscm/drawing/drawing-rule-width.scm]

;;; Procedure:
;;;   drawing-rule-width
;;; Parameters:
;;;   drawing, a drawing
;;; Purpose:
;;;   Get the width of the drawing
;;; Produces:
;;;   width, a real
;;; Preconditions:
;;;   (drawing-rule? drawing)
(define _drawing-rule-width
  (lambda (drawing)
    (abs (- (list-ref drawing 3) (list-ref drawing 5)))))

(define drawing-rule-width 
  (guard-unary-proc 'drawing-rule-width 
                    _drawing-rule-width
                    'drawing-rule 
                    drawing-rule?))


; +----------------------------------+------------------------------------------
; | Miscellaneous Drawing Procedures |
; +----------------------------------+

; [From mscm/drawing/drawing-to-figure.scm]

;;; Procedure:
;;;   drawing->figure
;;; Parameters:
;;;   drawing, a drawing
;;;   left, an integer
;;;   top, an integer
;;;   width, an integer
;;;   height, an integer
;;;   scale, an integer
;;; Purpose:
;;;   Convert a drawing to a figure appropriate for inclusion in
;;;   the MediaScheme manual
;;; Produces:
;;;   figure, a drawing
(define _drawing->figure
  (let ((make-rules
         (lambda (left top width height)
           (drawing-recolor
            (drawing-group 
             (apply drawing-group
                    (map (lambda (col)
                           (drawing-rule (+ col left) (+ top -0.5)
                                         (+ col left) (+ top height 0.5)))
                         (iota (+ 1 width))))
             (apply drawing-group
                    (map (lambda (row)
                           (drawing-rule (+ left -0.5) (+ row top)
                                         (+ left width 0.5) (+ row top)))
                         (iota (+ 1 height)))))
            (rgb-new 128 128 128))))
        (make-axes
         (lambda (left top width height)
           (drawing-group
            (drawing-rule 0 0 (+ left width 0.5) 0)
            (drawing-rule 0 0 0 (+ top height 0.5)))))
        (make-positives
         (lambda (left top width height)
           (drawing-recolor
            (drawing-rectangle 0 0 (+ left width 0.5) (+ top height 0.5))
            (rgb-new 223 223 223)))))
    (lambda (drawing left top width height scale)
      (drawing-scale
       (drawing-shift
        (drawing-group
         (make-positives left top width height)
         (make-rules left top width height)
         (make-axes left top width height)
         (make-axes left top width height)
         drawing)
        (- 0.5 left) (- 0.5 top))
       scale))))

(define drawing->figure
  (guard-proc 'drawing->figure
              _drawing->figure
              (list 'drawing
                    'integer 'integer 
                    'positive-integer 'positive-integer
                    'positive-integer)
              (list drawing?
                    integer? integer? 
                    (^and integer? positive?) (^and integer? positive?)
                    (^and integer? positive?))))


; +------------------------------------------------------+----------------------
; | Representing Colors as Hue/Saturation/Value Triplets |
; +------------------------------------------------------+

; [From mscm/hsv/hsv-new.scm]

;;; Procedure:
;;;   hsv-new
;;; Parameters:
;;;   hue, an integer
;;;   saturation, a real
;;;   value, a real
;;; Purpose:
;;;   Create a new HSV color
;;; Produces:
;;;   hsv, an HSV color
;;; Preconditions:
;;;   0 <= hsv <= 360
;;;   0 <= saturation <= 1
;;;   0 <= value <= 1
;;; Postconditions:
;;;   (hsv? hsv)
;;;   (hsv-hue hsv) = hue
;;;   (hsv-saturation hsv) = saturation
;;;   (hsv-value hsv = value
(define hsv-new
  (lambda (hue saturation value)
    (cond
      ((not (integer? hue))
       (error "hsv-new: Expects <integer> for parameter 1, given" hue))
      ((not (0 <= hue <= 360))
       (error "hsv-new: Hue must be in range [0..360], given" hue))
      ((not (real? saturation))
       (error "hsv-new: Expects <real> for parameter 2, given" saturation))
      ((not (0 <= saturation <= 1))
       (error "hsv-new: Saturation must be in range [0..360], given" 
              saturation))
      ((not (real? value))
       (error "hsv-new: Expects <real> for parameter 3, given" value))
      ((not (0 <= value <= 1))
       (error "hsv-new: Value must be in range [0..360], given" 
              value))
      (list hue saturation value))))
     
     

; [From mscm/hsv/hsv-p.scm]

;;; Procedure:
;;;   hsv?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determines if val could represent a hue-saturation-value color.
;;; Produces:
;;;   is-hsv?, a Boolean
(define hsv?
  (lambda (val)
    (and (list? val)
         (= (length val) 3)
         (integer? (car val))
         (<= 0 (car val) 360)
         (real? (cadr val))
         (<= 0 (cadr val) 1)
         (real? (caddr val))
         (<= 0 (caddr val) 1))))

; [From mscm/hsv/hsv-hue.scm]

;;; Procedure:
;;;   hsv-hue
;;; Parameters:
;;;   hsv, an HSV color
;;; Purpose:
;;;   Extract the hue from an HSV color.
;;; Produces:
;;;   hue, an integer
;;; Preconditions:
;;;   (hsv? hsv)
;;; Postconditions:
;;;   0 <= hue <= 360
(define hsv-hue car)

; [From mscm/hsv/hsv-saturation.scm]

;;; Procedure:
;;;   hsv-saturation
;;; Parameters:
;;;   hsv, an HSV color
;;; Purpose:
;;;   Extract the saturation from an HSV color.
;;; Produces:
;;;   saturation, a real
;;; Preconditions:
;;;   (hsv? hsv)
;;; Postconditions:
;;;   0 <= saturation <= 1
(define hsv-saturation cadr)

; [From mscm/hsv/hsv-value.scm]

;;; Procedure:
;;;   hsv-value
;;; Parameters:
;;;   hsv, an HSV color
;;; Purpose:
;;;   Extract the value from an HSV color.
;;; Produces:
;;;   value, a real number
;;; Preconditions:
;;;   (hsv? hsv)
;;; Postconditions:
;;;   0 <= value <= 1
(define hsv-value caddr)

; [From mscm/hsv/hsv-to-rgb.scm]

;;; Procedure
;;;   hsv->rgb
;;; Parmeters:
;;;   hsv, an hsv color
;;; Purpose:
;;;   Convert an hsv color to an rgb color.
;;; Produces:
;;;   rgb, an rgb color.
;;; Preconditions:
;;;   (hsv? hsv)
;;; Postconditions:
;;;   (rgb? rgb)
;;;   (hsv->rgb (rgb->hsv rgb)) should be close to rgb.
(define hsv->rgb
  (lambda (hsv)
    (let* ((h (hsv-hue hsv))
           (s (hsv-saturation hsv))
           (v (hsv-value hsv))
           (hi (mod (floor (/ h 60)) 6))
           (f (- (/ h 60) hi))
           (p (* v (- 1 s)))
           (q (* v (- 1 (* f s))))
           (t (* v (- 1 (* s (- 1 f))))))
      (cond
        ((equal? hi 0) (rgb-new (* 255 v) (* 255 t) (* 255 p)))
        ((equal? hi 1) (rgb-new (* 255 q) (* 255 v) (* 255 p)))
        ((equal? hi 2) (rgb-new (* 255 p) (* 255 v) (* 255 t)))
        ((equal? hi 3) (rgb-new (* 255 p) (* 255 q) (* 255 v)))
        ((equal? hi 4) (rgb-new (* 255 t) (* 255 p) (* 255 v)))
        ((equal? hi 5) (rgb-new (* 255 v) (* 255 p) (* 255 q)))))))

; [From mscm/position/position.scm]

;;; Procedure:
;;;   position
;;; Parameters:
;;;   col, a real number
;;;   row, a real number
;;; Purpose:
;;;   Creates the position (col,row)
;;; Produces:
;;;   pos, a position
;;; Postconditions:
;;;   (position-col pos) = col
;;;   (position-row pos) = row
(define position cons)

; [From mscm/position/position-distance.scm]

;;; Procedure:
;;;   position-distance
;;; Parameters:
;;;   pos1, a position
;;;   pos2, a position
;;; Purpose:
;;;   Computes the distance between pos1 and pos2.
;;; Produces:
;;;   distance, a non-negative real number.
;;; Preconditions:
;;;   [No additional]
(define position-distance
  (lambda (pos1 pos2)
    (sqrt (+ (square (- (position-col pos1) (position-col pos2)))
             (square (- (position-row pos1) (position-row pos2)))))))

; [From mscm/position/position-interpolate.scm]

;;; Procedure:
;;;   position-interpolate
;;; Parameters:
;;;   pos1, a position
;;;   pos2, a position
;;;   percent, a real number
;;; Purpose:
;;;   Compute the position that is percent of the way from pos1 to pos2.
;;; Produces:
;;;   interpolated, a position
(define position-interpolate
  (lambda (pos1 pos2 percent)
    (let ((tnecrep (- 1 percent)))
      (position-new (+ (* percent (position-col pos2)) 
                       (* tnecrep (position-col pos1)))
                    (+ (* percent (position-row pos2)) 
                       (* tnecrep (position-row pos1)))))))

; [From mscm/position/position-offset.scm]

;;; Procedure:
;;;   position-offset
;;; Parameters:
;;;   position, a position
;;;   col-offset, a real number
;;;   row-offset, a real number
;;; Purpose:
;;;   Build a new position, offset by position by the specified offsets.
;;; Produces:
;;;   new-position, a position
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (position-col position-new) = (position-col position) + col_offset
;;;   (position-row position-new) = (position-row position) + row_offset
(define position-offset
  (lambda (position delta-col delta-row)
    (position-new (+ (position-col position) delta-col)
                  (+ (position-row position) delta-row))))

; [From mscm/position/positions-to-floats.scm]

;;; Procedure:
;;;   positions->floats
;;; Parameters:
;;;   positions, a list of positions
;;; Purpose:
;;;   Build an array of floats, applicable to the various
;;;   GIMP PDB procedures that expect positions in that form.
;;; Produces:
;;;   float-positions, an array of floats
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (vector-length float-positions) = (* 2 (length positions))
;;;   For all reasonable i
;;;     (position-col (list-ref positions i)) = 
;;;       (vector-ref float-positions (* 2 i))
;;;     (position-row (list-ref positions i)) = 
;;;        (vector-ref float-positions (+ 1 (* 2 i)))
;;; Philosophy:
;;;   The GIMP PDB procedures need positions in a format that is
;;;   not particularly convenient or clear for novice programmers.
;;;   This procedure, used primarily by MediaScheme GIMP wrappers, allows
;;;   programmers to represent lists of positions in a more natural format.
(define _positions->floats
  (lambda (positions)
    (let* ((len (length positions))
           (floats (make-vector (* 2 len))))
      (let kernel ((pos 0)
                   (remaining positions))
         (if (null? remaining)
             floats
             (begin
               (vector-set! floats pos 
                            (exact->inexact (position-col (car remaining))))
               (vector-set! floats (+ pos 1) 
                            (exact->inexact (position-row (car remaining))))
               (kernel (+ pos 2) (cdr remaining))))))))

(define positions->floats
  (lambda (positions)
    (cond
      ((not (list? positions))
       (error/parameter-type 'positions->floats 1 'list-of-positions 
                             (list positions)))
      ((not (all position? positions))
       (error/parameter-type 'positions->floats 1 'list-of-positions 
                             (list positions)))
      (else
       (_positions->floats positions)))))


; +---------------------------------+-------------------------------------------
; | Generating "Random" Collections |
; +---------------------------------+

; [From mscm/random/random-colors.scm]

;;; Procedure:
;;;   random-colors
;;; Parameters:
;;;   n, an integer
;;; Purpose:
;;;   Generate a list of n colors
;;; Produces:
;;;   colors, a list of n rgb colors
;;; Preconditions:
;;;   n >= 0
;;; Postconditions:
;;;   (length colors) = n
;;;   For each i, 0 < i < n
;;;     (rgb? (list-ref colors i))
(define random-colors
  (lambda (n)
    (cond
      ((not (integer? n))
       (error "random-colors: Parameter 1 (n) must be <int>, given" n))
      ((negative? n)
       (error "random-colors: Parameter 1 (n) must be non-negative, given" 
              n))
      (else
       (_random-colors n)))))

(define _random-colors
  (lambda (n)
    (_list-random n rgb-random)))

; [From mscm/random/random-positions.scm]

;;; Procedure:
;;;   random-positions
;;; Parameters:
;;;   n, an integer
;;;   cols, an integer
;;;   rows, an integer
;;; Purpose:
;;;   Generate a list of n positions, each between (0,0) [inclusive]
;;;     and (cols,rows) [exclusive]
;;; Produces:
;;;   positions, a list of positions.
;;; Preconditions:
;;;   n >= 0
;;;   cols >= 1
;;;   rows >= 1
;;; Postconditions:
;;;   (length positions) = n
;;;   For each i, 0 < i < n
;;;     (position? (list-ref positions i)) 
;;;     (integer? (position-col (list-ref positions i)))
;;;     (integer? (position-row (list-ref positions i)))
;;;     0 <= (position-col (list-ref positions i)) < cols
;;;     0 <= (position-row (list-ref positions i)) < rows
(define random-positions
  (lambda (n cols rows)
    (cond
      ((not (integer? n))
       (error "random-positions: Parameter 1 (n) must be <int>, given" n))
      ((negative? n)
       (error "random-positions: Parameter 1 (n) must be non-negative, given" 
              n))
      ((not (integer? cols))
       (error "random-positions: Parameter 2 (columns) must be <int>, given"
              cols))
      ((not (positive? cols))
       (error "random-positions: Parameter 2 (columns) must be positive, given"
              cols))
      ((not (integer? rows))
       (error "random-positions: Parameter 3 (rows) must be <int>, given" 
              rows))
      ((not (positive? rows))
       (error "random-positions: Parameter 3 (rows) must be positive, given"
              rows))
      (else
       (_random-positions n cols rows)))))

(define _random-positions
  (lambda (n cols rows)
    (_list-random n (lambda () (position-new (random cols) (random rows))))))


; +--------------------------------------+--------------------------------------
; | Miscellaneous MediaScheme Procedures |
; +--------------------------------------+

; [From gimp/misc/pressure-valid.scm]

;;; Procedure:
;;;   pressure-valid?
;;; Parameters:
;;;   pressure, a valud
;;; Purpose:
;;;   Determine if the pressure is valid (a number between 0 and 100,
;;;   inclusive).
;;; Produces:
;;;   is-valid, an integer
(define pressure-valid?
  (lambda (pressure)
    (and (real? pressure) (>= pressure 0) (<= pressure 100))))


; +--------------+--------------------------------------------------------------
; | GIMP Brushes |
; +--------------+

; [From gimp/brush/brush-p.scm]

;;; Procedure:
;;;   brush?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val is a brush.
;;; Produces:
;;;   is-brush?, a Boolean.
;;; Points:
;;;   The #t at the end may seem a bit odd, since it's pointless.  However,
;;;   it ensures that brush? returns a Boolean.
(define brush?
  (lambda (val)
    (and (string? val)
         (member val (context-list-brushes val))
         #t)))


; +---------------+-------------------------------------------------------------
; | Cached Images |
; +---------------+

; Cached images provide faster access to indivisual pixels.  We
; build image caches with image-cache (see the image section).

; [From gimp/cache/cache-flush.scm]

;;; Procedure:
;;;   cache-flush!
;;; Parameters:
;;;   cache, an image cache created by image-cache
;;; Purpose:
;;;   Move the changes to the cache back to the original image
;;; Produces:
;;;   [Nothing; called for the side effect]
(define cache-flush! mgimp-cache-flush!)

(define _cache_flush! mgimp-cache-flush!)

; [From gimp/cache/cache-get-pixel.scm]

;;; Procedure:
;;;   cache-get-pixel
;;; Parameters:
;;;   cache, an image cache created by image-cache
;;;   col, an integer
;;;   row, an integer
;;; Purpose:
;;;   Get the pixel at (col,row)
;;; Produces:
;;;   color, an RGB color
(define cache-get-pixel mgimp-cache-get-pixel)

(define _cache-get-pixel mgimp-cache-get-pixel)

; [From gimp/cache/cache-set-pixel.scm]

;;; Procedure:
;;;   cache-set-pixel!
;;; Parameters:
;;;   cache, an image cache created by image-cache
;;;   col, an integer
;;;   row, an integer
;;;   color, an RGB color
;;; Purpose:
;;;   Set the color at (col,row) to color
;;; Produces:
;;;   [Nothing; called for the side effect]
(define cache-set-pixel! mgimp-cache-set-pixel!)

(define cache-set-pixel! mgimp-cache-set-pixel!)


; +--------------+--------------------------------------------------------------
; | Named Colors |
; +--------------+

; [From gimp/color-name/color-name-p.scm]

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
         (vector-contains? (context-get-color-names) val))))

; [From gimp/color-name/color-name-to-rgb.scm]

;;; Procedure:
;;;   color-name->rgb
;;; Parameters:
;;;   cname, a string
;;; Purpose:
;;;   Convert a named color to an RGB color.
;;; Produces:
;;;   rgb, an RGB color
;;; Preconditions:
;;;   (color-name? cname) must hold
(define _color-name->rgb mgimp-color-name->rgb)

(define color-name->rgb 
  (guard-unary-proc 'color-name->rgb
                    _color-name->rgb
                    'color-name
                    color-name?))


; +------------------------------------------------+----------------------------
; | Representation-Independent GIMP Color Routines |
; +------------------------------------------------+

; [From gimp/color/color-p.scm]

;;; Procedure:
;;;   color?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determines if val is a color in one of the valid representations.
;;; Produces:
;;;   is-color?, a Boolean
;;; Preconditions
;;;   [No additional]
;;; Postconditions
;;;   returns #t if val is a valid kind of color, and #f otherwise.
(define color? 
  (lambda (val)
    (or (rgb? val) 
        (rgb-list? val) 
        (hsv? val)
        (color-name? val))))

; [From gimp/color/color-representation.scm]

;;; Procedure:
;;;   color-representation
;;; Parameters:
;;;   color, a color
;;; Purpose:
;;;   Determine what representation is used for color
;;; Produces:
;;;   representation, a symbol (or #f)
(define color-representation
  (lambda (color)
    (cond
      ((rgb? color) 'RGB)
      ((rgb-list? color) 'RGB-LIST)
      ((hsv? color) 'HSV)
      ((color-name? color) 'COLOR-NAME)
      (else #f))))

; [From gimp/color/color-to-color-name.scm]

;;; Procedure:
;;;   color->color-name
;;; Parameters:
;;;   color, one of the many permitted forms of colors.
;;; Purpose:
;;;   Convert color to the name of the nearest color
;;; Produces:
;;;   color-name, a string
(define color->color-name
  (lambda (color)
    (cond
      ; Note: We violate the normal "check preconditions first" order
      ; for efficiency.  This causes a bit of code duplication.
      ((color-name? color)
       color)
      ((not (color? color))
       (error "color->color-name: invalid color" color))
      (else
       (_color->color-name color)))))

(define _color->color-name
  (lambda (color)
    (cond
      ((color-name? color)
       color)
      (else
       (rgb->color-name (color->rgb color))))))

; [From gimp/color/color-to-rgb.scm]

;;; Procedure:
;;;   color->rgb
;;; Parameters:
;;;   color, a color
;;; Purpose:
;;;   Convert any form of color to rgb.
;;; Produces:
;;;   rgb-color, a color
;;; Preconditions:
;;;   color must be a valid color (color-name, rgb, rgb-list, ...)
;;; Postconditions:
;;;   rgb-color has the same components as color
(define color->rgb
  (lambda (color)
    (cond
      ((rgb? color) color)
      ((hsv? color) (hsv->rgb color))
      ((rgb-list? color) (rgb-list->rgb color))
      ((color-name? color) (color-name->rgb color))
      ((string? color)
       (error "Unknown color name " color))
      (else
       (error "Unknown type of color" color)))))

; The standard "check preconditions first" order doesn't make sense here.
; However, client code may look for _color->rgb.
(define _color->rgb color->rgb)


; +-------------------------------------------+---------------------------------
; | Getting Basic GIMP Contextual Information |
; +-------------------------------------------+

; [From gimp/context/context-get-bgcolor.scm]

;;; Procedure:
;;;   context-get-bgcolor
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Gets the current background color (as an RGB color).
;;; Produces:
;;;   color, an RGB color.
(define context-get-bgcolor
  (lambda ()
    (color->rgb (car (gimp-context-get-background)))))

; [From gimp/context/context-get-fgcolor.scm]

;;; Procedure:
;;;   context-get-fgcolor
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Gets the current foreground color (as an RGB color).
;;; Produces:
;;;   color, an RGB color.
(define context-get-fgcolor
  (lambda ()
    (color->rgb (car (gimp-context-get-foreground)))))

; [From gimp/context/context-get-brush.scm]

;;; Procedure:
;;;   context-get-brush
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Gets the current brush.
;;; Produces:
;;;   brush, a string
;;; Postconditions:
;;;   (brush? brush)
(define context-get-brush
  (lambda ()
    (car (gimp-context-get-brush))))


; +-------------------------------------------+---------------------------------
; | Setting Basic GIMP Contextual Information |
; +-------------------------------------------+

; [From gimp/context/context-set-bgcolor.scm]

;;; Procedure:
;;;   context-set-bgcolor!
;;; Parameters:
;;;   color, a color
;;; Purpose:
;;;   Set the background color.
;;; Produces:
;;;   [Nothing; called for the side effect]
;;; Preconditions:
;;;   color is a one of the valid forms of color.
;;; Postconditions:
;;;   The background color is now the specified color.
(define _context-set-bgcolor!
  (lambda (color)
    (gimp-context-set-background (color->rgb-list color))
    color))

(define context-set-bgcolor!
  (guard-unary-proc 'context-set-bgcolor!
                    _context-set-bgcolor!
                    'color
                    color?))

; [From gimp/context/context-set-fgcolor.scm]

;;; Procedure:
;;;   context-set-fgcolor!
;;; Parameters:
;;;   color, a color
;;; Purpose:
;;;   Set the foreground color.
;;; Produces:
;;;   Nothing.
;;; Preconditions:
;;;   color is one of the valid forms of color.
;;; Postconditions:
;;;   The foreground color is now the specified color.
(define _context-set-fgcolor!
  (lambda (color)
    (process-gimp-result
     (gimp-context-set-foreground (color->rgb-list color)))))

(define context-set-fgcolor!
  (guard-unary-proc 'context-set-fgcolor!
                    _context-set-fgcolor!
                    'color
                    color?))

; [From gimp/context/context-set-brush.scm]

;;; Procedure
;;;   context-set-brush!
;;; Parameters
;;;   brush, a string
;;; Purpose
;;;   Change the gimp's currently selected brush
;;; Produces
;;;   brush, the provided brush
;;; Preconditions
;;;   (brush? brush)
;;; Postconditions
;;;   GIMP's current brush is now set to the given brush
(define _context-set-brush!
  (lambda (brush)
    (gimp-context-set-brush brush)
    brush))

(define context-set-brush!
  (lambda (brush)
    (cond
      ((not (string? brush))
       (error "context-set-brush!: expects a string as a parameter, received"
              brush))
      ((not (brush? brush))
       (error "context-set-brush!: invalid brush:" brush))
      (else 
       (_context-set-brush! brush)))))


; +--------------------------------+--------------------------------------------
; | Listing Context-Related Values |
; +--------------------------------+

; [From gimp/context/context-get-color-names.scm]

;;; Procedure:
;;;   context-get-color-names
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Get a vector of all the available color names.
;;; Produces:
;;;   names, a vector of strings
;;; Partners:
;;;   (context-find-color-names "NAME")
;;;      Provides a way to find a list of names that include "NAME".
(define context-get-color-names mgimp-get-color-names)

; [From gimp/context/context-list-brushes.scm]

;;; Procedure:
;;;  context-list-brushes
;;; Parameters:
;;;   pattern, a string [optional]
;;; Purpose:
;;;   Get a vector of usable brushes.
;;; Produces:
;;;   brushes, a list of strings
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If no parameters are passed, brushes is a list of all brushes in 
;;;     the GIMP.
;;;   Otherwise, brushes is a list of all brushes in the GIMP whose name
;;;     contains the pattern.
;;;   For each reasonable i,
;;;     (brush? (list-ref brushes i))
(define _context-list-brushes
  (lambda restriction
    (let* ((pattern (if (null? restriction) "" (car restriction)))
           (brushes (cadr (gimp-brushes-get-list (string-escape pattern)))))
      (if (vector? brushes) 
          (vector->list brushes) 
          brushes))))

(define context-list-brushes
  (lambda restriction
    (let ((pattern (if (null? restriction) "" (car restriction))))
      (cond
        ((not (string? pattern))
         (error "context-list-brushes: Invalid restriction: " pattern))
        (else
         (apply _context-list-brushes restriction))))))

; [From gimp/context/context-list-colors.scm]

;;; Procedure:
;;;   context-list-colors
;;; Parmeters:
;;;   pattern, a nonempty string [optional]
;;; Purpose:
;;;   List all the colors (or all the colors that match pattern)
;;; Produces:
;;;   colors, a list of strings
;;; Preconditions:
;;;   [Standard]
;;; Postconditions:
;;;   For each reasonable i,
;;;     (color-name? (list-ref colors i))
;;;   If pattern is non null, each string in colors contains pattern.
(define _context-list-colors
  (let ((colors (vector->list (context-get-color-names))))
    (lambda args
      (if (null? args)
          colors
          (list-select colors 
                       (lambda (color) (string-contains? color (car args))))))))
  
(define context-list-colors
    (lambda args
      (cond
        ((null? args)
         (_context-list-colors))
        ((not (null? (cdr args)))
         (error "context-list-colors: One 1 parameter permitted, received" 
                args))
        ((not (string? (car args)))
         (error "context-list-colors: Parameters must be a string, received"
                (car args)))
        (else
         (_context-list-colors (car args))))))

; [From gimp/context/context-list-fonts.scm]

;;; Procedure:
;;;   context-list-fonts
;;; Parameters:
;;;   pattern, a string [optional]
;;; Purpose:
;;;   List all of the fonts (if no parameter is given) or get all of
;;;   the fonts whose name contains pattern.
;;; Produces:
;;;   font-list, a list of strings
(define _context-list-fonts
  (lambda restriction
    (cond
      ((null? restriction)
       (vector->list (cadr (gimp-fonts-get-list ""))))
      (else
       (vector->list (cadr (gimp-fonts-get-list (car restriction))))))))

(define context-list-fonts
  (lambda restriction
    (cond
      ((null? restriction)
       (_context-list-fonts))
      ((not (string? (car restriction)))
       (error "context-get-fonts: Pattern must be a string"))
      ((not (null? (cdr restriction)))
       (throw "context-get-fonts: Only one pattern accepted"))
      (else
       (_context-list-fonts (car restriction))))))


; +------------------------------------------+----------------------------------
; | Miscellaneous Context-Related Procedures |
; +------------------------------------------+

; [From gimp/context/context-immediate-updates.scm]

;;; Procedure:
;;;   context-immediate-updates
;;; Parameters:
;;;   immediate?, an optional boolean
;;; Purpose:
;;;   Set or get a flag that specifies whether or not some operations
;;;   are requested to immediate-updates the context.
;;; Produces:
;;;   immediate-updates?, a Boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If called with no parameters, returns the current state of the
;;;     flag
;;;   If called with one parameter, sets the current state of the flag
;;;     to that parameter.
(define _context-immediate-updates
  (make-flag))

(define context-immediate-updates
  (guard-flag 'context-immediate-updates _context-immediate-updates))

; [From gimp/context/context-immediate-updates-p.scm]

;;; Procedure:
;;;   context-immediate-updates?
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Determine whether or not some operations should immediately
;;;   update the displays
;;; Produces:
;;;   immediate-updates?, a Boolean
;;; Preconditions:
;;;   [No additional]
(define _context-immediate-updates? _context-immediate-updates)

(define context-immediate-updates?
  (guard-proc 'context-immediate-updates?
              _context-immediate-updates?
              null
              null))

; [From gimp/context/context-immediate-updates-on.scm]

;;; Procedure:
;;;   context-immediate-updates-on!
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Indicate that varous operations should immediately update the
;;;   displays.
;;; Produces:
;;;   [Nothing; called for the side effect.]
(define _context-immediate-updates-on!
  (lambda ()
    (context-update-displays!)
    (context-immediate-updates #t)))

(define context-immediate-updates-on! _context-immediate-updates-on!)

; [From gimp/context/context-immediate-updates-off.scm]

;;; Procedure:
;;;   context-immediate-updates-off!
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Indicate that varous operations should not immediately update the
;;;   displays.  In most cases, this means that updates to an image will
;;;   not be visible until one calls (context-update-displays!)
;;; Produces:
;;;   [Nothing; called for the side effect.]
(define _context-immediate-updates-off!
  (lambda ()
    (context-immediate-updates #f)))

(define context-immediate-updates-off! _context-immediate-updates-off!)

; [From gimp/context/context-update-displays.scm]

;;; Procedure:
;;;   context-update-displays!
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Flush recent gimp image operations to the graphical user interface
;;;   (give gimp-displays-flush a "nicer" name for students).
;;; Produces
;;;   [Nothing; called for the side effect]
;;; Preconditions
;;;   [None]
;;; Postconditions
;;;   All completed image operations should be visible.
(define context-update-displays! gimp-displays-flush)


; +------------------------------------------+----------------------------------
; | Miscellaneous Context-Related Procedures |
; +------------------------------------------+

; [From gimp/context/context-preserve.scm]

;;; Procedure:
;;;   context-preserve
;;; Parameters:
;;;   preserve?, an optional boolean
;;; Purpose:
;;;   Set or get a flag that specifies whether or not some operations
;;;   are requested to preserve the context.
;;; Produces:
;;;   preserved?, a Boolean
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If called with no parameters, returns the current state of the
;;;     flag
;;;   If called with one parameter, sets the current state of the flag
;;;     to that parameter.
(define _context-preserve
  (make-flag))

(define context-preserve
  (guard-flag 'context-preserve _context-preserve))

; [From gimp/context/context-preserve-p.scm]

;;; Procedure:
;;;   context-preserve?
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Determine whether or not some operations are requested to preserve 
;;;   the context.
;;; Produces:
;;;   preserved?, a Boolean
;;; Preconditions:
;;;   [No additional]
(define _context-preserve? _context-preserve)

(define context-preserve?
  (guard-proc 'context-preserve?
              _context-preserve?
              null
              null))

; [From gimp/context/context-preserve-on.scm]

;;; Procedure:
;;;   context-preserve-on!
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Indicate that varous operations should preserve the color and brush.
;;; Produces:
;;;   [Nothing; called for the side effect.]
(define _context-preserve-on!
  (lambda ()
    (context-preserve #t)))

(define context-preserve-on! _context-preserve-on!)

; [From gimp/context/context-preserve-off.scm]

;;; Procedure:
;;;   context-preserve-off!
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Indicate that varous operations need not preserve the brush and
;;;   color.
;;; Produces:
;;;   [Nothing; called for the side effect.]
(define _context-preserve-off!
  (lambda ()
    (context-preserve #f)))

(define context-preserve-off! _context-preserve-off!)

; [From gimp/context/context-select-random-brush.scm]

;;; Procedure
;;;   context-select-random-brush!
;;; Parameters:
;;;   [None]
;;; Purpose:
;;;   Select one of the brushes.
;;; Produces:
;;;   [Nothing; called for the side effect]
;;; Postconditions:
;;;   It is difficult to predict the brush.
(define context-select-random-brush!
  (lambda ()
    (context-set-brush! (list-random-element (context-list-brushes)))))


; +----------------+------------------------------------------------------------
; | GIMP Drawables |
; +----------------+

; In general, the MediaScheme GIMP procedures refer to "layers", rather than
; "drawables", even though layers are a subset of drawables.  We made this
; choice because layers seem like a simpler concept to give to novices.
; Nonetheless, we provide a few opeations that work directly with drawables.

; [From gimp/drawable/drawable-p.scm]

;;; Procedure:
;;;   drawable?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determines if val names a drawable value
;;; Produces:
;;;   is-drawable?, a Boolean
(define drawable?
  (lambda (val)
    (and  (integer? val) (gimp-drawable-is-valid val))))


; +---------------------+-------------------------------------------------------
; | Working with Images |
; +---------------------+

; [From gimp/image/image-p.scm]

;;; Procedure:
;;;   image?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val is one of the valid image descriptions.
(define image?
  (lambda (val)
    (or (image-id? val) 
        (image-name? val))))

; [From gimp/image/image-new.scm]

;;; Procedure:
;;;   image-new
;;; Parameters:
;;;   width, a positive integer
;;;   height, a positive integer
;;; Purpose:
;;;   Create an image of the given width and height.
;;; Produces:
;;;   img, an encapsulated image
(define _image-new
  (lambda (width height)
    (let* ((image (car (gimp-image-new width height 0)))
           (layer (car (gimp-layer-new image width height 0 "Layer" 100 0))))
      (gimp-image-add-layer image layer 0)
      ; The following line is a temporary hack while I try to figure
      ; stuff out
      (let ((bgcolor (context-get-bgcolor)))
        (rgb-set-region! layer 0 0 width height bgcolor))
      (gimp-selection-all image)
      (gimp-edit-clear layer)
      (gimp-selection-none image)
      image)))

(define image-new
  (guard-proc 'image-new
              _image-new
              (list 'positive-integer 'positive-integer)
              (list (^and integer? positive?)
                    (^and integer? positive?))))

; [From gimp/image/image-compute.scm]

;;; Procedure:
;;;   image-compute
;;; Parameters:
;;;   pos2color, a function of the form (lambda (col row) color)
;;;   width, a positive integer
;;;   height, a positive integer
;;; Purpose:
;;;   Compute new width-by-height image by using pos2color to compute
;;;   the color at each position.
;;; Produces:
;;;   image, an image
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (image-width image) = width
;;;   (image-height image) = height
;;;   For each col, 0 <= col < width
;;;     For each row, 0 <= row < height
;;;       (image-get-pixel col row) = (pos2color col row)
(define _image-compute
  (lambda (pos2color width height)
    (let ((image (image-new width height)))
      (image-select-all! image)
      (mgimp-layer-compute-pixels-in-selection! (image-get-layer image)
                                               pos2color)
      (image-select-nothing! image)
      image)))

(define image-compute
  (guard-proc 'image-compute
              _image-compute
              (list 'pos2color 'positive-integer 'positive-integer)
              (list (constant #t)
                    (^and integer? exact? positive?)
                    (^and integer? exact? positive?))))

; [From gimp/image/image-load.scm]

;;; Procedure:
;;;   image-load
;;; Parameters:
;;;   fname, a string
;;; Purpose:
;;;   Loads the given image
;;; Produces:
;;;   img, an encapsulated image that corresponds to the image stored
;;;     in the given file.
;;; Preconditions:
;;;   fname names a valid image file.
(define image-load
  (lambda (fname)
    (cond
      ((not (file-exists? fname))
       (error "image-load: no such file" fname))
      (else
       (let* ((image (car (gimp-file-load 1 fname fname))))
         image)))))

; [From gimp/image/image-save.scm]

;;; Procedure:
;;;   image-save
;;; Parameters:
;;;   image, an image
;;;   fname, a string
;;; Purpose:
;;;   Saves the image in the corresponding file.
;;; Produces:
;;;   image, the same image
;;; Preconditions:
;;;   fname must be a full path name to a file [Unverified]
;;;   The file must be writable.
;;; Postconditions:
;;;   The file contains the image.
;;; Postscript:
;;;   The type of the file is determined by the name of the file.
(define _image-save
  (lambda (image fname)
    (gimp-file-save RUN-NONINTERACTIVE
                    image
                    (image-get-layer image)
                    fname
                    fname)))

(define image-save
  (guard-proc 'image-save
              _image-save
              (list 'image 'string/filename)
              (list image? string?)))


; [From gimp/image/image-get-layer.scm]

;;; Procedure:
;;;   image-get-layer
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Gets the active layer from the image.
;;; Produces:
;;;   layer, a layer
;;; Postconditions:
;;;   If the image has no active layer, returns #f
(define image-get-layer
  (lambda (image)
    (let ((id (and image (image-id image))))
      (and id
           (let ((active (car (gimp-image-get-active-layer id)))
                 (layers (gimp-image-get-layers id)))
             (if (= active -1)
                 (if (= (car layers) 0)
                     #f
                     (vector-ref (cadr layers) 0))
                active))))))

; [From gimp/image/image-height.scm]

;;; Procedure:
;;;   image-height
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   get the height of an image
;;; Produces:
;;;   height, an integer
;;; Preconditions:
;;;   image is a valid image
;;; Postconditions:
;;;   heith is the height of image.
(define image-height
  (lambda (image)
    (cond 
      ((not (image? image))
       (error "image-height: image must be a valid image"))
      (else
       (car (gimp-image-height image))))))

; [From gimp/image/image-width.scm]

;;; Procedure:
;;;   image-width
;;; Parameters:
;;;   image, an image
;;; Purpose
;;;   get the width of an image
;;; Produces
;;;   an integer
;;; Preconditions
;;;   image is a valid image
;;; Postconditions
;;;   returns the width of image, an integer
(define image-width
  (lambda (image)
    (cond 
      ((not (image? image))
       (error "image-width: invalid image" image))
      (else
       (car (gimp-image-width image))))))

; [From gimp/image/image-id.scm]

;;; Procedure:
;;;   image-id
;;; Parameters:
;;;   image, one of the various forms of images
;;; Purpose:
;;;   Get the id from the image.
;;; Produces:
;;;   imageid, the GIMP id for the image.
;;; Problems:
;;;   Returns #f if it does not seem to be an image.
(define image-id
  (lambda (image)
    (cond 
      ((and (integer? image) (image-id? image)) image)
      ((string? image) (image-name->image-id image))
      (else #f))))

; [From gimp/image/image-id-p.scm]

;;; Procedure:
;;;   image-id?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine whether or not val is a valid image id
(define image-id?
  (lambda (val)
    (and (integer? val)
         (not (= 0 (car (gimp-image-is-valid val)))))))

; [From gimp/image/image-name.scm]

;;; Procedure:
;;;   image-name
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Gets the name associated with the image.
;;; Produces:
;;;   name, a string
;;; Preconditions:
;;;   image is an image. That is (image? image) holds.
(define image-name
  (lambda (image)
    (cond
      ((image-id? image) (car (gimp-image-get-name image)))
      ((string? image) image)
      (else #f))))

; [From gimp/image/image-name-p.scm]

;;; Procedure:
;;;   image-name?
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   Determines if str names an image
;;; Produces:
;;;   is-image, a boolean
(define image-name?
  (lambda (str)
    (and (image-name->image-id str) #t)))

; [From gimp/image/image-name-to-image-id.scm]

;;; Procedure:
;;;   image-name->image-id
;;; Parameters:
;;;   name, a string
;;; Purpose:
;;;   Find the id of the image associated with the given name.
;;; Produces:
;;;   imageid, that id
;;; Postconditions:
;;;   If there is an image with the given name, imageid is one such image.
;;;     That is (image-name imageid) returns name.
;;;   If there is no such image, imageid is #f.
(define image-name->image-id
  (lambda (name)
    (and (string? name)
         (let* ((stuff (gimp-image-list))
                (len (car stuff))
                (images (cadr stuff)))
           (let kernel ((pos 0))
             (cond
               ((= pos len) #f)
               ((equal? name (image-name (vector-ref images pos)))
                (vector-ref images pos))
               (else (kernel (+ pos 1)))))))))

; [From gimp/image/image-cache.scm]

;;; Procedure:
;;;   image-cache
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Build a cache for the image.  (Caches permit faster setting and
;;;   getting of pixels, but the changes are not visible until the
;;;   cache is flushed.)
;;; Produces:
;;;   cache, a cache
(define image-cache
  (lambda (image)
    (let ((id (image-id image))
          (layer (image-get-layer image)))
      (cond
        ((not id)
         (error "image-cache: expected an image, received " image))
        ((not layer)
         (error "image-cache: cannot cache images with no layers"))
        (else
         (mgimp-drawable-cache layer))))))

; [From gimp/image/image-cached-p.scm]

;;; Procedure:
;;;   image-cached?
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Determine if an image is cached.
;;; Produces:
;;;   cached?, a Boolean
;;; Preconditions:
;;;   image must be an image
;;;   image must have at least one layer
(define image-cached?
  (lambda (image)
    (let ((id (image-id image))
          (layer (image-get-layer image)))
      (and id layer (mgimp-drawable-cached? layer)))))

; [From gimp/image/image-flush.scm]

;;; Procedure:
;;;   image-flush!
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Flush an image's cache
;;; Produces:
;;;   [Nothing, called for the side effect]
(define image-flush!
  (lambda (image)
    (let ((id (image-id image))
          (layer (image-get-layer image)))
      (cond
        ((not id)
         (error "image-flush!: expected an image, received" image))
        ((not layer)
         (error "image-flush!: cannot flush images with no layers"))
        ((not (image-cached? image))
         (error "image-flush!: image is not cached" image))
        (else
         (mgimp-cache-flush! (mgimp-drawable-cache layer)))))))

; [From gimp/image/image-clear.scm]

;;; Procedure:
;;;   image-clear!
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Clear the image
;;; Produces:
;;;   image, the same image
(define image-clear!
  (lambda (image)
    (cond
      ((not (image? image))
       (error "image-clear!: Expects <image>, given" image))
      (_image-clear! image))))

(define _image-clear!
  (lambda (image)
    (let ((selection (image-selection-save image)))
      (image-select-all! image)
      (image-clear-selection! image)
      (image-selection-load! image selection)
      (image-selection-drop image selection)
      image)))

; [From gimp/image/image-get-pixel.scm]

;;; Procedure:
;;;   image-get-pixel
;;; Parameters:
;;;   image, an image
;;;   col, an integer
;;;   row, an integer
;;; Purpose:
;;;   Extract the pixel at (col,row) from image.
;;; Produces:
;;;   color, a color
(define _image-get-pixel
  (lambda (image col row)
    (let* ((cached? (image-cached? image))
           (cache (image-cache image))
           (result (cache-get-pixel cache col row)))
      (if (not cached?) (cache-flush! cache) (void))
      result)))

(define image-get-pixel
  (lambda (image col row)
    (let ((id (image-id image)))
      (cond
        ((not id)
         (error "image-get-pixel: Invalid image" image))
        (else
         (_image-get-pixel id col row))))))

; [From gimp/image/image-set-pixel.scm]

;;; Procedure:
;;;   image-set-pixel!
;;; Parameters:
;;;   image, an image
;;;   col, a column
;;;   row, a row
;;;   color, an RGB color
;;; Purpose:
;;;   Sets the pixel at (col,row) to color.
;;; Produces:
;;;   [Nothing; called for the side effect]
(define image-set-pixel!
  (lambda (image col row color)
    (let ((id (image-id image)))
      (cond
        ((not id)
         (error "image-set-pixel: Invalid image" image))
        (else
         (let* ((cached? (image-cached? image))
                (cache (image-cache id))
                (result (cache-set-pixel! cache col row color)))
           (when (not cached?) 
             (cache-flush! cache))
           result))))))

; [From gimp/image/image-select-all.scm]

;;; Procedure:
;;;   image-select-all!
;;; Parameters:
;;;   image, a gimp image
;;; Purpose
;;;   Select all pixels in the image
;;; Produces:
;;;   [Nothing; Called for the side effect]
;;; Preconditions:
;;;   image is a valid image
;;; Postconditions:
;;;   All pixels in image have been selected.
(define image-select-all!
  (lambda (image)
    (let ((id (image-id image)))
      (if id
          (begin (gimp-selection-all id) image)
          (error "image-select-all!: invalid image: " image)))))

; [From gimp/image/image-has-selection-p.scm]

;;; Procedure:
;;;   image-has-selection?
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Determine if anything is selected on the image
;;; Produces:
;;;   has-selection?, a Boolean
(define _image-has-selection?
  (lambda (image)
    (zero? (car (gimp-selection-is-empty (image-id image))))))

(define image-has-selection?
  (guard-unary-proc 'image-has-selection? _image-has-selection?
                    'image image?))

; [From gimp/image/image-selection-bounds.scm]

;;; Procedure:
;;;   image-selection-bounds
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Find the bounds of the image's selection.
;;; Produces:
;;;   bounds, a five element list
;;;     (list-ref bounds 0) is a Boolean that indicates whether or not
;;;       anything is selected.  If nothing is selected, the selection
;;;       bounds will be the whole image.
;;;     (list-ref bounds 1) is an integer that gives the left column
;;;       of the selection
;;;     (list-ref bounds 2) is an integer that gives the top row of
;;;       the selection
;;;     (list-ref bounds 3) is an integer that gives the column that
;;;       immediately follows the selection
;;;     (list-ref bounds 4) is an integer that gives the row that
;;;       immediately follows the selection
(define image-selection-bounds
  (lambda (image)
    (let ((id (image-id image)))
      (cond
        ((not id)
         (error "image-selection-bounds: invalid image: " image))
        (else
         (let ((bounds (gimp-selection-bounds id)))
           (cons (> (car bounds) 0)
                 (cdr bounds))))))))

; [From gimp/image/image-selection-height.scm]

;;; Procedure:
;;;   image-selection-height
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Find the height of image's selection.
;;; Produces:
;;;   height, an integer
(define image-selection-height
  (lambda (image)
    (let ((bounds (image-selection-bounds image)))
      (- (list-ref bounds 4) (list-ref bounds 2)))))

; [From gimp/image/image-selection-left.scm]

;;; Procedure:
;;;   image-selection-left
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Find the left edge of image's selection.
;;; Produces:
;;;   left, an integer
(define image-selection-left
  (lambda (image)
    (list-ref (image-selection-bounds image) 1)))

; [From gimp/image/image-selection-top.scm]

;;; Procedure:
;;;   image-selection-top
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Find the top edge of image's selection.
;;; Produces:
;;;   top, an integer
(define image-selection-top
  (lambda (image)
    (list-ref (image-selection-bounds image) 2)))

; [From gimp/image/image-selection-width.scm]

;;; Procedure:
;;;   image-selection-width
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Find the width of image's selection.
;;; Produces:
;;;   width, an integer
(define image-selection-width
  (lambda (image)
    (let ((bounds (image-selection-bounds image)))
      (- (list-ref bounds 3) (list-ref bounds 1)))))

; [From gimp/image/image-selection-drop.scm]

;;; Procedure:
;;;   image-selection-drop!
;;; Parameters:
;;;   image, an image
;;;   selection, a selection
;;; Purpose:
;;;   Remove a saved selection.  (Afterwards, it is invalid to try to
;;;   reload the selection with image-selection-load!.)
;;; Produces:
;;;   [Nothing, called for the side effect]
;;; Preconditions:
;;;   selection must have been created with (image-selection-save image)
;;;   selection must not have been previously dropped
(define image-selection-drop!
  (lambda (image selection)
    (gimp-image-remove-channel image selection)
    (void)))

; [From gimp/image/image-selection-load.scm]

;;; Procedure:
;;;   image-load-selection!
;;; Parameters:
;;;   image, an image
;;;   selection, a selection
;;; Purpose:
;;;   Restore a previously saved selection
;;; Produces:
;;;   [Nothing; called for the side effect.]
;;; Preconditions:
;;;   selection must have been created with (image-save-selection image).
;;;   selection must not have been previously deleted with image-drop-selection.
(define image-selection-load!
  (lambda (image selection)
    (gimp-selection-load selection)
    (void)))

; [From gimp/image/image-selection-save.scm]

;;; Procedure:
;;;   image-selection-save
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Get a value that represents the current selection of the image
;;; Produces:
;;;   selection, a value that represents the selection
;;; Preconditions:
;;;   image must be a valid image
;;; Postconditions:
;;;   The image is unaffected.
;;;   (image-restore-selection image selection) will restore the selection.
(define image-selection-save
  (lambda (image)
    (car (gimp-selection-save image))))

; [From gimp/image/image-calculate-pixels.scm]

;;; Procedure:
;;;   image-calculate-pixels!
;;; Parameters:
;;;   image, an image
;;;   colorfun, a procedure of the form (lambda (col row) color)
;;; Purpose:
;;;   Iterate through the positions in the image, row by row, setting the 
;;;     color at each position to (colorfun col row).
;;; Produces:
;;;   image, the updated image.
;;; Preconditions:
;;;   image is a valid, open image
;;;   colorfun is a procedure of the form (lambda (col row) color) 
;;; Postconditions:
;;;   For each position in the image, if (colorfun col row) is not
;;;     rgb-transparent, then the pixel at (col,row) is now
;;;     (colorfun col row).
;;; Philosophy:
;;;   In contrast with image-compute-pixels!, which can compute pixels
;;;   in any order, image-calculate-pixels! is guaranteed to scan through
;;;   the image a row at a time.
;;;     However, image-calculate-pixels! is much slower and may use
;;;   more resources than image-compute-pixels!.
(define _image-calculate-pixels!
  (lambda (image colorfun)
    (region-calculate-pixels! image 
                              0 0 
                              (image-width image) (image-height image)
                              colorfun)))

(define image-calculate-pixels!
  (guard-proc 'image-calculate-pixels!
              _image-calculate-pixels!
              (list 'image 'procedure)
              (list image? procedure?)))

; [From gimp/image/image-compute-pixels.scm]

;;; Procedure:
;;;   image-compute-pixels!
;;; Parameters:
;;;   image, an image
;;;   pos2color, a function of the form (lambda (col row) color)
;;; Purpose:
;;;   Compute all of the pixels in the image by applying fun to each
;;;      (col,row) pair in the image and using the result as an RGB
;;;      color.
;;; Produces:
;;;   image, the same image
(define _image-compute-pixels!
  (lambda (image pos2color)
    (let ((id (image-id image)))
      (let ((sel (image-selection-save id)))
        (image-select-all! image)
        (mgimp-layer-compute-pixels-in-selection! (image-get-layer id)
                                                  pos2color)
        (image-selection-load! id sel)
        (image-selection-drop! id sel)
        image))))

(define image-compute-pixels!
  (guard-proc 'image-compute-pixels!
              _image-compute-pixels!
              (list 'image 'function-from-position-to-color)
              (list image? procedure?)))

; [From gimp/image/image-scan.scm]

;;; Procedure:
;;;   image-scan
;;; Parameters:
;;;   image, an image
;;;   proc!, a procedure of the form (lambda (col row color) ...)
;;; Purpose:
;;;   Iterate through the positions in the image, row by row, appling
;;;     proc! to the column, row, and color of each position.
;;; Produces:
;;;   image, the same image
;;; Preconditions:
;;;   image is a valid, open image
;;;   proc! is a procedure of the form (lambda (col row color) ...)
;;; Postconditions:
;;;   proc! has been applied to every <col,row,color> triplet.
(define _image-scan
  (lambda (image proc!)
    (region-scan image 0 0 (image-width image) (image-height image) proc!)))

(define image-scan
  (guard-proc 'image-scan
              _image-scan
              (list 'image 'procedure)
              (list image? procedure?)))

; [From gimp/image/image-transform.scm]

;;; Procedure:
;;;   image-transform!
;;; Parameters:
;;;   image, an image
;;;   color-transform, a color transformation
;;; Purpose:
;;;   Transform image by applying color-transform to each pixel
;;;     in image.
;;; Produces:
;;;   image, the same image
(define image-transform!
  (lambda  (image color-transform)
    (let ((id (image-id image)))
      ; Ensure that we have a valid image.
      (cond ((not id)
             (error "image-transform!: Expects an image, received " image))
            (else
             (let ((sel (image-selection-save id)))
               (image-select-all! image)
               (mgimp-layer-slurp-from-selection! (image-get-layer id)
                                                  (image-get-layer id)
                                                  color-transform
                                                  0
                                                  0)
               (image-selection-load! id sel)
               (image-selection-drop! id sel)
               image))))))

; [From gimp/image/image-variant.scm]

;;; Procedure:
;;;   image-variant
;;; Parameters:
;;;   image, an image
;;;   color-transform, a color transformation
;;; Purpose:
;;;   Build a new version of image by applying color-transform to 
;;;     each pixel in image
;;; Produces:
;;;   variant, the new version of the image
(define image-variant
  (lambda  (image color-transform)
    (let ((id (image-id image)))
      ; Ensure that we have a valid image.
      (cond 
        ((not id)
         (error "image-variant: Expects an image, received " image))
        (else
         ; Build the new image
         (let ((new-image (image-new (image-width id) (image-height id))))
           ; Save the selection in the current image
           (let ((sel (image-selection-save id)))
             ; Replace the selection
             (image-select-all! image)
             ; Do the real work
             (mgimp-layer-slurp-from-selection! (image-get-layer new-image)
                                                (image-get-layer id)
                                                color-transform
                                                0
                                                0)
             ; Restore the selection
             (image-selection-load! id sel)
             (image-selection-drop! id sel)
             ; And return the newly computed image
             new-image)))))))

; [From gimp/image/image-to-list.scm]

;;; Procedure:
;;;   image-to-list
;;; Parameters:
;;;   image, an image
;;;   fun, a function of the form (lambda (col row color) ...)
;;; Purpose:
;;;   Build a list of values by iterating through the positions in image.
;;;   For each position in the image, we create a value in the list by applying
;;;     fun to the column, the row, and the color at the position. 
;;; Produces:
;;;   lst, a list
;;; Preconditions:
;;;   image is a valid, open image
;;;   colorfun is a function of the form (lambda (col row) ...)
;;; Postconditions:
(define _image->list
  (lambda (image fun)
    (let ((iwidth (image-width image))
          (iheight (image-height image))
          (cache (image-cache image)))
      (let kernel ((c 0)
                   (r 0))
        (cond
          ((>= r iheight)
           (cache-flush! cache)
           null)
          ((>= c iwidth)
           (kernel 0 (+ r 1)))
          (else
           (let ((color (cache-get-pixel cache c r)))
             (cons (fun c r color)
                   (kernel (+ c 1) r)))))))))

(define image->list
  (guard-proc 'image->list
              _image->list
              (list 'image 'function-from-col-row-color)
              (list image? procedure?)))
; [From gimp/image/image-scan-region.scm]

;;; Procedure:
;;;   image-scan-region
;;; Parameters:
;;;   image, an image
;;;   proc!, a procedure of the form (lambda (col row color) ...)
;;;   left, an integer
;;;   top, an integer
;;;   width, an integer
;;;   height, an integer
;;; Purpose:
;;;   Iterate through the rectangular grid of positions in the image, 
;;;     applying colorfun tat each position.
;;; Produces:
;;;   [Nothing, called for the side effect.]
;;; Preconditions:
;;;   image is a valid, open image
;;;   0 <= left < (image-width image)
;;;   0 <= top < (image-height image)
;;;   0 < width
;;;   0 < height
;;;   (+ left width) <= (image-width image)
;;;   (+ top height) <= (image-height image)
;;;   proc! is a procedure of the form (lambda (col row color) ...)
;;; Postconditions:
;;;   proc! has been applied to every <col,row,color> triplet.
(define _image-scan-region
  (lambda (image proc! left top width height)
    (let ((iwidth (image-width image))
          (iheight (image-height image))
          (layer (image-get-layer image))
          (right (+ left width))
          (bottom (+ top height)))
      ; Check more preconditions
      (cond
        ((>= left iwidth)
         (error "image-scan-region: Left margin must be < width, given" left))
        ((>= top iheight)
         (error "image-scan-region: Top margin must be < height, given" left))
        ((> right iwidth) 
         (error "image-scan-region: width too large, given" width))
        ((> bottom iheight) 
         (error "image-scan-region: height too large, given" height)))
      ; Life is faster if you cache the layer
      (let ((cache (image-cache image)))
        (let kernel ((c left)
                     (r top))
          (cond
            ((>= r bottom)
             (cache-flush! cache)
             image)
            ((>= c right)
             (kernel left (+ r 1)))
            (else
             (let ((color (cache-get-pixel cache c r)))
               (proc! c r color)
               (kernel (+ c 1) r)))))))))

(define image-scan-region
  (let ((nni? (^and integer? exact? (^not negative?)))
        (pi? (^and integer? exact? positive?)))
    (guard-proc 'image-scan-region
                _image-scan-region
                (list 'image 'procedure 'non-negative-integer 'non-negative-integer 'positive-integer 'positive-integer)
                (list image? procedure? nni? nni? pi? pi?))))  
; [From gimp/image/image-airbrush-line.scm]

;;; Procedure:
;;;   image-airbrush-line!
;;; Parameters:
;;;   image, an image
;;;   pressure, a real
;;;   col1, a real
;;;   row1, a real
;;;   col2, a real
;;;   row2, a real
;;; Preconditions:
;;;   image is not NULL
;;;   0 <= pressure <= 100
;;; Purpose:
;;;   Draw a line with set airbrush pressure and the current foreground color, 
;;;   beginning at (col1,row1) and ending at (col2, row2).
;;; Produces:
;;;   [Nothing; called for the side effect]
(define _image-airbrush-line!
  (lambda (image pressure col1 row1 col2 row2)
    (gimp-airbrush (image-get-layer image)
                   pressure
                   4
                   (vector col1 row1 col2 row2))
    (cond ((context-immediate-updates?) (context-update-displays!)))
    image))

(define image-airbrush-line!
  (guard-proc 'image-airbrush-line!
              _image-airbrush-line!
              (list 'image 'between-0-and-100 'real 'real 'real 'real)
              (list image? pressure-valid? real? real? real? real?)))



; [From gimp/image/image-animate.scm]

;;; Procedure:
;;;   image-animate!
;;; Parameters:
;;;   image, an image
;;;   makeframe!, a procedure of the form (lambda (image i n) ...)
;;; Purpose:
;;;   "Animate" the image by calling makeframe! on every number from
;;;   0 to n-1, pausing for a tenth of a second between frames.
;;; Produces:
;;;   image, the same image
(define _image-animate!
  (lambda (image makeframe! n)
    (let kernel ((step 0))
      (cond
        ((< step n)
         (image-select-all! image)
         (image-clear-selection! image)
         (makeframe! image step n)
         (context-update-displays!)
         (image-select-nothing! image)
         (usleep 100000)
         (kernel (+ step 1)))
        (else
         image)))))

(define image-animate!
  (guard-proc 'image-animate!
              _image-animate!
              (list 'image 'procedure 'positive-integer)
              (list image? procedure? (^and integer? positive?))))

; [From gimp/image/image-blot.scm]

;;; Procedure:
;;;   image-blot!
;;; Parameters:
;;;   image, an image
;;;   col, an integer
;;;   row, an integer
;;; Purpose:
;;;   Draw a spot at (col,row) using the current brush and foreground
;;;   color.
;;; Produces:
;;;   image, the modified image.
;;; Preconditions:
;;;   (image? image)
;;;   0 <= col < (image-width image)
;;;   0 <= row < (image-height image)
;;; Postconditions:
;;;   image contains an additional spot at (col,row)
;;;   That spot may not yet be visible.
(define _image-blot!
  (lambda (image x y)
    (gimp-paintbrush-default (image-get-layer image) 2 (vector x y))
    image))

(define image-blot!
  (guard-proc 'image-blot! _image-blot!
              '(image real real)
              (list image? real? real?)))

; [From gimp/image/image-build.scm]

;;; Procedure:
;;;   image-build!
;;; Parameters:
;;;   image, an image
;;;   makeframe!, a procedure of the form (lambda (image i n) ...)
;;; Purpose:
;;;   Gradually build an image by makeframe! to each i from 0 to n-1.
;;; Produces:
;;;   image, the final image
(define image-build!
  (lambda (image makeframe! n)
    (let kernel ((step 0))
      (cond
        ((< step n)
         (makeframe image step n)
         (context-update-displays!)
         (image-select-nothing! image)
         (usleep 100000)
         (kernel (+ step 1)))
        (else
         image)))))

; [From gimp/image/image-clear-selection.scm]

;;; Procedure:
;;;   image-clear-selection!
;;; Parameters:
;;;   image, a GIMP image
;;; Purpose:
;;;    Clears the current selection in the active layer in the image.
;;; Produces:
;;;    image, the same image
;;; Preconditions:
;;;    image is a valid image
;;; Postconditions:
;;;    All pixels inside the current selection in image are now the 
;;;    background color (or transparent if the active layer has an
;;;    alpha channel)
(define _image-clear-selection!
  (lambda (image)
    (gimp-edit-clear (image-get-layer image))
    image))

(define image-clear-selection!
  (guard-unary-proc 'image-clear-selection!
                    _image-clear-selection!
                    'image
                    image?))

; [From gimp/image/image-color-swatch.scm]

;;; Procedure:
;;;   image-color-swatch
;;; Parameters:
;;;   color1, a color
;;;   color2, a color [optional]
;;;   color3, a color [optional]
;;;   color4, a color [optional]
;;;   color5, a color [optional]
;;;   color6, a color [optional]
;;; Purpose:
;;;   Create a "color swatch", an image that shows the colors
;;; Produces:
;;;   swatch, an image
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   The only colors in swatch are color1 ... color6.
;;;   All of the specified colors are in swatch
(define _image-color-swatch
  (let ((width 60)
        (height 16))
    (lambda colors
      (let* ((len (length colors))
             (wid (/ width len))
             (rgbs (map color->rgb colors)))
        (image-compute (lambda (col row)
                         (list-ref rgbs (quotient col wid)))
                       width height)))))

(define image-color-swatch
  (lambda colors
    (cond
      ((null? colors)
       (error/arity 'image-color-swatch "1 to 6" colors))
      ((< 6 (length colors))
       (error/arity 'image-color-swatch "1 to 6" colors))
      ((not (all color? colors))
       (error/misc 'image-color-swatch "encountered non-color" colors))
      (else 
       (apply _image-color-swatch colors)))))
; [From gimp/image/image-connect-the-dots.scm]

;;; Procedure:
;;;   image-connect-the-dots
;;; Parameters:
;;;   image, an image
;;;   positions, a list of positions
;;; Purpose:
;;;   Draw a line through all the positions
;;; Produces:
;;;   image, an image
;;; Preconditions:
;;;   There are at least two values in positions.  
;;;   All values in positions are positions: (all position? positions)
;;; Postconditions:
;;;   A line has been drawn through the positions, using the current
;;;   color and brush.
(define image-connect-the-dots!
  (lambda (image positions)
    (cond
      ((not (image? image))
       (error "image-connect-the-dots!: invalid image" image))
      ((not (all position? positions))
       (error "image-connect-the-dots!: invalid positions" positions))
      ((or (null? positions) (null? (cdr positions)) (null? (cddr positions)))
       (error "image-connect-the-dots!: at least two positions required"))
      (else
       (_image-connect-the-dots! image positions)))))

(define _image-connect-the-dots!
  (lambda (image positions)
    (let ((floats (positions->floats positions)))
      (gimp-paintbrush-default (image-get-layer image)
                               (vector-length floats)
                               floats)
      image)))

; [From gimp/image/image-contains-layer-p.scm]

;; Procedure
;;  image-contains-layer?
;; Parameters
;;  image, an image-id
;;  layer, a layer-id
;; Purpose
;;  determine whether layer refers to a layer in image
;; Produces
;;  layer? a boolean
;; Preconditions
;;  image must be a valid image
(define image-contains-layer?
  (lambda (image layer)
    (if (image? image)
        (let kernel ((booleanlist (map (left-section equal? layer) (vector->list (cadr (gimp-image-get-layers image)))))
                     (found #f))
          (if (or found (null? booleanlist))
              found
              (kernel (cdr booleanlist) (car booleanlist))))
        (error "image-contains-layer?: image must be a valid image"))))

; [From gimp/image/image-copy-selection.scm]

;;; Procedure:
;;;   image-copy-selection
;;; Parameters
;;;   image, an image
;;; Purpose:
;;;   Copy the selected portion of the image
;;; Produces:
;;;   [Nothing; Called for the side effect]
;;; Preconditions:
;;;   image is a valid image
;;;   something in image is selected
;;; Postconditions:
;;;   The "edit buffer" now contains the same colors as the
;;;   selected region.
(define image-copy-selection
  (lambda (image)
    (cond
      ((not (image? image))
       (error "image-copy-selection: Not a valid image"))
      (else
       (gimp-edit-copy (image-get-layer image))))
    image))

; [From gimp/image/image-curve-the-dots.scm]

;;; Procedure:
;;;   image-curve-the-dots!
;;; Parameters:
;;;   image, an image
;;;   dots, a list of positions 
;;;   curviness, a real number
;;; Purpose:
;;;   Draw a "reasonable" curved line through the specified positions
;;;   If close is true (#t), draws it as a closed loop.
;;; Produces:
;;;   image, the modified image
;;; Preconditions:
;;;   There are at least two values in dots.  
;;;   All values in dots are positions
;;;     That is, (all position? dots)
;;; Postconditions:
;;;   A curved has been drawn through the positions, using the current
;;;   color and brush.
(define image-curve-the-dots!
  (lambda (image dots curviness)
    (cond
      ((not (image? image))
       (error "image-curve-the-dots!: invalid image" image))
      ((not (all position? dots))
       (error "image-curve-the-dots!: invalid positions" dots))
      ((or (null? dots) (null? (cdr dots)) (null? (cddr dots)))
       (error "image-curve-the-dots!: at least two positions required"))
      (else
       (_image-curve-the-dots! image dots curviness)))))

(define _image-curve-the-dots!
  (letrec (
           (compute-bezier-positions
            (lambda (prev current next curviness)
              (let* ((delta-col-1 
                      (- (position-col prev) (position-col current)))
                     (delta-col-2 
                      (- (position-col next) (position-col current)))
                     (delta-row-1 
                      (- (position-row prev) (position-row current)))
                     (delta-row-2 
                      (- (position-row next) (position-row current)))
                     (dist-1 
                      (sqrt (+ (square delta-col-1) (square delta-row-1))))
                     (dist-2 
                      (sqrt (+ (square delta-col-2) (square delta-row-2))))
                     (temp-col 
                      (* 0.5 (+ (/ delta-col-1 dist-1) (/ delta-col-2 dist-2))))
                     (temp-row 
                      (* 0.5 (+ (/ delta-row-1 dist-1) (/ delta-row-2 dist-2))))
                     (scale 
                      (sqrt (+ (square temp-col) (square temp-row))))
                     (delta-col 
                      (/ temp-col scale))
                     (delta-row 
                      (/ temp-row scale))
                     (control-1 
                      (position-offset current 
                                       (* 20 delta-row) (* -20 delta-col)))
                     (control-2 
                      (position-offset current 
                                       (* -20 delta-row) (* 20 delta-col)))
                     (scale-1 (* 0.5 curviness dist-1))
                     (scale-2 (* 0.5 curviness dist-2))
                     (helper 
                      (if (< (position-distance control-1 prev)
                             (position-distance control-2 prev))
                          (cons 1 -1)
                          (cons -1 1)))
                     )
                (list (position-offset current
                                       (* (car helper) scale-1 delta-col)
                                       (* (cdr helper) scale-1 delta-row))
                      current
                      (position-offset current
                                       (* (cdr helper) scale-2 delta-col)
                                       (* (car helper) scale-2 delta-row))))))
           ; dots->bezier computes bezier positions for a sequence of dots.
           (dots->bezier
            (lambda (dots curviness closed?)
              (append 
               (if closed?
                   (compute-bezier-positions (list-last dots) (car dots) (cadr dots) 
                                             curviness)
                   (list (car dots) (car dots) (car dots)))
               (let kernel ((prev (car dots))
                            (current (cadr dots))
                            (remaining (cddr dots)))
                 (if (null? remaining)
                     (if closed?
                         (append
                          (compute-bezier-positions prev current (car dots) curviness) 
                          (compute-bezier-positions current (car dots) (cadr dots) 
                                                    curviness))
                         (list current current current))
                     (let ((next (car remaining)))
                       (append (compute-bezier-positions prev current next curviness)
                               (kernel current next (cdr remaining))))))))))
    (lambda (image dots curviness)
      (let ((positions (dots->bezier dots curviness #f)))
        (image-draw-bezier! image positions)
        ; (image-display-control! image positions)
        image))))

; [From gimp/image/image-display-control.scm]

;;; Procedure:
;;;   image-display-control!
;;; Parameters:
;;;   image, an RGB image
;;;   positions, a list of positions suitable for image-draw-bezier!
;;; Purpose:
;;;   Draws the control positions (and some other info) for the bezier
;;;   curve.  Intended mostly for debugging.
;;; Produces:
;;;   image, the modified image
(define image-display-control!
  (lambda (image positions)
    (let ((saved-color (and (context-preserve?) (context-get-fgcolor)))
          (saved-brush (and (context-preserve?) (context-get-brush))))
      (let kernel ((remaining positions))
        (when (not (null? remaining))
          (context-set-brush! "Circle (03)")
          (context-set-fgcolor! color-red)
          (image-dot! image (car remaining))
          (context-set-fgcolor! color-black)
          (image-dot! image (cadr remaining))
          (context-set-fgcolor! color-blue)
          (image-dot! image (caddr remaining))
          (context-set-brush! "Circle (01)")
          (context-set-fgcolor! color-black)
          (image-draw-line! image (position-col (car remaining))
                                  (position-row (car remaining))
                                  (position-col (caddr remaining))
                                  (position-row (caddr remaining)))
          (kernel (cdddr remaining))))
      (when saved-brush (context-set-brush! saved-brush))
      (when saved-color (context-set-fgcolor! color))
      image)))

; [From gimp/image/image-dot.scm]

;;; Procedure:
;;;   image-dot!
;;; Parameters:
;;;   image, an image
;;;   position, a position
;;; Purpose:
;;;   Draw a dot in the current brush and color at the specified position
;;;   on the image.
;;; Produces:
;;;   image, the modified image
;;; Preconditions:
;;;   position most be on the image.
(define _image-dot!
  (lambda (image position)
    (image-blot! image (position-col position) (position-row position))))

(define image-dot!
  (guard-proc 'image-dot!
              _image-dot!
              (list 'image 'position)
              (list image? position?)))

; [From gimp/image/image-draw-arrow.scm]

;;; Procedure:
;;;   image-draw-arrow!
;;; Parameters:
;;;   image, an image
;;;   type, a symbol
;;;   from-col, a real number
;;;   from-row, a real number
;;;   to-col, a real number
;;;   to-row, a real number
;;;   head-width, a positive real number
;;;   head-length, a positive real number
;;; Purpose:
;;;   Draw an arrow from (from-col,from-row) to (to-col,to-row), with a 
;;;   head of the specified width and length.
;;; Produces:
;;;   image, the same image (now updated with an arrow)
;;; Preconditions:
;;;   (from-col,from-row) != (to-col,to-row)
;;;   type must be one of 'line, 'filled, 'hollow, 'pointy, and
;;;     'hollow-pointy
(define _image-draw-arrow!
  (lambda (image type from-col from-row to-col to-row head-width head-length)
    (let* ((delta-col (- to-col from-col))
           (delta-row (- to-row from-row))           
           (distance (sqrt (+ (square delta-col) (square delta-row))))
           (scale-col (/ delta-col distance))
           (scale-row (/ delta-row distance))
           (half-width (* head-width 0.5))
           ; (c0,r0) gives a point head-length back along the line
           (c0 (- to-col (* scale-col head-length)))
           (r0 (- to-row (* scale-row head-length)))
           ; hoff and voff give horizontal and vertical offsets to
           ; arrow tail points
           (hoff (* scale-row half-width))
           (voff (* scale-col half-width))
           ; (c1,r1) and (c2,r2)
           (c1 (+ c0 hoff))
           (r1 (- r0 voff))
           (c2 (- c0 hoff))
           (r2 (+ r0 voff))
           ; (c3,r3) gives a point halfway to c0 r0
           (c3 (- to-col (* scale-col head-length 0.5)))
           (r3 (- to-row (* scale-row head-length 0.5))))
      (cond
        ((eq? type 'lines)
         (image-draw-line! image from-col from-row to-col to-row)
         (image-draw-line! image c1 r1 to-col to-row)
         (image-draw-line! image c2 r2 to-col to-row))
        ((eq? type 'hollow)
         (image-draw-line! image from-col from-row c0 r0)
         (image-draw-line! image c1 r1 to-col to-row)
         (image-draw-line! image c2 r2 to-col to-row)
         (image-draw-line! image c1 r1 c2 r2))
        ((eq? type 'hollow-pointy)
         (image-draw-line! image from-col from-row c3 r3)
         (image-draw-line! image c1 r1 to-col to-row)
         (image-draw-line! image c2 r2 to-col to-row)
         (image-draw-line! image c1 r1 c3 r3)
         (image-draw-line! image c2 r2 c3 r3))
        ((or (eq? type 'filled) (eq? type 'pointy))
         (let ((sel (image-selection-save image)))
           (cond
             ((eq? type 'filled)
              (image-draw-line! image from-col from-row to-col to-row)
              (image-select-polygon! image REPLACE ; should be INTERSECT, but needs work
                                     (position-new c1 r1)
                                     (position-new c2 r2)
                                     (position-new to-col to-row)))
             ((eq? type 'pointy)
              (image-draw-line! image from-col from-row c3 r3)
              (image-select-polygon! image REPLACE ; should be INTERSECT, but needs work
                                     (position-new c1 r1)
                                     (position-new c3 r3)
                                     (position-new c2 r2)
                                     (position-new to-col to-row))))
           (gimp-selection-grow image 1)
           (when (image-has-selection? image) (image-fill! image))
           (image-selection-load! image sel)
           (image-selection-drop! image sel)))
        ((eq? type 'pointy)
         (let ((sel (image-selection-save image)))
           
           (image-fill! image)
           (image-selection-load! image sel)
           (image-selection-drop! image sel)))
        (else
         (error/misc 'image-draw-arrow!
                     (string-append "Invalid arrow type: "
                                    (value->string type)
                                    ", expects one of "
                                    "lines, filled, hollow, "
                                    "pointy, hollow-pointy")
                     (list image type from-col from-row to-col to-row 
                           head-width head-length)))))))

(define image-draw-arrow!
  (guard-proc 'image-draw-arrow!
              _image-draw-arrow!
              (list 'image 
                    'arrow-type
                    'real 'real 'real 'real 
                    'positive-real 'positive-real)
              (list image?
                    (r-s member? 
                         (list 'lines 'hollow 'filled 'pointy 'hollow-pointy))
                    real? real? real? real?
                    (and positive? real?) (and positive? real?))))

; [From gimp/image/image-draw-bezier.scm]

;;; Procedure:
;;;   image-draw-bezier!
;;; Parameters:
;;;   image, an image
;;;   positions, a list of positions of the form (p1 p2 ... pn)
;;;     [Alternately, you can simply provide p1 p2 ... pn as parameters]
;;; Purpose:
;;;   Draw a bezier curve using the positions
;;; Produces:
;;;   [Nothing, called for the side effect]
;;; Preconditions:
;;;   n is a multiple of 3
;;;   n is at least 6
;;;   positions are presented in the order Control Anchor Control Control 
;;;     Anchor Control ....
;;; Postconditions:
;;;   Image now contains a bezier curve as described by the positions.
;;; Practica:
;;;   (image-draw-bezier image 
;;;                      (position-new 0 10) (position-new 0 10) 
;;;                      (position-new 75 25) (position-new 80 45) 
;;;                      (position-new 20 90) (position-new 20 90))
(define image-draw-bezier!
  (lambda (image . positions)
    (let* ((data (if (position? (car positions)) positions (car positions))))
      (cond 
        ((not (image? image))
         (error "image-draw-bezier!: invalid image" image))
        (else
         (_image-draw-bezier! image data))))))

(define _image-draw-bezier!
  (lambda (image . positions)
    (let* ((data (if (position? (car positions)) positions (car positions)))
           (floats (positions->floats data))
           (vectors (car (gimp-vectors-new image "BEZIER")))
           (stroke-id (car (gimp-vectors-stroke-new-from-points
                            vectors 0 
                            (vector-length floats) floats
                            0))))
      (gimp-image-add-vectors image vectors -1)
      (gimp-edit-stroke-vectors (car (gimp-image-get-active-drawable image))
                                vectors)
      (gimp-image-remove-vectors image vectors))))

; [From gimp/image/image-draw-curve.scm]

;;; Procedure:
;;;   image-draw-curve!
;;; Parameters:
;;;   image, an image
;;;   source, a position
;;;   destination, a position
;;;   control, a position
;;; Purpose:
;;;   Draw a simple curve that starts at source, curves toward control, 
;;;   and then ends up at destination.
(define image-draw-curve!
  (lambda (image source destination control)
    (let ((positions (list source source control
                        control destination destination)))
      (image-draw-bezier! image positions))))

; [From gimp/image/image-draw-line.scm]

;;; Procedure:
;;;   image-draw-line!
;;; Parameters:
;;;   image, an image
;;;   col1, a real
;;;   row1, a real
;;;   col2, a real
;;;   row2, a real
;;; Purpose:
;;;   Draw a line with the current brush and the foreground color, 
;;;   beginning at (col1,row1) and ending at (col2, row2).
;;; Produces:
;;;   image, the original image
(define _image-draw-line!
  (lambda (image col1 row1 col2 row2)
    (gimp-paintbrush-default (image-get-layer image)
                             4                   
                             (vector col1 row1 col2 row2))
    (cond ((context-immediate-updates?) (context-update-displays!)))
    image))

(define image-draw-line!
  (guard-proc 'image-draw-line!
              _image-draw-line!
              (list 'image 'real 'real 'real 'real)
              (list image? real? real? real? real?)))

; [From gimp/image/image-draw-simple-curve.scm]

;;; Procedure:
;;;   image-draw-simple-curve!
;;; Parameters:
;;;   image, an image
;;;   source, a position
;;;   dest, a position
;;;   curviness, a real number
;;; Purpose:
;;;   Draw a curve from source to dest.  
;;; Produces:
;;;   image, the modified image
;;; Additional notes:
;;;   With a curviness of one, the curve is gently to the right.
;;;   To curve to the left, use a negative curviness.  For sharper
;;;   curves, use numbers with a larger absolute value.  For
;;;   shallower curves, use a smaller absolute value.
(define image-draw-simple-curve!
  (lambda (image source dest curviness)
    (let* ((delta-col (- (position-col source) (position-col dest)))
           (delta-row (- (position-row source) (position-row dest)))
           (mid-col (/ (+ (position-col source) (position-col dest)) 2))
           (mid-row (/ (+ (position-row source) (position-row dest)) 2))
           (control (position-new (+ mid-col (* curviness delta-row)) 
                                  (- mid-row (* curviness delta-col)))))
      (image-draw-bezier! image 
                          source source 
                          (position-interpolate source control 0.25)
                          (position-interpolate control dest 0.75) 
                          dest dest))))

; [From gimp/image/image-draw-text.scm]

;;; Procedure:
;;;   image-draw-text!
;;; Parameters:
;;;   image, an image
;;;   str, a string
;;;   font, a string
;;;   size, a positive number
;;;   x, an integer
;;;   y, an integer
;;;   angle, a number
;;; Purpose:
;;;   Add text specfied by str to an image, in the current foreground color, 
;;;   with the given font name and size in pixels, with the top left corner at 
;;;   (x,y)
;;; Produces:
;;;   image, the modified image
(define image-draw-text!
  (lambda (image str font size x y angle)
    (cond
      ((not (image? image))
       (error "image-draw-text!: invalid image" image))
      ((not (string? str))
       (error "image-draw-text!: expected string, got" str))
      ((or (not (number? size)) (not (positive? size)))
       (error "image-draw-text!: expected positive size, got" size))
      (else
       (gimp-text-fontname image (image-get-layer image)
                            x y
                            str
                            0 0
                            size
                            0
                            font)
       (gimp-drawable-transform-rotate-default (get-drawable image)
                                               angle
                                               FALSE ; no auto-center
                                               x y
                                               TRUE  ; interpolate
                                               0 ; TRANSFORM-RESIZE_ADJUST
                                               )
       (image-flatten! image)
       image))))

; [From gimp/image/image-duplicate.scm]

;;; Procedure:
;;;   image-duplicate
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   duplicates the given image
;;; Produces:
;;;   dup, a duplicate of image
;;; Preconditions:
;;;   image must be a valid image
;;; Package:
;;;   TEmily
(define image-duplicate
  (lambda (image)
    (cond
      ((not (image? image))
       (error "image.duplicate: parameter must be a valid image id."))
      (else
       (let ((new-image (gimp-image-duplicate image)))
             (car new-image))))))

; [From gimp/image/image-fill-bgcolor.scm]

;;; Procedure:
;;;   image-fill-bgcolor!
;;; Parameters:
;;;   image, a gimp image
;;; Purpose:
;;;   fill image's current selection (in the active layer) with the 
;;;   current background color
;;; Produces:
;;;   image, the modified image
;;; Preconditions
;;;   image is a valid image
;;; Postconditions
;;;   All the pixels of the active layer in the current selection are 
;;;   filled with the current background color.
(define image-fill-bgcolor! 
  (lambda (image)
    (cond
      ((not (image? image))
       (error "image-fill-bgcolor!: image must be a valid image"))
      (else 
       (gimp-edit-fill (image-get-layer image) 1)
       image))))

; [From gimp/image/image-fill.scm]

;;; Procedure
;;;   image-fill!
;;; Parameters
;;;   image, a gimp image
;;; Purpose
;;;   Fill image's current selection (in the active layer) with the current 
;;;   foreground color
;;; Produces
;;;   [Nothing; called for the side effect]
;;; Preconditions
;;;   image is a valid image
;;; Postconditions
;;;   All the pixels of the active layer in the current selection are filled 
;;;   with the current foreground color
(define _image-fill! 
  (lambda (image)
    (gimp-edit-fill (image-get-layer image) 0)
    (cond ((context-immediate-updates?) (context-update-displays!)))
    image))

(define image-fill!
  (guard-unary-proc 'image-fill! _image-fill! 'image image?))

; [From gimp/image/image-fill-selection.scm]

;;; Procedure
;;;   image-fill-selection!
;;; Parameters
;;;   image, a gimp image
;;; Purpose
;;;   Fill image's current selection (in the active layer) with the current 
;;;   foreground color
;;; Produces
;;;   [Nothing; called for the side effect]
;;; Preconditions
;;;   image is a valid image
;;; Postconditions
;;;   All the pixels of the active layer in the current selection are filled 
;;;   with the current foreground color
(define image-fill-selection! 
  (lambda (image)
    (cond
      ((not (image? image))
       (error "image-fill-selection!: invalid image" image))
      (else 
       (gimp-edit-fill (image-get-layer image) 0)
       image))))

; [From gimp/image/image-flatten.scm]

;;; Procedure:
;;;   image-flatten!
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Flatten the image: merge all of its layers into a single layer 
;;;   with the appearance of the combined layers
(define image-flatten!
  (lambda (image)
    (cond
     ((not (image? image))
      (error "image-flatten!: image must be a valid image"))
     (else
      (gimp-image-flatten image)))))

; [From gimp/image/image-new-layer.scm]

;; Procedure
;;  image-new-layer
;; Parameters
;;  image, an image; type, a constant; name, a string; opacity, an integer; mode, a constant
;; Purpose
;;  to create a new layer in the given image, using the given string as a name
;;  The type parameter specfices whether the layer colors will be RGB (0), RGBA (1), GRAY (2),
;;         GRAYA (3), INDEXED (4), or INDEXEDA (5) 
;;  The opacity parameter should be between 0 and 100
;;  The mode parameter determines how the appearance of the layer will affect the combined 
;;  appearance of the layers beneath it. Values are 
;;        NORMAL-MODE (0), DISSOLVE-MODE (1), BEHIND-MODE (2), MULTIPLY-MODE (3), SCREEN-MODE (4),
;;        OVERLAY-MODE (5), DIFFERENCE-MODE (6), ADDITION-MODE (7), SUBTRACT-MODE (8), 
;;        DARKEN-ONLY-MODE (9), LIGHTEN-ONLY-MODE (10), HUE-MODE (11), SATURATION-MODE (12), 
;;        COLOR-MODE (13), VALUE-MODE (14), DIVIDE-MODE (15), DODGE-MODE (16), BURN-MODE (17), 
;;        HARDLIGHT-MODE (18), SOFTLIGHT-MODE (19), GRAIN-EXTRACT-MODE (20), GRAIN-MERGE-MODE (21), 
;;        COLOR-ERASE-MODE (22), ERASE-MODE (23), REPLACE-MODE (24), ANTI-ERASE-MODE (25)
;;  In addition, newlayer selects everything on the new layer, clears it, and selects nothing again,
;;  since the layer shows up with garbage. This means any previous selection will be lost.


(define image-new-layer 
  (lambda (image type name opacity mode)
    (let ((layer (car (gimp-layer-new 
                       image 
                       (car (gimp-drawable-width (car (gimp-image-get-active-drawable image))))
                       (car (gimp-drawable-height (car (gimp-image-get-active-drawable image))))
                       type
                       name
                       opacity
                       mode))))
      (begin
        (gimp-image-add-layer image layer -1)
        (gimp-image-set-active-layer image layer)
        (select-all image)
        (clear-selection image)
        (select-nothing image)))))

; [From gimp/image/image-paste.scm]

;;; Procedure:
;;;   image-paste!
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Pastes the contents of the edit buffer into the selected
;;;   region in image.
;;; Produces:
;;;   [Nothing; called for the side effect]
;;; Preconditions:
;;;   image must be a valid image
;;;   the edit buffer must contain something 
;;;     (from image.copy-selection)
;;;   There must be a selection in image.
;;; Postconditions:
;;;   The selected area of image now contains the edit buffer.
;;;   The edit buffer is unchanged.
;;;   The selection of image is cleared.
;;; Practica:
;;;  (define source (image-load "/home/rebelsky/Desktop/istrauber.jpg"))
;;;  (define target (image-new 200 200))
;;;  (image-show source)
;;;  (image-show target)
;;;  (image-select-rectangle! source REPLACE 90 100 50 20)
;;;  (image-copy-selection source)
;;;  (image-select-rectangle! target REPLACE 0 0 50 20)
;;;  (image-paste! target)
;;;  (image-select-rectangle! target REPLACE 50 10 30 10)
;;;  (image-paste! target)
;;;  (image-select-rectangle! target REPLACE 0 100 90 90)
;;;  (image-paste! target)
;;;  (context-update-displays!)
(define image-paste!
  (lambda (image)
    (cond
      ((not (image? image))
       (error "image-paste!: Parameter is not an image."))
      (else
       (gimp-edit-paste (image-get-layer image) 1)
       (image-flatten! image)
       image))))

; [From gimp/image/image-remove-layer.scm]

;;; Procedure:
;;;   image-remove-layer!
;;; Parameters:
;;;   image, an image-id
;;;   layer, a layer-id
;;; Purpose:
;;;   Remove layer from image
;;; Produces:
;;;   [Nothing; called for the side effect]
;;; Preconditions:
;;;  image must be a valid image
;;;  layer must exist in image
(define image-remove-layer!
  (lambda (image layer)
    (cond
      ((not (image? image))
       (error "image-remove-layer!: invalid image"))
      ((not (image-contains-layer? image layer))
       (error "imageremove-layer!: image does not contain layer")) 
      (else
       (gimp-image-remove-layer image layer)
       image))))

; [From gimp/image/image-rotate-selection.scm]

;;; Procedure:
;;;   image-rotate-selection!
;;; Parameters:
;;;   image, an image
;;;   col, an integer
;;;   row, an integer
;;;   angle, a real number
;;; Purpose:
;;;   Rotate the current selection around (col,row) by an angle
;;;   of theta (expressed in radians).
;;; Produces:
;;;   [Nothing; Called for the side effects.]
(define image-rotate-selection!
  (lambda (image col row angle)
    (gimp-drawable-transform-rotate-default
      (image-get-layer image)
      angle
      FALSE ; Do not use the center of the selection as the center
      col row
      TRUE  ; Interpolate
      0     ; TRANSFORM-RESIZE-ADJUST
      )))

; [From gimp/image/image-scale.scm]

;;; Procedure:
;;;   image-scale!
;;; Parameters:
;;;   image, an image, width, an integer, height, an integer
;;; Purpose:
;;;   To scale an image so that its new width and height are equal to the 
;;;   given parameters
;;; Produces:
;;;   [Nothing; called for the side effect]
;;; Preconditions
;;;   image is a valid gimp image, width and height are positive numbers
;;; Postconditions
;;;   image's width and height are now equal to the given parameters
(define image-scale!
  (lambda (image width height)
    (cond
      ((not (image? image))
       (error "scale: image must be a valid image"))
      ((or (< width 0) (< height 0))
       (error "scale: width and height must both be greater than zero"))
      (else
       (gimp-image-scale image width height)
       image))))

; [From gimp/image/image-select-ellipse.scm]

;;; Procedure:
;;;   image-select-ellipse!
;;; Parameters:
;;;   image, a gimp image
;;;   operation, one of the valid GIMP operations
;;;   left, an integer
;;;   top, an integer
;;;   width, an integer;
;;;   height, an integer
;;; Purpose:
;;;   Select an ellipse according to the selection mode specfied by 
;;;   operation, inscribed in the rectangle with the given top left corner, 
;;;   and the given width and height.
;;; Produces:
;;;   [Noting; called for the side effect]
;;; Preconditions:
;;;   image is a valid gimp image
;;;   operation is one of: ADD, SUBTRACT, REPLACE, and INTERSECT.
;;;   left, top, width, and height describe an ellipse.
;;; Postconditions
;;;   An appropriate ellipse is selected.
(define image-select-ellipse!
  (lambda (image operation left top width height)
    (image-validate-selection! image operation left top width height
                               "image-select-ellipse!")
    (gimp-ellipse-select image
                         left top
                         width height
                         (selection-op operation)
                         1 1 1)
    ; (context-update-displays!)
    image))

; [From gimp/image/image-select-inverse.scm]

;;; Procedure:
;;;   image-select-inverse!
;;; Parameters:
;;;   image, a gimp image
;;; Purpose:
;;;   Invert the current selection, so that all previously unselected 
;;;   pixels are now selected, and all previously selected pixels are 
;;;   now unselected.
;;; Produces;
;;;   [Nothing; Called for the side effects]
;;; Preconditions:
;;;  image is a valid image
(define image-select-inverse!
  (lambda (image)
    (cond 
      ((not (image? image))
       (error "select-inverse: image must be a valid image"))
      (else
       (gimp-selection-invert image)
       image))))

; [From gimp/image/image-select-nothing.scm]

;;; Procedure:
;;;   image-select-nothing!
;;; Parameters:
;;;   image, a gimp image
;;; Purpose:
;;;   De-select all pixels in the image
;;; Produces:
;;;   [Nothing; called for the side effect]
;;; Preconditions:
;;;   image is a valid image
;;; Postconditions:
;;;   No pixels in image are selected.
(define image-select-nothing!
  (lambda (image)
    (cond
      ((not (image? image))
       (error "select-nothing: invalid image"))
      (else
       (gimp-selection-none image)
       image))))

; [From gimp/image/image-select-polygon.scm]

;;; Procedure:
;;;   image-select-polygon!
;;; Parameters:
;;;   image, an image
;;;   operation, one of the selection operations (ADD, SUBTRACT,
;;;     INTERSECT, REPLACE)
;;;   positions, a list of positions 
;;;     OR
;;;   pos1 ... posn, n positions
;;; Purpose:
;;;   Select the polygon bounded by the given positions.
;;; Produces:
;;;   image, the image
(define _image-select-polygon!
  (lambda (image operation first . rest)
    (let* ((positions (if (null? rest) first (cons first rest)))
           (floats (positions->floats positions))
           (len (vector-length floats)))
      (gimp-free-select image (vector-length floats) floats
                        operation 1 0 0))))

(define image-select-polygon!
  (let ((operations (list ADD SUBTRACT INTERSECT REPLACE)))
    (lambda (image operation first . rest)
      (let* ((positions (if (null? rest) first (cons first rest)))
             (params (list image operation positions)))
        (cond
          ((not (image? image))
           (error/parameter-type 'image-select-polygon! 1 'image params))
          ((not (member? operation operations))
           (error/parameter-type 'image-select-polygon! 2 'selection-op params))
          ((not (list? positions))
           (error/parameter-type 'image-select-polygon! 3 
                                 'list-of-positions params))
          ((not (all position? positions))
           (error/parameter-type 'image-select-polygon! 3
                                 'list-of-positions  params))
          ((or (null? positions) 
               (null? (cdr positions)) 
               (null? (cdr (cdr positions))))
           (error/misc 'image-select-polygon!
                       (string-append "Requires at least 3 positions, given "
                                      (number->string (length positions)))
                       params))
          (else
           (apply _image-select-polygon! params)))))))

; [From gimp/image/image-select-px.scm]

;; Procedure
;;   image-select-px!
;; Parameters
;;   image, a gimp image
;;   op, an operation (ADD, SUB, INTERSECT, or REPLACE)
;;   width, the x-coord of a pixel in img
;;   height, the y-coord of a pixel in img
;; Purpose
;;   select the pixel at (x y) in image
;; Produces
;;   nothing
;; Preconditions
;;  image is a valid image
;;  op must be either ADD, SUB, INTERSECT or REPLACE
;;  width and height must both be integers.
(define image-select-px!
  (lambda (image op width height)
    (select-rectangle image
                      op
                      width
                      height
                      1
                      1)))

; [From gimp/image/image-select-rectangle.scm]

;;; Procedure:
;;;   image-select-rectangle!
;;; Parameters:
;;;   image, a gimp image
;;;   operation, one of ADD, SUBTRACT, INTERSECT, REPLACE
;;;   left, an integer
;;;   top, an integer
;;;   width, an integer
;;;   height, an integer
;;; Purpose:
;;;   Select a rectangle according to the selection mode specfied by 
;;;   operation, beginning in the given top left corner, with the given 
;;;   width and height.
;;; Produces:
;;;   [Nothing; called for the side-effect
;;; Preconditions:
;;;   image is a valid gimp image
;;;    operation is one of: ADD, SUBTRACT, REPLACE, INTERSECT
;;;   left and top are integers, width and height are nonnegative integers
;;;   left, top, width, and height descirbe an area onscreen in image.
;;; Postconditions:
;;;   The given rectangle is now selected.
(define image-select-rectangle!
  (lambda (image operation left top width height)
    (image-validate-selection! image operation left top width height
                               "image-select-rectangle!")
    (gimp-rect-select image
                      left top
                      width height
                      (selection-op operation)
                      0 0)
    (context-update-displays!)
    image))

; [From gimp/image/image-select-text.scm]

;;; Procedure:
;;;   image-select-text!
;;; Parameters:
;;;   image, an image id
;;;   str, a string
;;;   col, an integer
;;;   row, an integer
;;;   font, a string
;;;   size, an integer
;;; Purpose:
;;;   Creates a selection based on the given text.
;;; Produces:
;;;   image, the modified image
(define image-select-text!
  (lambda (image str col row font size)
    (let ((text-layer
           (car (gimp-text-fontname image
                                    (image-get-layer image)
                                    col row
                                    str
                                    0
                                    0
                                    size
                                    0
                                    font))))
       (gimp-layer-add-alpha text-layer)
       (gimp-selection-layer-alpha text-layer)
       (image-flatten! image)
       image)))

; [From gimp/image/image-show.scm]

;;; Procedure:
;;;   image-show
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Displays the image (which may have been modified behind the scenes).
;;; Produces:
;;;   image, the original image.
;; Preconditions:
;;;   image must be a valid image (created by create-image or image-load).
(define _image-show 
  (lambda (image)
    (gimp-display-new (image-id image))
    image))

(define image-show
  (guard-unary-proc 'image-show
                    _image-show
                    'image
                    image?))

; [From gimp/image/image-stroke.scm]

;;; Procedure:
;;;   image-stroke!
;;; Parameters:
;;;   image, a gimp image
;;; Purpose:
;;;   Trace the edge of the selected region of the current image
;;;   (in the active layer) using the current brush and foreground
;;    color.
;;; Produces:
;;;   image, the updated image
;;; Preconditions:
;;;   image is a valid image
;;; Postconditions:
;;;   The image has been stroked, as in the stroke menu item.
(define _image-stroke!
  (lambda (image)
    (gimp-edit-stroke (image-get-layer image))
    (cond ((context-immediate-updates?) (context-update-displays!)))
    image))

(define image-stroke!
  (guard-unary-proc 'image-stroke! _image-stroke! 'image image?))

; [From gimp/image/image-stroke-selection.scm]

;;; Procedure:
;;;   image-stroke-selection!
;;; Parameters:
;;;   image, a gimp image
;;; Purpose:
;;;   Trace the edge of the selected region of the current image
;;;   (in the active layer) using the current brush and foreground
;;    color.
;;; Produces:
;;;   image, the updated image
;;; Preconditions:
;;;   image is a valid image
;;; Postconditions:
;;;   The image has been stroked, as in the stroke menu item.
(define image-stroke-selection!
  (lambda (image)
    (cond 
      ((not (image? image))
       (error "image-stroke-selection!: invalid image" image))
      (else
       (gimp-edit-stroke (image-get-layer image))))))

; [From gimp/image/image-transform-pixel.scm]

;;; Procedure:
;;;   image-transform-pixel!
;;; Parameters:
;;;   image, an image identifier
;;;   col, an integer
;;;   row, an integer
;;;   ctrans, a function from rgb colors to rgb colors
;;; Purpose:
;;;   Transform one pixel in the image
;;; Produces:
;;;   [Nothing; Called for the side effect]
;;; Preconditions:
;;;   image names a valid image.
;;;   0 <= col < (image-width image)
;;;   0 <= row < (image-height image)
;;;   For any rgb color, c, (rgb? (ctrans c))
;;; Postconditions:
;;;   Let c be (image.get-pixel image col row) prior to this call.
;;;   After this call, (image.get-pixel image col row) is now (ctrans c).
(define image-transform-pixel!
  (lambda (image col row ctrans)
    (image-set-pixel! image col row
                      (ctrans (image-get-pixel image col row)))))

; [From gimp/image/image-validate-selection.scm]

;;; Procedure:
;;;   image-validate-selection!
;;; Purpose:
;;;   Validate the typical parameters to a selection routine, such
;;;   as image.select-ellipse! or image.select-rectangle!.
;;; Parameters:
;;;   image, an image
;;;   operation, one of ADD, SUBTRACT, REPLACE, INTERSECT
;;;   left, an integer
;;;   top, an integer
;;;   width, an integer
;;;   height, an integer
;;;   proc, a string
;;; Produces:
;;;   [Nothing, called for the side effect]
;;; Postconditions:
;;;   If any parameter is invalid, throws an exception.
;;;   Otherwise, it should be safe to do the selection.
(define image-validate-selection!
  (lambda (image operation left top width height proc)
    (let ((crash (lambda (message) (error (string-append proc ": " message)))))
      (cond
        ((not (image? image))
         (crash "invalid image"))
        ((not (member operation (list ADD SUBTRACT REPLACE INTERSECT)))
         (crash "operation must be ADD, SUBTRACT, REPLACE, or INTERSECT" 
                (number->string operation)))
;       ((or (not (integer? left)) (not (integer? top)) 
;            (not (integer? width)) (not (integer? height)))
;        (crash "left, top, width, and height must all be integers"))
        ((or (< width 1) (< height 1))
         (crash "width and height must be at least 1"))
        ((or (>= left (image-width image))
             (>= top (image-height image))
             (>= 0 (+ left width))
             (>= 0 (+ top height)))
         (crash "selection is outside of the bounds of the image"))))))

; [From gimp/image/image-write.scm]

;;; Procedure:
;;;   image-write!
;;; Parameters:
;;;   image, an image id
;;;   str, a string
;;;   col, an integer
;;;   row, an integer
;;;   font, a string
;;;   size, an integer
;;; Purpose:
;;;   Writes the given text on the image.
;;; Produces:
;;;   image, the modified image.
(define image-write!
  (lambda (image str col row font size)
     (gimp-text-fontname image
                         (image-get-layer image)
                         col row
                         str
                         0
                         0
                         size
                         0
                         font)
     (image-flatten! image)
     image))

; [From gimp/image/image-to-drawing.scm]

;;; Procedure:
;;;   image->drawing
;;; Parameters:
;;;   image, an image
;;; Purpose:
;;;   Create a new drawing, based on image
;;; Produces:
;;;   drawing, a drawing
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   (drawing->image drawing (image-width image) (image-height image))
;;;     will appear the same as image
(define _image->drawing
  (lambda (image)
    (apply drawing-group
           (image->list image
                        (lambda (col row color)
                          (drawing-recolor (drawing-rectangle col row 1 1)
                                           color))))))

(define image->drawing
  (guard-unary-proc 'image->drawing
                    _image->drawing
                    'image
                    image?))


; +--------------------------------+--------------------------------------------
; | GIMP Extensions for "Drawings" |
; +-----------------------------------+

; Procedures that work with the MediaScheme "drawing" type.

; [From gimp/drawing/drawing-on-image-p.scm]

;;; Procedure:
;;;   drawing-on-image?
;;; Parameters:
;;;   drawing, a drawing
;;;   image, an image
(define _drawing-on-image?
  (lambda (drawing image)
    (and (>= (drawing-right drawing) 0)
         (>= (drawing-bottom drawing) 0)
         (< (drawing-left drawing) (image-width image))
         (< (drawing-top drawing) (image-height image)))))

(define drawing-on-image?
  (guard-proc 'drawing-on-image?
              _drawing-on-image?
              (list 'drawing 'image)
              (list drawing? image?)))

; [From gimp/drawing/drawing-to-image.scm]

;;; Procedure:
;;;   drawing->image
;;; Parameters:
;;;   drawing, a drawing
;;;   width, an integer
;;;   height, an integer
;;; Purpose:
;;;   Creates an image that shows the given drawing.
;;; Produces:
;;;   img, an image id
;;; Preconditions:
;;;   width > 0
;;;   height > 0
;;; Postconditions:
;;;   img, when shown, contains the given drawing.
(define drawing->image
  (lambda (drawing width height)
    (let ((img (image-new width height)))
      (drawing-render! drawing img)
      img)))

; [From gimp/drawing/drawing-render.scm]

;;; Procedure:
;;;   drawing-render!
;;; Parameters:
;;;   image, an image
;;;   drawing, a drawing
;;; Purpose:
;;;   Render drawing on image.
;;; Produces:
;;;   image, the input image
;;; Preconditions:
;;;   image is a valid image.
;;;   drawing is a valid drawing.
;;; Postconditions:
;;;   image has been extended by the appropriate drawing.
(define _drawing-render!
  (lambda (drawing image)
    (let ((type (drawing-type drawing)))
      (cond
        ((eq? type 'blank))
        ((eq? type 'group)
         (_drawing-group-render! drawing image))
        ((eq? type 'line)
         (drawing-line-render! drawing image))
        ((eq? type 'rule)
         (drawing-rule-render! drawing image))
        ((or (eq? type 'ellipse) (eq? type 'rectangle))
         (_drawing-shape-render! drawing image))
        (else
         (error "drawing-render!: Unable to render" drawing))))
    image))

(define drawing-render!
  (lambda (drawing image)
    (validate-params! 'drawing-render!
                      (list 'drawing 'image)
                      (list drawing? image?)
                      (list drawing image))
    (cond
      ((and (not (eq? (drawing-type drawing) 'group))
            (not (drawing-on-image? drawing image)))
       (error "drawing-render!: Drawing does not fit within image bounds"
              (list
               'image-width: (image-width image)
               'image-height: (image-height image)
               'drawing-left: (drawing-left drawing)
               'drawing-top: (drawing-top drawing)
               'drawing-right: (drawing-right drawing)
               'drawing-bottom: (drawing-bottom drawing)
               )))
      (else
       (_drawing-render! drawing image)))))

; [From gimp/drawing/drawing-group-render.scm]

;;; Procedure:
;;;   drawing-group-render!
;;; Parameters:
;;;   image, an image
;;;   drawing, a gropued drawing
;;; Purpose:
;;;   Render drawing on image.
;;; Produces:
;;;   image, the input image
;;; Preconditions:
;;;   image is a valid image.
;;;   drawing is a valid drawing.
;;; Postconditions:
;;;   image has been extended by the appropriate drawing.
(define _drawing-group-render!
  (lambda (drawing image)
    (foreach! (lambda (d)
                (and (or (eq? (_drawing-type d) 'group)
                         (_drawing-on-image? d image))
                     (_drawing-render! d image)))
              (_drawing-members drawing))))

(define drawing-group-render!
  (guard-proc 'drawing-group-render!
              _drawing-group-render!
              (list 'drawing-group 'image)
              (list drawing-group? image?)))

; [From gimp/drawing/drawing-line-render.scm]

;;; Procedure:
;;;   drawing-line-render!
;;; Parameters:
;;;   image, an image
;;;   line, a drawing line
;;; Purpose:
;;;   Render line on image.
;;; Produces:
;;;   image, the input image
;;; Preconditions:
;;;   (image? image)
;;;   (drawing? drawing)
;;;   (drawing-line? drawing)
;;; Postconditions:
;;;   image has been extended by the appropriate drawing.
(define _drawing-line-render!
  (lambda (line image)
    (let ((c1 (drawing-line-left line))
          (r1 (drawing-line-top line))
          (c2 (drawing-line-right line))
          (r2 (drawing-line-bottom line))
          (newcolor (drawing-line-color line))
          (h (drawing-line-hstretch line))
          (v (drawing-line-vstretch line))
          (savecolor (context-get-fgcolor)))

      ; Change the fgcolor if necessary
      (cond ((not (equal? newcolor savecolor))
             (context-set-fgcolor! newcolor)))

      ; Decide how we're going to draw the line
      (cond
        ; If h and v are zero, we just draw a rule
        ((and (zero? h) (zero? v))
         (let ((savebrush (context-get-brush))
               (newbrush "Circle (01)"))
           ; Change the brush if necessary
           (cond ((not (string=? newbrush savebrush))
                  (context-set-brush! newbrush)))
           ; Draw the line
           (image-draw-line! image c1 r1 c2 r2)
           ; Restore the brush if necessary
           (cond ((not (string=? newbrush savebrush))
                  (context-set-brush! savebrush)))))
        ; Otherwise, we do some fancy footwork to draw the line
        (else
         (let* ((hh (/ h 2))
                (vv (/ v 2))
                (points (list (position-new (- c1 hh) (+ r1 vv))
                              (position-new (+ c1 hh) (- r1 vv))
                              (position-new (+ c2 hh) (- r2 vv))
                              (position-new (- c2 hh) (+ r2 vv)))))
           (image-select-polygon! image REPLACE points)
           (image-fill-selection! image)
           (image-select-nothing! image))))

      ; Restore the fgcolor if necessary
      (cond ((not (equal? newcolor savecolor))
             (context-set-fgcolor! newcolor)))

      ; And return the modified image
      image)))

(define drawing-line-render!
  (guard-proc 'drawing-line-render!
              _drawing-line-render!
              (list 'drawing-line 'image)
              (list drawing-line? image?)))

; [From gimp/drawing/drawing-rule-render.scm]

;;; Procedure:
;;;   drawing-rule-render!
;;; Parameters:
;;;   image, an image
;;;   drawing, a rule drawing
;;; Purpose:
;;;   Render drawing on image.
;;; Produces:
;;;   image, the input image
;;; Preconditions:
;;;   (image? image)
;;;   (drawing? drawing)
;;;   (drawing-line? drawing)
;;; Postconditions:
;;;   image has been extended by the appropriate drawing.
(define _drawing-rule-render!
  (lambda (drawing image)
    (let ((c1 (drawing-rule-left drawing))
          (r1 (drawing-rule-top drawing))
          (c2 (drawing-rule-right drawing))
          (r2 (drawing-rule-bottom drawing))
          (saved-color (and (context-preserve?) (context-get-fgcolor)))
          (saved-brush (and (context-preserve?) (context-get-brush))))
      (context-set-fgcolor! "grey")
      (gimp-pencil (image-get-layer image)
                   4
                   (vector c1 r1 c2 r2))
      (context-set-brush! "Circle (01)")
      ; (image-draw-line! image c1 r1 c2 r2)
      (cond (saved-color (context-set-fgcolor! saved-color)))
      (cond (saved-brush (context-set-brush! saved-brush)))
      image)))

(define drawing-rule-render!
  (guard-proc 'drawing-rule-render!
              _drawing-rule-render!
              (list 'drawing-rule 'image)
              (list drawing-rule? image?)))

; [From gimp/drawing/drawing-shape-render.scm]

;;; Procedure:
;;;   drawing-shape-render!
;;; Parameters:
;;;   image, an image
;;;   drawing, a drawing shape (ellipse or rectangle)
;;; Purpose:
;;;   Render drawing on image.
;;; Produces:
;;;   image, the input image
;;; Preconditions:
;;;   image is a valid image.
;;;   drawing is a valid drawing.
;;; Postconditions:
;;;   image has been extended by the appropriate drawing.
(define _drawing-shape-render!
  (lambda (drawing image)
    (let ((select! (if (eq? (drawing-type drawing) 'ellipse) 
                       image-select-ellipse! 
                       image-select-rectangle!))
          (fgcolor (context-get-fgcolor)))
      (select! image REPLACE
               (drawing-left drawing) (drawing-top drawing)
               (drawing-width drawing) (drawing-height drawing))
      (context-set-fgcolor! (drawing-color drawing))
      (if (drawing-filled? drawing)
          (image-fill-selection! image)
          (let ((brush (context-get-brush)))
            (context-set-brush! (drawing-brush drawing))
            (image-stroke-selection! image)
            (cond ((context-preserve?) (context-set-brush! brush)))))
      (image-select-nothing! image)
      (cond ((context-preserve?) (context-set-fgcolor! fgcolor))))))

(define drawing-shape-render!
  (guard-proc 'drawing-shape-render!
              _drawing-shape-render!
              (list 'drawing-shape 'image)
              (list drawing-shape? image?)))

; [From gimp/drawing/drawing-to-file.scm]

;;; Procedure:
;;;   drawing->file
;;; Parameters:
;;;   drawing, a drawing
;;;   fname, a string
;;; Purpose:
;;;   Renders the drawing to an "appropriate sized" image and 
;;;     saves the image in a file.
;;; Produces:
;;;   drawing, the original drawing
;;; Preconditions:
;;;   fname names a valid file location
;;;   The user has permission to create that file.
;;; Postconditions:
;;;   The file contains a PNG rendering of the image.
;;; Philosophy:
;;;   Designed mostly to make it easier for the authors to create figures.
(define _drawing->file
  (lambda (drawing fname)
    (image-save 
     (drawing->image drawing 
                     (inexact->exact (ceiling (drawing-right drawing)))
                     (inexact->exact (ceiling (drawing-bottom drawing))))
     fname)))

(define drawing->file
  (guard-proc 'drawing->file
              _drawing->file
              (list 'drawing 'string/filename)
              (list drawing? string?)))


; +--------+--------------------------------------------------------------------
; | Layers |
; +--------+

; [From gimp/layer/layer-p.scm]

;;; Procedure:
;;;   layer?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determines if val names a layer (or other drawable)
(define layer?
  (lambda (val)
    (and  (integer? val) (gimp-drawable-is-valid val))))


; +---------------------+-------------------------------------------------------
; | GIMP RGB Extensions |
; +---------------------+

; [From gimp/rgb/rgb-to-color-name.scm]

;;; Procedure:
;;;   rgb->color-name
;;; Parameters:
;;;   color, an rgb color
;;; Purpose:
;;;   Find the name of a color that is similar to the given color.
;;; Produces:
;;;   name, a string
;;; Preconditions:
;;;   _accio_colors_ is defined as an association list of named colors.
;;; Postconditions:
;;;   There is no color name k for which
;;;     (rgb-distance-squared color (name->color k)
;;;        < (rgb-distance-squared color (name->color name)
(define _rgb->color-name
  (let* ((color-names (context-list-colors))
         (color-values (map color-name->rgb color-names))
         (distance rgb-distance-squared))
    (lambda (rgb)
      (let kernel ((guess-name (car color-names))
                   (guess-distance (distance rgb (car color-values)))
                   (remaining-names (cdr color-names))
                   (remaining-values (cdr color-values)))
               (if (null? remaining-names)
                   guess-name
                   (let ((next-distance 
                         (distance rgb (car remaining-values))))
                     (if (< next-distance guess-distance)
                         (kernel (car remaining-names)
                                 next-distance 
                                 (cdr remaining-names)
                                 (cdr remaining-values))
                         (kernel guess-name
                                 guess-distance 
                                 (cdr remaining-names)
                                 (cdr remaining-values)))))))))

(define rgb->color-name (guard-rgb-proc 'rgb->color-name _rgb->color-name))

; [From gimp/rgb/gimp-rgb-colors.scm]

;;; Values:
;;;   RGB-RED
;;;   RGB-GREEN
;;;   RGB-BLUE
;;;   RGB-VIOLET
;;;   ...
;;; Type:
;;;   RGB color
;;; Summary:
;;;   Constants that correspond to the various GIMP colors.
(for-each (lambda (name)
            (eval (list 'define 
                        (string->symbol (string-append "RGB-" 
                                                       (string-upcase name)))
                        (color->rgb name))))
          ; (context-list-colors)
          (list "black" "white" 
                "red" "orange" "yellow" "green" "blue" "indigo" "violet")
          )



; +-------------------------------+---------------------------------------------
; | Rectangular Regions in Images |
; +-------------------------------+

; [From gimp/region/region-calculate-pixels.scm]

;;; Procedure:
;;;   region-calculate-pixels!
;;; Parameters:
;;;   image, an image
;;;   left, an integer
;;;   top, an integer
;;;   width, an integer
;;;   height, an integer
;;;   colorfun, a procedure of the form (lambda (col row) color)
;;; Purpose:
;;;   Iterate through the positions in a rectangular region in the image, 
;;;     row by row, setting the color at each position to (colorfun col row).
;;; Produces:
;;;   image, the updated image
;;; Preconditions:
;;;   image is a valid, open image
;;;   0 <= left < (image-width image)
;;;   0 <= top < (image-height image)
;;;   0 < width
;;;   0 < height
;;;   (+ left width) <= (image-width image)
;;;   (+ top height) <= (image-height image)
;;;   colorfun is a procedure of the form (lambda (col row) color) 
;;; Postconditions:
;;;   For each position in the region, if (colorfun col row) is not
;;;     rgb-transparent, then the pixel at (col,row) is now
;;;     (colorfun col row).
;;; Philosophy:
;;;   Unlike some other procedures, which may iterate a region in an
;;;   undefined order, region-calculate-pixels! is guaranteed to iterate
;;;   the region row by row.  However, it is likely to be a bit slower than
;;;   region-compute-pixels!.
(define _region-calculate-pixels!
  (lambda (image left top width height colorfun)
    (let ((iwidth (image-width image))
          (iheight (image-height image))
          (layer (image-get-layer image))
          (right (+ left width))
          (bottom (+ top height)))
      ; Check more preconditions
      (cond
        ((>= left iwidth)
         (error "region-calculate-pixels!: Left margin must be < width, given" left))
        ((>= top iheight)
         (error "region-calculate-pixels!: Top margin must be < height, given" left))
        ((> right iwidth) 
         (error "region-calculate-pixels!: width too large, given" width))
        ((> bottom iheight) 
         (error "region-calculate-pixels!: height too large, given" height)))
      ; Life is faster if you cache the layer
      (let ((cache (image-cache image)))
        (let kernel ((c left)
                     (r top))
          (cond
            ((>= r bottom)
             (cache-flush! cache)
             image)
            ((>= c right)
             (kernel left (+ r 1)))
            (else
             (let ((color (colorfun c r)))
               (when (not (= color rgb-transparent))
                 (cache-set-pixel! cache c r color))
               (kernel (+ c 1) r)))))))))

(define region-calculate-pixels!
  (let ((non-negative-integer? (^and integer? exact? (^not negative?)))
        (positive-integer? (^and integer? exact? positive?)))
    (guard-proc 'region-calculate-pixels!
                _region-calculate-pixels!
                (list 'image 
                      'non-negative-integer 'non-negative-integer 
                      'positive-integer 'positive-integer 
                      'procedure)
                (list image? 
                      non-negative-integer? non-negative-integer? 
                      positive-integer? positive-integer? 
                      procedure?))))  

; [From gimp/region/region-compute-pixels.scm]

;;; Procedure:
;;;   region-compute-pixels!
;;; Parameters:
;;;   image, an image
;;;   left, an integer
;;;   top, an integer
;;;   width, a postiive integer
;;;   height, a positive integer;;;   pos2color, a function of the form (lambda (col row) color)
;;; Purpose:
;;;   Compute all of the pixels in the region by applying fun to each
;;;      (col,row) pair in the region and using the result as an RGB
;;;      color.
;;; Produces:
;;;   image, the same image
(define region-compute-pixels!
  (lambda (image left top width height pos2color)
    (let ((id (image-id image)))
      (cond
        ((not id)
         (error "region-compute-pixels! Expected image for first parameter, got " 
                image))
        ((or (not (integer? width)) (< width 1))
         (error "region-compute-pixels! Expects positive width, got " width))
        ((or (not (integer? height)) (< height 1))
         (error "region-compute-pixels! Expects postive height, got " height))
        (else
         (let ((sel (image-selection-save id)))
           (image-select-rectangle! image REPLACE left top width height)
           (mgimp-layer-compute-pixels-in-selection! (image-get-layer id)
                                                     pos2color)
           (image-selection-load! id sel)
           (image-selection-drop! id sel)
           image))))))

; [From gimp/region/region-scan.scm]

;;; Procedure:
;;;   region-scan
;;; Parameters:
;;;   image, an image
;;;   left, an integer
;;;   top, an integer
;;;   width, an integer
;;;   height, an integer
;;;   proc!, a procedure of the form (lambda (col row color) ...)
;;; Purpose:
;;;   Iterate through a rectangular region in the image, applying
;;;     proc! at each position.
;;; Produces:
;;;   image, the same image
;;; Preconditions:
;;;   image is a valid, open image
;;;   0 <= left < (image-width image)
;;;   0 <= top < (image-height image)
;;;   0 < width
;;;   0 < height
;;;   (+ left width) <= (image-width image)
;;;   (+ top height) <= (image-height image)
;;;   proc! is a procedure of the form (lambda (col row color) ...)
;;; Postconditions:
;;;   proc! has been applied to every <col,row,color> triplet.
(define _region-scan
  (lambda (image left top width height proc!)
    (let ((iwidth (image-width image))
          (iheight (image-height image))
          (layer (image-get-layer image))
          (right (+ left width))
          (bottom (+ top height)))
      ; Check more preconditions
      (cond
        ((>= left iwidth)
         (error "region-scan: Left margin must be < width, given" left))
        ((>= top iheight)
         (error "region-scan: Top margin must be < height, given" left))
        ((> right iwidth) 
         (error "region-scan: width too large, given" width))
        ((> bottom iheight) 
         (error "region-scan: height too large, given" height)))
      ; Life is faster if you cache the layer
      (let ((cache (image-cache image)))
        (let kernel ((c left)
                     (r top))
          (cond
            ((>= r bottom)
             (cache-flush! cache)
             image)
            ((>= c right)
             (kernel left (+ r 1)))
            (else
             (let ((color (cache-get-pixel cache c r)))
               (proc! c r color)
               (kernel (+ c 1) r)))))))))

(define region-scan
  (let ((non-negative-integer? (^and integer? exact? (^not negative?)))
        (positive-integer? (^and integer? exact? positive?)))
    (guard-proc 'region-scan
                _region-scan
                (list 'image 
                      'non-negative-integer 'non-negative-integer 
                      'positive-integer 'positive-integer 
                      'procedure)
                (list image? 
                      non-negative-integer? non-negative-integer? 
                      positive-integer? positive-integer? 
                      procedure?))))  

; [From gimp/region/region-transform.scm]

;;; Procedure:
;;;   region-transform!
;;; Parameters:
;;;   image, an image
;;;   left, an integer
;;;   top, an integer
;;;   width, a postiive integer
;;;   height, a positive integer
;;;   color-transform, a color transformation
;;; Purpose:
;;;   Transform a width-by-height region in image, starting at (left,top)
;;;     by applying color-transform to each pixel in the region.
;;; Produces:
;;;   image, the same image
(define region-transform!
  (lambda  (image left top width height color-transform)
    (let ((id (image-id image)))
      ; Check preconditions.
      (cond 
        ((not id)
         (error "region-transform!: Expects an image, received " image))
        ((<= width 0)
         (error "region-transform!: Expects positive width, received " width))
        ((<= height 0)
         (error "region-transform!: Expects positive height, received " height))
        (else
         ; Save the selection
         (let ((sel (image-selection-save id)))
           ; Select an appropriate rectangular region
           (image-select-rectangle! id REPLACE left top width height)
           ; Do the real work
           (mgimp-layer-slurp-from-selection! (image-get-layer id)
                                              (image-get-layer id)
                                              color-transform
                                              0
                                              0)
           ; Restore the selection
           (image-selection-load! id sel)
           (image-selection-drop! id sel)
           ; And we're done
           image))))))

; [From gimp/region/region-variant.scm]

;;; Procedure:
;;;   region-variant
;;; Parameters:
;;;   image, an image
;;;   left, an integer
;;;   top, an integer
;;;   width, a postiive integer
;;;   height, a positive integer;;;   color-transform, a color transformation
;;; Purpose:
;;;   Build a new image by applying color-transform to each pixel in the
;;;     width-by-height region, starting at (left,top).
;;; Produces:
;;;   variant, the new version of the region
(define region-variant
  (lambda  (image left top width height color-transform)
    (let ((id (image-id image)))
      ; Ensure that we have a valid image.
      (cond 
        ((not id)
         (error "region-variant: Expects an image, received " image))
        ((<= width 0)
         (error "region-transform!: Expects positive width, received " width))
        ((<= height 0)
         (error "region-transform!: Expects positive height, received " height))
        (else
         ; Build the new image
         (let ((new-image (image-new width height)))
           ; Save the selection in the current image
           (let ((sel (image-selection-save id)))
             ; Replace the selection
             (image-select-rectangle! id REPLACE left top width height)
             ; Do the real work
             (mgimp-layer-slurp-from-selection! (image-get-layer new-image)
                                                (image-get-layer id)
                                                color-transform
                                                (- left)
                                                (- top))
             ; Restore the selection
             (image-selection-load! id sel)
             (image-selection-drop! id sel)
             ; And return the newly computed image
             new-image)))))))


; +-----------------------------------------+-----------------------------------
; | Working with Selected Regions of Images |
; +-----------------------------------------+

; [From gimp/selection/selection-fill.scm]

;;; Procedure
;;;   selection-fill!
;;; Parameters
;;;   image, a gimp image
;;; Purpose
;;;   Fill image's current selection (in the active layer) with the current 
;;;   foreground color
;;; Produces
;;;   image, the modified image
;;; Preconditions
;;;   image is a valid image
;;; Postconditions
;;;   All the pixels of the active layer in the current selection are filled 
;;;   with the current foreground color
(define selection-fill! 
  (lambda (image)
    (cond
      ((not (image? image))
       (error "selection-fill!: invalid image" image))
      (else 
       (gimp-edit-fill (image-get-layer image) 0)
       image))))

; [From gimp/selection/selection-stroke.scm]

;;; Procedure:
;;;   selection-stroke!
;;; Parameters:
;;;   image, a gimp image
;;; Purpose:
;;;   Trace the edge of the selected region of the current image
;;;   (in the active layer) using the current brush and foreground
;;    color.
;;; Produces:
;;;   image, the updated image
;;; Preconditions:
;;;   image is a valid image
;;; Postconditions:
;;;   The image has been stroked, as in the stroke menu item.
(define selection-stroke!
  (lambda (image)
    (cond 
      ((not (image? image))
       (error "selection-stroke!: invalid image" image))
      (else
       (gimp-edit-stroke (image-get-layer image))))))

; [From gimp/selection/selection-compute-pixels.scm]

;;; Procedure:
;;;   selection-compute-pixels!
;;; Parameters:
;;;   image, an image
;;;   pos2color, a function of the form (lambda (col row) color)
;;; Purpose:
;;;   Compute pixels in the selected area of image by applying
;;;   pos2color to each col,row pair.
;;; Produces:
;;;   image, the same image
(define selection-compute-pixels!
  (lambda (image pos2color)
    (let ((id (image-id image)))
      (cond
        ((not id)
         (error "selection-compute-pixels! Expected image as parameter 1, given"
                image))
        (else
         (mgimp-layer-compute-pixels-in-selection! (image-get-layer id)
                                                   pos2color))))))

; [From gimp/selection/selection-transform.scm]

;;; Procedure:
;;;   selection-transform!
;;; Parameters:
;;;   image, an image
;;;   color-transform, a color transformation
;;; Purpose:
;;;   Transform the current selection in image by applying color-transform 
;;;     to each pixel in image.
;;; Produces:
;;;   image, the same image
(define selection-transform!
  (lambda  (image color-transform)
    (let ((id (image-id image)))
      ; Ensure that we have a valid image.
      (cond 
        ((not id)
         (error "selection-transform!: Expects an image, received " image))
        (else
         ; Do the real work.
         (mgimp-layer-slurp-from-selection! (image-get-layer id)
                                            (image-get-layer id)
                                            color-transform
                                            0
                                            0)
         image)))))

; [From gimp/selection/selection-variant.scm]

;;; Procedure:
;;;   selection-variant
;;; Parameters:
;;;   image, an image
;;;   color-transform, a color transformation
;;; Purpose:
;;;   Build a new image by applying color-transform to 
;;;     each selected pixel in image
;;; Produces:
;;;   variant, the new version of the selection
(define selection-variant
  (lambda  (image color-transform)
    (let ((id (image-id image)))
      ; Ensure that we have a valid image.
      (cond 
        ((not id)
         (error "selection-variant: Expects an image, received " image))
        (else
         ; Build the new image
         (let ((new-image (image-new (image-selection-width id) 
                                     (image-selection-height id))))
           ; Fill it with a simple color
           (let ((fgcolor (context-get-fgcolor)))
             (context-set-fgcolor! (rgb-new 255 255 255))
             (image-select-all! new-image)
             (image-fill-selection! new-image)
             (image-select-nothing! new-image)
             (context-set-fgcolor! fgcolor))
           
           ; Do the real work
           (mgimp-layer-slurp-from-selection! (image-get-layer new-image)
                                              (image-get-layer id)
                                              color-transform
                                              (- (image-selection-left id))
                                              (- (image-selection-top id)))
           ; And return the newly computed image
           new-image))))))

; [From gimp/selection/selection-op.scm]

;;; Procedure:
;;;   selection-op
;;; Parameters:
;;;   op, a potential selection operation
;;; Purpose:
;;;   Convert op to one of the four selection operations.
;;; Produces:
;;;   selop, an integer
;;; Philosophy:
;;;   For some reason, Script-Fu defines SUBTRACT as 8 rather than 1.
;;;   This procedure helps us ensure that we get the right values.
(define selection-op
  (lambda (op)
    (cond
      ((and (integer? op) (= op SUBTRACT))
       CHANNEL-OP-SUBTRACT)
      ((equal? op "ADD")
       CHANNEL-OP-ADD)
      ((equal? op "SUBTRACT")
       CHANNEL-OP-SUBTRACT)
      ((equal? op "REPLACE")
       CHANNEL-OP-REPLACE)
      ((equal? op "INTERSECT")
       CHANNEL-OP-INTERSECT)
      (else
       op))))


; +---------+-------------------------------------------------------------------
; | Turtles |
; +---------+

; Turtles are a traditional way of drawing on the computer.  These operations
; provide some basic simulations of the common turtle operations.

; [From gimp/turtle/turtle-new.scm]

;;; Procedure:
;;;   turtle-new
;;; Parameters:
;;;   image, the id of an image
;;; Purpose:
;;;   Create a new turtle that "lives" on image.
;;; Produces:
;;    newturtle, a turtle.
;;; Preconditions:
;;;   image is a valid image id.
;;; Postconditions:
;;;   newturtle can be used for the various turtle operations (at least
;;;   until the image disappears).
;;; Phoo:
;;;   The body of the object should probably be generated automatically.
(define _turtle-new
  (lambda (image)
    (let ((world image)
          (col 0) 
          (row 0)
          (angle 0)
          (pen? #t)
          (color (rgb-new 0 0 0))
          (brush "Circle (01)"))
      (lambda (message . params)
        (cond
          ((eq? message ':type) 
           'turtle)
          ((eq? message ':string)
           "<turtle>")
          ((eq? message ':world)
           world)
          ((eq? message ':col)
           col)
          ((eq? message ':row)
           row)
          ((eq? message ':angle)
           angle)
          ((eq? message ':pen?)
           pen?)
          ((eq? message ':color)
           color)
          ((eq? message ':brush)
           brush)
          ((eq? message ':set-world)
           (set! world (car params)))
          ((eq? message ':set-col!)
           (set! col (car params)))
          ((eq? message ':set-row!)
           (set! row (car params)))
          ((eq? message ':set-angle!)
           (set! angle (car params)))
          ((eq? message ':set-color!)
           (set! color (car params)))
          ((eq? message ':set-brush!)
           (set! brush (car params)))
          ((eq? message ':up!)
           (set! pen? #f))
          ((eq? message ':down!)
           (set! pen? #t))
          (else
           (error (string-append "<turtle>: invalid message \""
                                 (value->string message)
                                 "\""
                                 "\n  in "
                                 (value->string
                                  (cons "<turtle>"
                                        (cons message
                                              params)))))))))))

(define turtle-new
  (guard-unary-proc 'turtle-new
                    _turtle-new
                    'image
                    image?))

; [From gimp/turtle/turtle-p.scm]

;;; Procedure:
;;;   turtle?
;;; Parameters:
;;;   val, a Scheme value
;;; Purpose:
;;;   Determine if val can be interepreted as a turtle.
;;; Produces:
;;;   turtle-like?, a Boolean
;;; Ponderings:
;;;   Since we represent turtles as procedures, it is difficult to tell if a
;;;   particular procedure is really a turtle.  Hence, we use a bit of a hack:
;;;   We rely on PLT Scheme's rendering of anonymous procedures as strings.  
;;;   Two anonymous procedures created by the same procedure will have the same
;;;   string.  Two anonymous procedures created by different procedures will have
;;;   different strings.
(define _turtle?
  (let ((sample (value->string (_turtle-new 0))))
    (lambda (val)
      (and (procedure? val)
           (string=? (value->string val) sample)))))

(define turtle? _turtle?)
; [From gimp/turtle/turtle-clone.scm]

;;; Procedure:
;;;   turtle-clone
;;; Parameters:
;;;   turtle, a turtle
;;; Purpose:
;;;   Make a clone of turtle. 
;;; Produces:
;;;   clone, a turtle
;;; Postconditions:
;;;   clone is at the same position as turtle
;;;   clone is oriented in the same direction as turtle
;;;   clone has the same color and brush as turtle
;;;   clone is on the same image as turtle
(define _turtle-clone
  (lambda (turtle)
    (let ((clone (turtle-new (turtle ':world))))
      (clone ':set-col! (turtle ':col))
      (clone ':set-row! (turtle ':row))
      (clone ':set-angle! (turtle ':angle))
      (clone ':set-color! (turtle ':color))
      (clone ':set-brush! (turtle ':brush))
      clone)))

(define turtle-clone
  (guard-unary-proc 'turtle-clone
                    _turtle-clone
                    'turtle
                    turtle?))

; [From gimp/turtle/turtle-down.scm]

;;; Procedure:
;;;   turtle-down!
;;; Parameters:
;;;   turtle, a turtle
;;; Purpose:
;;;   Puts the turtle's brush down
;;; Produces:
;;;   turtle, the same turtle
(define _turtle-down!
  (lambda (turtle)
    (turtle ':down!)
    turtle))

(define turtle-down!
  (guard-unary-proc 'turtle-down!
                    _turtle-down!
                    'turtle
                    turtle?))

; [From gimp/turtle/turtle-face.scm]

;;; Procedure:
;;;   turtle-face!
;;; Parameters:
;;;   turtle, a turtle
;;;   angle, an integer
;;; Purpose:
;;;   Make the turtle face in a particular direction.
;;; Produces:
;;;   [Nothing, called for the side effect.]
;;; Postconditions:
;;;   The turtle is now facing in the direction specified by angle
;;;   (clockwise from right).
(define _turtle-face!
  (lambda (turtle angle)
     (turtle ':set-angle! angle)
     turtle))

(define turtle-face!
  (guard-proc 'turtle-face!
              _turtle-face!
              (list 'turtle 'integer)
              (list turtle? integer?)))

; [From gimp/turtle/turtle-forward.scm]

;;; Procedure:
;;;   turtle-forward!
;;; Parameters:
;;;   turtle, a turtle (created by turtle-new or turtle-clone)
;;;   distance, a real number
;;; Purpose:
;;;   Moves the turtle forward by the given distance.
;;; Produces:
;;;   turtle, the same turtle
(define _turtle-forward!
  (let ((d2r (/ pi 180)))
    (lambda (turtle distance)
      (let ((col (turtle ':col))
            (row (turtle ':row))
            (angle (turtle ':angle)))
        (let ((newcol (+ col (* distance (cos (* d2r angle)))))
              (newrow (+ row (* distance (sin (* d2r angle))))))
          (when (turtle ':pen?)
            (let ((saved-brush (context-get-brush))
                  (saved-color (context-get-fgcolor))
                  (turtle-brush (turtle ':brush))
                  (turtle-color (turtle ':color)))
              (when (not (equal? turtle-brush saved-brush))
                (context-set-brush! turtle-brush))
              (when (not (equal? turtle-color saved-color))
                (context-set-fgcolor! turtle-color))
              (image-draw-line! (turtle ':world)
                                col row
                                newcol newrow)
              (when (and (context-preserve?) 
                         (not (equal? turtle-brush saved-brush)))
                (context-set-brush! saved-brush))
              (when (and (context-preserve?) 
                         (not (equal? turtle-color saved-color)))
                (context-set-fgcolor! saved-color))))
          (turtle ':set-col! newcol)
          (turtle ':set-row! newrow)))
      turtle)))

(define turtle-forward!
  (guard-proc 'turtle-forward!
              _turtle-forward!
              (list 'turtle 'real)
              (list turtle? real?)))

; [From gimp/turtle/turtle-set-brush.scm]

;;; Procedure:
;;;   turtle-set-brush!
;;; Parameters:
;;;   turtle, a turtle
;;;   brush, a string that names a brush
;;; Purpose:
;;;   Set the brush with which the turtle draws.
;;; Produces:
;;;   turtle, the same turtle
(define _turtle-set-brush! 
  (lambda (turtle brush)
    (turtle ':set-brush! brush)
    turtle))

(define turtle-set-brush!
  (guard-proc 'turtle-set-brush!
              _turtle-set-brush!
              (list 'turtle 'brush)
              (list turtle? brush?)))

; [From gimp/turtle/turtle-set-color.scm]

;;; Procedure:
;;;   turtle-set-color!
;;; Parameters:
;;;   turtle, a turtle
;;;   color, an rgb color
;;; Purpose:
;;;   Set the color with which the turtle draws.
;;; Produces:
;;;   turtle, the same turtle
(define _turtle-set-color!
  (lambda (turtle color)
    (turtle ':set-color! (color->rgb color))
    turtle))

(define turtle-set-color!
  (guard-proc 'turtle-set-color!
              _turtle-set-color!
              (list 'turtle 'color)
              (list turtle? color?)))

; [From gimp/turtle/turtle-set-image.scm]

;;; Procedure:
;;;   turtle-set-image!
;;; Parameters:
;;;   turtle, a turtle
;;;   image, an image id
;;; Purpose:
;;;   Moves a turtle to a differnt image.  
;;; Produces:
;;;   turtle, the original turtle
;;; Ponderings:
;;;   Probably not very useful, but included just in case I find
;;;   a reason for it.
(define _turtle-set-image!
  (lambda (turtle image)
    (turtle ':set-world! image)
    turtle))

(define turtle-set-image!
  (guard-proc 'turtle-set-image!
              _turtle-set-image!
              (list 'turtle 'image)
              (list turtle? image?)))

; [From gimp/turtle/turtle-show.scm]

;;; Procedure:
;;;   turtle-show
;;; Parameters:
;;;   turtle, a turtle (created by turtle-new or turtle-clone)
;;; Purpose:
;;;   Shows the turtle on the screen
;;; Produces:
;;;   turtle, the same turtle
(define _turtle-show
  (let ((d2r (/ pi 180)))
    (lambda (turtle)
      (let ((col (turtle ':col))
            (row (turtle ':row))
            (angle (turtle ':angle)))
        (let ((back-col (- col (cos (* d2r angle))))
              (back-row (- row (sin (* d2r angle)))))
          (let ((saved-color (context-get-fgcolor))
                (saved-brush (context-get-brush))
                (turtle-color (turtle ':color)))
            (when (not (equal? turtle-color saved-color))
              (context-set-fgcolor! turtle-color))
            (when (not (equal? saved-brush "Circle (01)"))
              (context-set-brush! "Circle (01)"))
            (image-draw-arrow! (turtle ':world)
                               'pointy
                               back-col back-row
                               col row
                               15 10)
            (when (context-preserve?)
              (when (not (equal? turtle-color saved-color))
                (context-set-fgcolor! saved-color))
              (when (not (equal? saved-brush "Circle (01)"))
                (context-set-brush! saved-brush))))))
      turtle)))

(define turtle-show
  (guard-unary-proc 'turtle-show _turtle-show 'turtle turtle?))

; [From gimp/turtle/turtle-teleport.scm]

;;; Procedure:
;;;   turtle-teleport!
;;; Parameters:
;;;   turtle, a turtle
;;;   col, a real
;;;   row, a real
;;; Purpose:
;;;   Move the turtle to (col,row).
;;; Produces:
;;;   turtle, the same turtle
(define _turtle-teleport!
  (lambda (turtle col row)
    (turtle ':set-col! col)
    (turtle ':set-row! row)
    turtle))

(define turtle-teleport!
  (guard-proc 'turtle-teleport!
              _turtle-teleport!
              (list 'turtle 'real 'real)
              (list turtle? real? real?)))

; [From gimp/turtle/turtle-turn.scm]

;;; Procedure:
;;;   turtle-turn!
;;; Parameters:
;;;   turtle, a turtle (created by turtle-new or turtle-clone)
;;;   angle, a real number
;;; Purpose:
;;;   Rotate turtle clockwise by angle (expressed in degrees)
;;; Produces:
;;;   turtle, the same turtle
(define _turtle-turn!
  (letrec ((fixangle 
            (lambda (angle)
              (if (>= angle 360) (fixangle (- angle 360))
                  (if (< angle 0) (fixangle (+ angle 360))
                      angle)))))   
    (lambda (turtle angle)
      (turtle ':set-angle! 
              (fixangle (+ (turtle ':angle) angle)))
      turtle)))

(define turtle-turn!
  (guard-proc 'turtle-turn!
              _turtle-turn!
              (list 'turtle 'real)
              (list turtle? real?)))

; [From gimp/turtle/turtle-up.scm]

;;; Procedure:
;;;   turtle-up!
;;; Parameters:
;;;   turtle, a turtle
;;; Purpose:
;;;   Puts the turtle's brush up
;;; Produces:
;;;   turtle, the same turtle
(define _turtle-up!
  (lambda (turtle)
    (turtle ':up!)
    turtle))

(define turtle-up!
  (guard-unary-proc 'turtle-up!
                    _turtle-up!
                    'turtle
                    turtle?))

; [From gimp/turtle/turtle-world.scm]

;;; Procedure:
;;;   turtle-world
;;; Parameters:
;;;   turtle, a turtle
;;; Purpose:
;;;   Determine the world on which turtle resides
;;; Produces:
;;;   world, an image id
(define _turtle-world
  (lambda (turtle)
    (turtle ':world)))

(define turtle-world
  (guard-unary-proc 'turtle-world
                    _turtle-world
                    'turtle
                    turtle?))


; +-------------------------------------------+---------------------------------
; | Miscellaneous MediaScheme/GIMP Procedures |
; +-------------------------------------------+

; [From gimp/misc/bezier-constants.scm]

;;; Values:
;;;   BEZIER_ANCHOR
;;;   BEZIER_CONTROL
;;;   BEZIER_MOVE
;;; Summary:
;;;   These values are mentioned in the GIMP PDB documentation, but are
;;;   not supplied in all versions of Script-Fu.
(define BEZIER_ANCHOR 1.0)
(define BEZIER_CONTROL 2.0)
(define BEZIER_MOVE 3.0)

; [From gimp/misc/process-gimp-result.scm]

;;; Procedure:
;;;   process-gimp-result
;;; Parameters:
;;;   result, a Scheme value
;;; Purpose:
;;;   Clean up the result of a GIMP PDB call for convenience.
;;; Produces:
;;;   clean-result, a "cleaner" version of result
;;; Philosophy:
;;;   Almost all of the standard gimp procedures return their result
;;;   as a list.  Users generally want just the value.  This handles
;;;   special cases.
;;; Problems:
;;;   * Colors appear to be the an exception to the standard GIMP return
;;;     policy.  For example, gimp-image-get-pixel returns a triplet.
;;;   * Some procedures return multiple values.  In those cases, it makes
;;;     sense to leave them as a list.
(define process-gimp-result
  (lambda (result)
    (if (and (pair? result) (null? (cdr result)))
        (car result)
        result)))


; +------------------------------------------------+----------------------------
; | MediaScheme/PLT GIMP RGB Procedures and Values |
; +------------------------------------------------+

; [From mpltg/rgb/gimp-rgb-colors.scm]

;;; Values:
;;;   RGB-RED
;;;   RGB-GREEN
;;;   RGB-BLUE
;;;   RGB-VIOLET
;;;   ...
;;; Type:
;;;   RGB color
;;; Summary:
;;;   Constants that correspond to the various GIMP colors.
(for-each (lambda (name)
            (eval (list 'define 
                        (string->symbol (string-append "RGB-" 
                                                       (string-upcase name)))
                        (color->rgb name))))
          (context-list-colors))
