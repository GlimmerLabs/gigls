;;; File:
;;;   analyst.scm
;;; Authors:
;;;   Janet L. N. Davis
;;;   Samuel A. Rebelsky
;;; Version:
;;;   0.1.0.2 of 8 April 2009
;;; Summary:
;;;   A collection of procedures that help us analyze the cost of running code.
;;; Procedures:
;;;   (analyst ':count! sym)
;;;     Add a count for the specified symbol (usually the name of a proc.)
;;;   (analyst ':report!)
;;;     Print a short report on the steps executed.
;;;   (analyst ':reset!)
;;;     Reset the counts of the various steps.
;;; Syntax:
;;;   ($ proc)
;;;     A version of proc that counts the call.
;;;   (analyze exp)
;;;     Evaluate expression and then print out a summarize of the counts
;;;     generated during that evaluation.
;;;   (define$ proc body)
;;;     Define proc as a procedure that counts calls for each subexpression.
;;; Advanced Use:
;;;   1. Augment the procedure to count steps in one of three ways (examples in
;;;      analysis-examples.scm).
;;;     (0) Insert (analyst 'count! sym) each time you want to count a step.
;;;     (1) Apply $ to the procedure to call.
;;;     (2) Replace the define with define$. 
;;;   2. To analyze an algorithm,
;;;     Reset the counts.
;;;     Call your algorithm.
;;;     Report on the counts
;;; Practica:
;;;   (define lst ...)
;;;   (define$ myproc ...)
;;;   (analyst ':reset!)
;;;   (myproc lst)
;;;   (analyst ':report!)

;;; Copyright (c) 2006-2009 Janet L. N. Davis and Samuel A. Rebelsky
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

; +-------+----------------------------------------------------------------
; | To Do |
; +-------+

; [] Make define$ work more generally.
; [] Test define$ on a wide variety of procedures.
; [] Think about a way to count kernels and the parameters to hops better.
; [] Make insert-$ local.
; [] Write a corresponding reading (?).


; +-----+------------------------------------------------------------------
; | HOP |
; +-----+

(define left-section (lambda (proc left) (lambda (right) (proc left right))))
(define l-s left-section)


; +---------+--------------------------------------------------------------
; | Sorting |
; +---------+

; We're only sorting small lists, so let's use insertion sort

(define isort
  (letrec ((insert
            (lambda (new-value lst may-precede?)
              (let kernel ((rest lst)
                           (bypassed null))
                (cond ((null? rest) 
                       (reverse (cons new-value bypassed)))
                      ((may-precede? new-value (car rest))
                       (append (reverse (cons new-value bypassed)) rest))
                      (else 
                       (kernel (cdr rest) (cons (car rest) bypassed))))))))
    (lambda (lst may-precede?)
      (let helper ((unsorted lst)  ; The remaining unsorted values
                   (sorted null))      ; The sorted values
        (if (null? unsorted) sorted
            (helper (cdr unsorted) 
            (insert (car unsorted) sorted may-precede?)))))))

           

; +--------------------+---------------------------------------------------
; | Primary Procedures |
; +--------------------+

;;; Procedure
;;;   (analyst 'count! name)
;;; Parameters:
;;;   name, a symbol
;;; Purpose:
;;;   Increments the counter associated with name.
;;; Produces:
;;;   Nothing; called for its side effect.
;;; Preconditions:
;;;   (none)
;;; Postconditions:
;;;   The count associated with name has been incremented by 1.

;;; Procedure:
;;;   (analyst 'report! sym1 ... symn)
;;; Parameters:
;;;   sym1 ... symn
;;; Purpose:
;;;   Report on all the counts for sym1 ... symn (or everything, if no
;;;   symbols are provided).
;;; Produces:
;;;   Nothing; called for its side effect.
;;; Preconditions:
;;;   (none)
;;; Postconditions:
;;;   The counts have been displayed.

;;; Procedure:
;;;   (analyst 'reset!)
;;; Parameters:
;;;   (none)
;;; Purpose:
;;;   Reset all the counts.
;;; Produces:
;;;   Nothing; called ofr its side effect.
(define analyst
  (let ((counts null))
    (lambda (message . params)
      (cond
        ; (analyst ':count! name)
        ;   Add one to the count associated with name.
        ((eq? message ':count!)
         (letrec ((kernel (lambda (name lst)
                            (cond
                              ((null? lst) 
                               (list (vector name 1)))
                              ((eq? name (vector-ref (car lst) 0))
                               (vector-set! (car lst) 1
                                            (+ 1 (vector-ref (car lst) 1)))
                               lst)
                              (else 
                               (cons (car lst) (kernel name (cdr lst))))))))
              (set! counts (kernel (car params) counts))))

        ; (analyst ':report! [name0 ... namen])
        ;   Report on counts for the given names.  If no names are
        ;   given, reports on all counts.
        ((eq? message ':report!) 
         ; The kernel steps through the list of counts and displays
         ; any relevant ones.  It returns the total of those counts.
         (letrec ((kernel (lambda (lst)
                            (cond
                              ((null? lst) 0)
                              ((or (null? params) 
                                   (member (vector-ref (car lst) 0) params))
                               (let* ((entry (car lst))
                                      (name (vector-ref entry 0))
                                      (count (vector-ref entry 1)))
                                 (display name)
                                 (display ": ")
                                 (display count)
                                 (newline)
                                 (+ count (kernel (cdr lst)))))
                              (else
                               (kernel (cdr lst)))))))
           (set! counts
                 (isort counts 
                       (lambda (p1 p2) 
                         (string-ci<? (symbol->string (vector-ref p1 0)) 
                                      (symbol->string (vector-ref p2 0))))))
           (let ((total (kernel counts)))
             (display "Total: ")
             (display total)
             (newline))))

        ; (analyst ':reset!)
        ;   Reset all counts to 0.
        ((eq? message ':reset!)
         (set! counts null))

        (else 
         (error "analyst: unknown message" message))))))


; +--------+---------------------------------------------------------------
; | Syntax |
; +--------+

;;; Keyword:
;;;   $
;;; Parameters:
;;;   proc, a procedure
;;; Purpose:
;;;   Record the call to proc.
;;; Produces:
;;;   proc, its parameter
;;; Preconditions:
;;;   proc names a procedure
;;; Postconditions:
;;;   The call has been recorded.
(define-syntax $
  (syntax-rules ()
    (($ proc)
     (begin (analyst ':count! 'proc) proc))))

;;; Procedure:
;;;   _insert-$
;;; Parameters:
;;;   struct, a Scheme structure
;;; Purpose:
;;;   Insert $ at all of the procedure calls (or so I hope)
;;; Produces:
;;;   $struct, a Scheme structure.
;;; Problems:
;;;   Only works with standard Scheme syntax.
(define _insert-$
  ; process-second recurses only on the second element of a length-two list.  It
  ; is used for the definitions part of let, let*, letrec, and named let.
  (let ((process-second (lambda (def) (list (car def) (_insert-$ (cadr def))))))
    (lambda (struct)
      (if (or (null? struct) (not (list? struct)) )
          struct
          (let ((first (car struct)))
            (cond
              ; lambda expression: Only process the body
              ((eq? first 'lambda)
               (cons first (cons (cadr struct) (map _insert-$ (cddr struct)))))
              ; cond expression: Process each clause
              ((eq? first 'cond)
               (cons first (map (l-s map _insert-$) (cdr struct))))
              ; if, and, and or syntax all involve the same process
              ((member first (list 'if 'and 'or 'when))
               (cons first (map _insert-$ (cdr struct))))
              ; let*, lectrec, and plain let:
              ((or (eq? first 'let*)
                   (eq? first 'letrec)
                   (and (eq? first 'let) (not (symbol? (cadr struct)))))
               (cons first 
                     (cons (map process-second (cadr struct)) 
                           (map _insert-$ (cddr struct)))))
              ; Named let
              ((eq? first 'let)
               (cons first 
                     (cons (cadr struct)
                           (cons (map process-second (caddr struct))
                                 (map _insert-$ (cdddr struct))))))
              ; A simple application
              ((symbol? first)
               (cons (list '$ first) (map _insert-$ (cdr struct))))
              ; Everything else: Funky application
              (else (map _insert-$ struct))))))))

;;; Syntax:
;;;   analyze
;;; Parameters:
;;;   exp, an annotated expression
;;;   sym1, ... symn, symbols [optional]
;;; Purpose:
;;;   Analyze the given expression.
;;; Produces:
;;;   val, a Scheme value.
;;; Preconditions:
;;;   exp is a valid expression.
;;; Postconditions:
;;;   val is the value of exp.
;;;   A report on exp has been printed.
;;;   If no symbols are included, all counts are reported.
;;;   If symbols are included, only the counts for those symbols are reported.
(define-syntax analyze
  (syntax-rules ()
    ((analyze exp sym ...)
     (begin
       (analyst ':reset!)
       (let ((result exp))
         (analyst ':report! 'sym ...)
         result)))))

;;; Syntax:
;;;   define$
;;; Parameters:
;;;   name, a symbol (unquoted)
;;;   definiens, an expression
;;; Purpose:
;;;   Associates name with definiens, counting steps as appropriate
;;;   (and preparing to count more).
;;; Produces:
;;;   (nothing)
;;; Preconditions:
;;;   definiens is a valid expression.
;;; Postconditions:
;;;   Any procedure calls that appear in definiens are recorded.
;;;   If definiens returns a procedure, name refers to a version of
;;;     that procedure that records its steps.
(define-syntax define$
  (syntax-rules ()
    ((define$ name definiens)
     (define name (eval (_insert-$ 'definiens))))))

;;; Syntax:
;;;   define$$
;;; Parameters:
;;;   name, a symbol (unquoted)
;;;   definiens, an expression
;;; Purpose:
;;;   See what define$ does to the expression.
;;; Preconditions:
;;;   (none)
;;; Postconditions:
;;;   name now refers to the symbolic version of the annotated
;;;   definiens.
(define-syntax define$$
  (syntax-rules()
    ((define$$ name definiens)
     (define name (_insert-$ 'definiens)))))

