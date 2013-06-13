;;; File:
;;;   unit-test.scm
;;; Authors:
;;;   Janet L. N. Davis
;;;   Samuel A. Rebelsky
;;; Version:
;;;;  0.1.0.0 of 15 January 2009
;;; Summary:
;;;   A set of procedures that support a simple form of unit testing
;;;   for Scheme code.
;;; Contents:
;;;   (begin-tests!) - Set up the environment for testing
;;;   (test! expression value) - Determine whether (a) expression can
;;;     be evaluated and (b) the value of expression is value.
;;;   (test-error! expression) - Verify that expression is
;;;     erroneous (that is, that it cannot be evaluated).
;;;   (end-tests!) - Conclude a set of tests and report their results.
;;; Use:
;;;   In general, you begin a series of tests by calling (begin-tests!).
;;;   You conduct each test with the test! procedure, which takes two
;;;     parameters: (1) a list that represents an expression to be evaluated
;;;     (2) the expected value of that expression.
;;;   Use test-error! to evaluate expressions that should result in an error
;;;     (e.g., if a precondition is violated).
;;;   When you are done, you end the series of tests by calling (end-tests!).
;;;   This procedure will report on all the failures.
;;; Practica:
;;;   (begin-tests!)
;;;   (test! (reverse (list 'a 'b 'c)) (list 'c 'b 'a))
;;;   (test-error! (reverse 'spam))
;;;   (end-tests!)

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

; * Document helper procedures.
; * Continue to think about how to phrase the error/failure distinction.
; * Make the kernels local.
; * Add test-equals! test-true! test-false! (maybe)

; +---------------+--------------------------------------------------------
; | Encapsulation |
; +---------------+

;;; Procedure:
;;;   test-proc
;;; Parameters:
;;;   name, a symbol
;;; Purpose:
;;;   Given the name of one of the unit testing procedures, extracts its body.  
;;; Produces:
;;;   proc, a procedure
;;; Preconditions:
;;;   name must be 'begin-tests! 'test-kernel! 'test-error-kernel! or
;;;     'end-tests!
;;; Postconditions:
;;;   proc is a procedure that accomplishes the stated goals.  (See
;;;     below for those goals.)
;;; Philosophy:
;;;   This style of design permits us to encapsulate the variables shared
;;;   between procedures, and therefore makes the code a bit safer.
(define test-proc
  (let (
        ; A number that gives the count of tests performed.
        (tests 0)
        ; A list of errors encountered.  Each error has the form
        ;   (_expression_ _exception_)
        (errors null)
        ; A list of failed equality tests.  Each element has the form
        ;   (_expression_ _expected_value_ _actual_value_)
        (failures null))
    (letrec (
             ; (report-failures! remaining-failures)
             ;   Prints info about each of the failed tests.
             ;   Expects the form documented above.
             (report-failures!
              (lambda (remaining-failures)
                (cond
                  ((not (null? remaining-failures))
                   (let ((error (car remaining-failures)))
                     (display "  For ")
                     (write (car error))
                     (display " expected [")
                     (write (cadr error))
                     (display "] got [")
                     (write (caddr error))
                     (display "]")
                     (newline)
                     (report-failures! (cdr remaining-failures)))))))

             ; (report-errors! errors)
             ;   Prints a list of the errors encountered.  Assumes that 
             ;   the list has the form documented above.
             (report-errors!
              (lambda (remaining-errors)
                (cond
                  ((not (null? remaining-errors))
                   (let ((exception (car remaining-errors)))
                     (display "  The expression ")
                     (display (car exception))
                     (display " failed to evaluate because [")
                     (display (exn-message  (cadr exception)))
                     (display "]")
                     (newline)
                     (report-errors! (cdr remaining-errors))))))))

    (lambda (proc)
      (cond 
        ((eq? proc 'begin-tests!)
         (lambda ()
           (set! failures null)
           (set! errors null)
           (set! tests 0)))

        ((eq? proc 'end-tests!)
         (lambda ()
           (let* ((num-errors (length errors))
                  (num-failures (length failures))
                  (total-failures (+ num-errors num-failures))
                  (num-successes (- tests total-failures)))
             (cond
               ((= tests 0)
                (display "No tests conducted."))
               ((= tests 1)
                (display "One test conducted."))
               (else
                (display tests)
                (display " tests conducted.")))
             (newline)
             (cond
               ((= num-successes 0)
                (display "No tests succeeded."))
               ((= num-successes 1)
                (display "One test succeeded."))
               (else
                (display num-successes)
                (display " tests succeeded.")))
             (newline)
             (cond 
               ((= total-failures 0) (display "No tests failed."))
               ((= total-failures 1) (display "One test failed."))
               (else (display total-failures) (display " tests failed.")))
             (newline)
             (cond 
               ((= total-failures 0))
               ((= num-errors 0)
                (display "No errors encountered.") (newline))            
               ((= num-errors 1) 
                (display "One error encountered:") (newline))
               (else 
                (display num-errors) 
                (display " errors encountered:") 
                (newline)))
             (report-errors! (reverse errors))
             (cond 
               ((= total-failures 0))
               ((= num-failures 0)
                (display "No other tests failed to give the expected result.") 
                (newline))
               ((= num-failures 1) 
                (display "One other test failed to give the expected result:") 
                (newline))
               (else 
                (display num-failures) 
                (display " other tests failed to give the expected result:")
                (newline)))
             (report-failures! (reverse failures))
             (if (= 0 (+ num-failures num-errors))
                 (display "CONGRATULATIONS!  All tests passed.")
                 (display "Sorry.  You'll need to fix your code."))
             (newline))))

        ((eq? proc 'test-kernel!)
         (lambda (exp expected) 
           (set! tests (+ tests 1))
           (with-handlers
               ((exn:fail?
                 (lambda (exception)
                   (set! errors
                         (cons (list exp exception) errors)))))
             (let ((result (eval exp)))
               (cond
                 ((not (equal? result expected))
                  (set! failures 
                        (cons (list exp expected result) failures))))))))

        ((eq? proc 'test-error-kernel!)
         (lambda (exp) 
           (set! tests (+ tests 1))
           (with-handlers
               ((exn:fail?
                 (lambda (exception)
                   ; This is what we expected!  The next line is a hack for
                   ; "do nothing".
                   (set! exp exp))))
             (let ((result (eval exp)))
               ((set! failures (cons (list exp '<error> result) failures)))))))

        ((eq? proc 'test-permutation-kernel!)
         (lambda (exp lst)
           (set! tests (+ tests 1))
           (with-handlers
               ((exn:fail?
                 (lambda (exception)
                   (set! errors
                         (cons (list exp exception) errors)))))
             (let ((result (eval exp)))
               (cond
                 ((not (ut-permutation? result lst))
                  (set! failures 
                        (cons (list exp (list 'permutation-of lst) result) 
                              failures))))))))

        (else 
         (error "test-proc: unknown procedure")))))))


; +--------------------+---------------------------------------------------
; | Primary Procedures |
; +--------------------+

;;; Procedure:
;;;   begin-tests!
;;; Parameters:
;;;   (none)
;;; Purpose:
;;;   Prepare the system for testing.
;;; Produces:
;;;   Nothing.
;;; Preconditions:
;;;   (none)
;;; Postconditions:
;;;   The system is now in a state that permits use of the testing procedures.
(define begin-tests! (test-proc 'begin-tests!))

;;; Procedure:
;;;   test!
;;; Parameters:
;;;   exp, a Scheme value that represents an expression to be evaluated.
;;;   expected, a Scheme value that represents the expected result
;;; Purpose:
;;;   Evaluate exp and determine whether or not it equals expected.  In
;;;   either case, updates our testing statistics.
;;; Produces:
;;;   (nothing)
;;; Preconditions:
;;;   The test system has been initialized with begin-tests!
;;;   The test system has not been finished with end-tests!
;;; Postconditions:
;;;   The information in the test system has been updated appropriately.
(define-syntax test!
  (syntax-rules ()
    ((test! exp expected)
     (test-kernel! 'exp expected))))

(define test-kernel! (test-proc 'test-kernel!))

;;; Procedure:
;;;   test-error!
;;; Parameters:
;;;   exp, a Scheme value that represents an expression to be evaluated.
;;;   The expression should be one that is expected to generate an error.
;;; Purpose:
;;;   Evaluate exp and determine whether it generates an error.
;;;   If there is an error, the test succeeds.
;;;   Otherwise, the test fails.
;;;   In either case, updates our testing statistics.
;;; Produces:
;;;   (nothing)
;;; Preconditions:
;;;   The test system has been initialized with begin-tests!
;;;   The test system has not been finished with end-tests!
;;; Postconditions:
;;;   The information in the test system has been updated appropriately.
(define-syntax test-error!
  (syntax-rules ()
    ((test-error! exp)
     (test-error-kernel! 'exp))))

(define test-error-kernel! (test-proc 'test-error-kernel!))

;;; Procedure:
;;;   test-permutation!
;;; Parameters:
;;;   expression, an expression to test
;;;   lst, a list
;;; Purpose:
;;;   Evaluate expression and determine whether or not the result is a
;;;   permutation of lst.
;;; Produces:
;;;   (nothing)
;;; Preconditions:
;;;   (none)
;;; Postconditions:
;;;   The information in the test system has been updated appropriately.
(define-syntax test-permutation!
  (syntax-rules ()
    ((test-permutation! exp expected)
     (test-permutation-kernel! 'exp expected))))

(define test-permutation-kernel! (test-proc 'test-permutation-kernel!))

;;; Procedure:
;;;   end-tests!
;;; Parameters:
;;;   (none)
;;; Purpose:
;;;   Completes a sequence of tests.
;;; Produces:
;;;   (nothing) [displays results]
;;; Preconditions:
;;;   The test system has been initialized with begin-tests!
;;;   The test system has not been finished with end-tests!
;;; Postconditions:
;;;   Information about the tests has been displayed.
(define end-tests! (test-proc 'end-tests!))

; +-----------------+------------------------------------------------------
; | Local Utilities |
; +-----------------+

;;; Procedure:
;;;   ut-permutation?
;;; Parameters:
;;;   left, a list
;;;   right, a list
;;; Purpose:
;;;   Determines whether left is a permutation of right.
;;; Produces:
;;;   is-permutation?, a boolean
;;; Preconditions:
;;;   None
;;; Postconditions:
;;;   is-permutation? is true exactly when left and right are 
;;;   permutations of each other.
(define ut-permutation?
  (letrec
    ((member? (lambda (val lst)
                (and (not (null? lst))
                     (or (equal? val (car lst))
                         (member? val (cdr lst))))))
     (remove (lambda (val lst)
                (cond 
                  ((null? lst) null)
                  ((equal? val (car lst)) (cdr lst))
                  (else (cons (car lst) (remove val (cdr lst))))))))
    (lambda (left right)
      (or (and (null? left) (null? right))
          (and (not (null? left))
               (not (null? right))
               (member? (car left) right)
               (ut-permutation? (cdr left)
                                (remove (car left) right)))))))

