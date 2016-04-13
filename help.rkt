#lang racket

; gigls/help
;   A simple help facility for gigls (or, I suppose, for any set of
;   code documented by six-P style documentation).
;
;   Copyright (c) 2013 Samuel A. Rebelsky.  All rights reserved.
;
;   This file is part of gigls, the Glimmer Improved Gimp Library for 
;   Scripting.
;
;   gigls is free software: you can redistribute it and/or modify it under the
;   terms of the GNU Lesser General Public License as published by the Free
;   Software Foundation, either version 3 of the License, or (at your option)
;   any later version.
;
;   gigls is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;   GNU General Public License for more details.
;
;   You should have received a copy of the GNU Lesser General Public 
;   License along with gigls.  If not, see <http://www.gnu.org/licenses/>.

(provide (all-defined-out))
(require gigls/docs
         gigls/guard
         gigls/higher
         gigls/strings)

; +-------+-----------------------------------------------------------
; | Notes |
; +-------+

; We store information about procedures and values in an association
; list.  The key (car) of each element is the procedure name.  The 
; value (cdr) is itself an association list, using the section headers
; as keys.  Each record also has an element that contains the original
; text, with a key of "Source".

; +-------+-----------------------------------------------------------
; | Hacks |
; +-------+

(define string-split (lambda params null))
(define string-join (lambda params ""))

; +------------+------------------------------------------------------
; | Predicates |
; +------------+

;;; Procedure:
;;;   sixps-header?
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   Determine if the string represents a six-P style header
;;; Produces:
;;;   header?, a boolean
(define/contract sixps-header?
  (-> string? boolean?)
  (lambda (str)
    (and (sixps-line? str)
         (char-upper-case? (string-ref str 4)))))

;(define sixps-header?
;  _sixps-header?)

;;; Procedure:
;;;   sixps-line?
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   Determines if str is a line of six-p-style documentation
;;; Produces:
;;;   is-line?, a boolean
(define/contract sixps-line?
  (-> string? boolean?)
  (lambda (str)
    (and (string? str)
         (>= (string-length str) 3)
         (string=? ";;;" (substring str 0 3)))))

;(define sixps-line? 
;  _sixps-line?)

; +------------+------------------------------------------------------
; | Procedures |
; +------------+

;;; Procedure:
;;;   extract-documentation
;;; Parameters:
;;;   file-1 ... file-n, 0 or more files
;;; Purpose:
;;;   Extract documentation from a bunch of files
;;; Produces:
;;;   docs, a list of strings
;;; Preconditions:
;;;   All of the files exist and are open for reading
;;; Postconditions:
;;;   docs contains all of the lines in the files that begin with
;;;   three semicolons, with subsequent lines grouped together.
(define/contract extract-documentation
  (->* (path-string?) () #:rest (listof path-string?) (listof string?))
  (lambda filenames
    (let kernel ([port (open-input-file (car filenames))]
                 [current-item ""]
                 [items '()]
                 [remaining-files (cdr filenames)])
        (let ([line (read-line port)]
               [all-items (if (string=? "" current-item) 
                              items
                              (cons current-item items))])

          (cond
            ; When we exhaust the current file
            [(eof-object? line)
             ; Close the port
             (close-input-port port)
             ; If there are no remaining files
             (if (null? remaining-files) 
                 ; Give back our result
                 (reverse all-items)
                 ; If there are remaining files, continue with next file
                 (kernel (open-input-file (car remaining-files))
                         ""
                         all-items
                         (cdr remaining-files)))]
            ; If we see a six-p style line, append it to the current record
            [(sixps-line? line)
             (kernel port
                     (string-append current-item line "\n") 
                     items
                     remaining-files)]
            ; Every other time, end the current item
            [else
             (kernel port "" all-items remaining-files)])))))

;(define extract-documentation 
;  _extract-documentation)

;;; Procedure:
;;;   extract-documentation-from-file
;;; Parameters:
;;;   fname, a string
;;; Purpose:
;;;   Extract documentation from a file
;;; Produces:
;;;   docs, a list of strings
;;; Preconditions:
;;;   A file with the given name exists and is open for reading.
;;; Postconditions:
;;;   docs contains all of the lines in the file that begin with
;;;   three semicolons, with subsequent lines grouped together.
(define/contract extract-documentation-from-file
  (-> string? (listof string?))
  (lambda (fname)
    (let ([port (open-input-file fname)])
      (let kernel ([current-item ""]
                   [items '()])
        (let ((line (read-line port)))
          (cond
            [(eof-object? line)
             (close-input-port port)
             (reverse items)]
            [(sixps-line? line)
             (kernel (string-append current-item line "\n") items)]
            [else
             (if (string=? "" current-item)
                 (kernel current-item items)
                 (kernel "" (cons current-item items)))]))))))

;(define extract-documentation-from-file
;  (lambda (fname)
;    (when (not (file-exists? fname))
;      (error "extract-documentation: The file does not exist: " fname))
;    (_extract-documentation-from-file fname)))

;;; Procedure:
;;;   sixps-extract-data
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   Extract data from a string
;;; Produces:
;;;   data, a string
;;; Preconditions:
;;;   str begins with three semicolons
;;; Postconditions:
;;;   data is str with the leading semicolons and spaces stripped
(define/contract sixps-extract-data
  (-> string? string?)
  (let ([cleanup (o (r-s string-remove-trailing-chars char-whitespace?)
                    (r-s string-remove-leading-chars char-whitespace?)
                    (r-s string-remove-leading-chars (l-s char=? #\;)))])
    (lambda (str)
      (cleanup str))))

;(define sixps-extract-data
;  (guard-unary-proc 'sixps-extract-data
;                    _sixps-extract-data
;                    'string
;                    string?))
    
;;; Procedure:
;;;   sixps-extract-header
;;; Parameters:
;;;   str, a string
;;; Purpose:
;;;   Extract the six-P style header from string
;;; Produces:
;;;   header, a string
;;; Preconditions:
;;;   (sixps-header? str)
;;; Postconditions:
;;;   header is the header (e.g., "Procedure")
(define/contract sixps-extract-header
  (-> string? string?)
  (let ([cleanup (o (r-s string-remove-trailing-chars (l-s char=? #\:))
                    (r-s string-remove-trailing-chars char-whitespace?)
                    (r-s string-remove-leading-chars char-whitespace?)
                    (r-s string-remove-leading-chars (l-s char=? #\;)))])
    (lambda (str)
      (cleanup str))))
 
;(define sixps-extract-header
;  (guard-unary-proc 'sixps-extract-header
;                    _sixps-extract-header
;                    'sixp-style-header
;                    sixps-header?))

;;; Procedure:
;;;   convert-sixps
;;; Purpose:
;;;   Convert 6P-style documentation to an association list
;;; Parameters:
;;;   sixps, a string
;;; Produces:
;;;   doc, an association list
;;; Preconditions:
;;;   sixps has the appropriate form (a sequence of lines, each of which
;;;   starts with three semicolons)
(define/contract convert-sixps
  (-> string? list?)
  (lambda (sixps)
    (let ([lines (string-split sixps "\n")])
      (let kernel ([header (sixps-extract-header (car lines))]
                   [data null]
                   [entries null]
                   [remaining (cdr lines)])
         (cond
           [(null? remaining)
            (reverse (cons (cons header (reverse data)) entries))]
           [(sixps-header? (car remaining))
            (kernel (sixps-extract-header (car remaining))
                    null
                    (cons (cons header (reverse data)) entries)
                    (cdr remaining))]
           [else
            (kernel header
                    (cons (sixps-extract-data (car remaining)) data)
                    entries
                    (cdr remaining))])))))

;(define convert-sixps
;  _convert-sixps)

;;; Procedure:
;;;   sixps-short
;;; Parameters:
;;;   doc, an association list of the form created by convert-sixps
;;; Purpose:
;;;   Create a string with the short documentation for doc
;;; Produces:
;;;   short, a string
(define/contract sixps-short
  (-> list? string?)
  (lambda (doc)
    (let ([proc (cdr (assoc "Procedure" doc))]
          [params (string-join (map (o car (r-s string-split ","))
                                    (cdr (assoc "Parameters" doc)))
                               " ")]
          [purpose (cdr (assoc "Purpose" doc))])
      (if (null? params)
          (string-append "(" proc ") - " purpose)
          (string-append "(" proc " " params ") - " purpose)))))

;;; Procedure:
;;;   sixps-entry
;;; Parameters:
;;;   sixps, a string
;;; Purpose:
;;;   Create a documentation entry for a sixps string
;;; Produces:
;;;   entry, an entry
;;; Preconditions:
;;;   sixps is a string representing multiple lines from a file in 
;;;   sixp style.
;;; Postconditions:
;;;   entry is a pair of the form
;;;     ("*name*" ("Source" . "...")
;;;               ("Short" . "...")
;;;               ("Procedure" . "...")
;;;               ...)
