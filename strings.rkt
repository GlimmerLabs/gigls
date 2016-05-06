#lang racket

; strings.rkt
;   A variety of procedures useful for working with strings.
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
(require gigls/guard)

;;; Procedure:
;;;   string-ends-with?
;;; Parameters:
;;;   string, a string
;;;   suffix, a string
;;; Purpose:
;;;   Determine if string ends with suffix
;;; Produces:
;;;   is-suffix?, a Boolean
(define/contract string-ends-with?
  (-> string? string? boolean?)
  (lambda (str suffix)
    (let ([string-len (string-length str)]
          [suffix-len (string-length suffix)])
      (and (>= string-len suffix-len)
           (string=? (substring str (- string-len suffix-len)) suffix)))))

;(define string-ends-with?
;  (guard-proc 'string-ends-with?
;              _string-ends-with?
;              (list 'string 'string)
;              (list string? string?)))

;;; Procedure:
;;;   string-remove-leading-chars
;;; Parameters:
;;;   str, a string
;;;   char-pred?, a character predicate
;;; Purpose:
;;;   Remove all of the leading characters from str for which pred holds
;;; Produces:
;;;   newstr, a string
;;; Postconditions:
;;;   Exists i s.t. newstr is (substring str i).
;;;   For all j, 0 <= j < i, (char-pred? (string-ref str j)).
;;;   (not (char-pred? (string-ref str i))).
(define/contract string-remove-leading-chars
  (-> string? (-> char? boolean?) string?)
  (lambda (str char-pred?)
    (let kernel ([i 0]
                 [len (string-length str)])
      (cond
        [(>= i len)
         ""]
        [(char-pred? (string-ref str i))
         (kernel (+ i 1) len)]
        [else
         (substring str i)]))))

;(define string-remove-leading-chars
;  (guard-proc 'string-remove-leading-chars
;              _string-remove-leading-chars
;              (list 'string 'character-predicate)
;              (list string? procedure?)))

;;; Procedure:
;;;   string-remove-trailing-chars
;;; Parameters:
;;;   str, a string
;;;   char-pred?, a character predicate
;;; Purpose:
;;;   Remove all of the trailing characters from str for which pred holds
;;; Produces:
;;;   newstr, a string
;;; Postconditions:
;;;   Exists i s.t. newstr is (substring str 0 i)
;;;   For all j, i <= j < (string-length str), 
;;;     (char-pred? (string-ref str j))
;;;   (not (char-pred? (string-ref str (- i 1))))
(define/contract string-remove-trailing-chars
  (-> string? (-> char? boolean?) string?)
  (lambda (str char-pred?)
    (let ([len (string-length str)])
      (let kernel ([i (- len 1)])
        (cond
          [(< i 0)
           ""]
          [(char-pred? (string-ref str i))
           (kernel (- i 1))]
          [else
           (substring str 0 (+ i 1))])))))

;(define string-remove-trailing-chars
;  (guard-proc 'string-remove-trailing-chars
;              _string-remove-trailing-chars
;              (list 'string 'character-predicate)
;              (list string? procedure?)))

