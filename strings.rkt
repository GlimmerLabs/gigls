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

;;; Procedure:
;;;   string-ends-with?
;;; Parameters:
;;;   string, a string
;;;   suffix, a string
;;; Purpose:
;;;   Determine if string ends with suffix
;;; Produces:
;;;   is-suffix?, a Boolean
(define string-ends-with?
  (lambda (string suffix)
    (let ([string-len (string-length string)]
          [suffix-len (string-length suffix)])
      (and (>= string-len suffix-len)
           (string=? (substring string (- string-len suffix-len)) suffix)))))
           


