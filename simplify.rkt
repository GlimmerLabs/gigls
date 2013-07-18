#lang racket
; We need the bridge between Racket and the raw PDB functions
(require LoudGimp/gimp-dbus)

; Behind-the-scenes functions that don't need to be exported.
(require
         LoudGimp/makers)

; Primary functions that do need to be exported.
(require LoudGimp/alt
         LoudGimp/colors
         LoudGimp/context
         LoudGimp/drawings
         LoudGimp/guard
         LoudGimp/image
         LoudGimp/hacks
         LoudGimp/higher
         LoudGimp/irgb
         LoudGimp/list
         LoudGimp/mgimp
         LoudGimp/misc
         LoudGimp/positions
         LoudGimp/tile
         LoudGimp/utils
         LoudGimp/turtles)

;;; File:
;;;   LoudGimp/simplify
;;; Summary:
;;;   Simplified versions of the PDB procedures, intended
;;;   for beginning programmers.

(provide (all-defined-out)
         (all-from-out LoudGimp/gimp-dbus)
         (all-from-out LoudGimp/alt)
         (all-from-out LoudGimp/colors)
         (all-from-out LoudGimp/context)
         (all-from-out LoudGimp/drawings)
         (all-from-out LoudGimp/guard)
         (all-from-out LoudGimp/hacks)
         (all-from-out LoudGimp/irgb)
         (all-from-out LoudGimp/higher)
         (all-from-out LoudGimp/image)
         (all-from-out LoudGimp/list) 
         (all-from-out LoudGimp/mgimp)
         (all-from-out LoudGimp/misc)
         (all-from-out LoudGimp/positions)
         (all-from-out LoudGimp/tile)
         (all-from-out LoudGimp/utils)
         (all-from-out LoudGimp/turtles))

