#lang racket

;;; File:
;;;   gigls/unsafe
;;; Summary:
;;;   The primary gigls interface.  A library of functions that make it
;;;   easier for beginning programmers to script the Gimp.

(define gigls-version "gigls 0.1.0b1 of 20 August 2013")

; We need the bridge between Racket and the raw PDB functions
(require gigls/pdb-dbus)

; Behind-the-scenes functions that don't need to be exported.
(require gigls/makers)

; Primary functions that do need to be exported.
(require gigls/alt
         gigls/brushes
         gigls/colors
         gigls/context
         gigls/current-brush
         gigls/drawings
         gigls/guard
         gigls/image
         gigls/hacks
         gigls/higher
         gigls/irgb
         gigls/list
         gigls/mgimp
         gigls/misc
         gigls/positions
         gigls/rgb-core
         gigls/tile
         gigls/turtles
         gigls/utils
         )

(provide (all-defined-out)
         (all-from-out gigls/pdb-dbus)
         (all-from-out gigls/alt)
         (all-from-out gigls/brushes)
         (all-from-out gigls/colors)
         (all-from-out gigls/context)
         (all-from-out gigls/current-brush)
         (all-from-out gigls/drawings)
         (all-from-out gigls/guard)
         (all-from-out gigls/hacks)
         (all-from-out gigls/irgb)
         (all-from-out gigls/higher)
         (all-from-out gigls/image)
         (all-from-out gigls/list) 
         (all-from-out gigls/mgimp)
         (all-from-out gigls/misc)
         (all-from-out gigls/positions)
         (all-from-out gigls/rgb-core)
         (all-from-out gigls/tile)
         (all-from-out gigls/turtles)
         (all-from-out gigls/utils)
         gigls-version
         )

