Files in LoudGimp
=================

Primary Racket Files
--------------------
* `gimp-dbus.rkt`
  The basic bridge with the GIMP D-Bus server.
* `simplify.rkt`
  Simplified versions of the GIMP functions, as used in Grinnell's CSC 151.

Additional Racket Files
-----------------------
* `guard.rkt`
  Guards to allos the simplified GIMP functions to check their parameters.
* `higher.rkt`
  Some of SamR's favorite higher-order procedures.
* `stringify.rkt`
  value->string, which used to be provided in MzScheme, but doesn't seem
  to exist in Racket.

Miscellaneous
------------
* `funs`
  A list of all the functions exported by Gimp over D-Bus.  Generated
  from an appropriate call to loudbus-methods.
* `Makefile`
  Instructions for building everything.
* `mkwrapper.c
