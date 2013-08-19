gigls
=====

The Glimmer Improved Gimp Library for Scripting.  A Racket library to
access Gimp over DBus (requires the louDBus library).

To build gigls:

1. Clone and install https://github.com/GlimmerLabs/louDBus, the
Glimmer Labs library for accessing DBus from Racket.

2. Clone and install https://github.com/GlimmerLabs/gimp-dbus, the
Glimmer Labs library for providing Gimp PDB and other functions over
DBus.

3. Start Gimp.  Start the Gimp DBus server.

4. Make gigls

5. In Racket, use (require gigls/unsafe).

Organized list of functions forthcoming.

