CFLAGS = -g -Wall

default: gimp-dbus.rkt

clean: rm gimp-dbus.rkt

# Expose exposes this directory to DrRacket
expose:
	raco link `pwd`

gimp-dbus.rkt: funs mkwrapper
	./mkwrapper funs > gimp-dbus.rkt
