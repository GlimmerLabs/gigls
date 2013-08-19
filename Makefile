

# +----------+--------------------------------------------------------
# | Settings |
# +----------+

CFLAGS = -g -Wall


# +---------------+---------------------------------------------------
# | Sets of Files |
# +---------------+

# The extensions to racket implemented in C.
C_EXTENSIONS = \
	$(COMPILED_DIR)/irgb-core.so \
	$(COMPILED_DIR)/tile-core.so


# +-------------------------------------------------------------------
# | Special Settings for Racket C Stuff |
# +-------------------------------------+

RACO_CFLAGS = $(shell echo '' $(CFLAGS) | utils/racocflags)

# Inside Racket says to use --cgc, but that requires mzdyn.o, which does
# not seem to ship with the standard distribution, and I'm lazy.
RACO_GC = --3m

# We need to know where to put the compiled Racket library.
COMPILED_DIR = $(shell racket utils/compiled-goes-here.rkt)


# +------------------+------------------------------------------------
# | Standard Targets |
# +------------------+

default: pdb-dbus.rkt $(C_EXTENSIONS)

clean: 
	rm -f pdb-funs pdb-dbus.rkt


# +-----------------+-------------------------------------------------
# | Special Actions |
# +-----------------+

# Expose exposes this directory to DrRacket
expose:
	raco link `pwd`

# +-----------------+-------------------------------------------------
# | Special Targets |
# +-----------------+

pdb-dbus.rkt: pdb-funs mkwrapper
	./mkwrapper $< > $@

pdb-funs:
	racket utils/list-pdb-funs.rkt > pdb-funs

rebuild-funs: 
	rm -f pdb-funs
	make pdb-funs

irgb-core.o: irgb-core.c
	raco ctool --cc $(RACO_GC) $(RACO_CFLAGS) $<

irgb-core.so: irgb-core.o
	raco ctool --vv $(RACO_RC) ++ldf -L/usr/lib/x86_64-linux-gnu $(RACO_LDLIBS) --ld $@ $^

$(COMPILED_DIR)/irgb-core.so: irgb-core.so
	install -D $< $@

tile-core.o: tile-core.c
	raco ctool --cc $(RACO_GC) $(RACO_CFLAGS) $<

tile-core.so: tile-core.o
	raco ctool --vv $(RACO_RC) ++ldf -L/usr/lib/x86_64-linux-gnu $(RACO_LDLIBS) --ld $@ $^

$(COMPILED_DIR)/tile-core.so: tile-core.so
	install -D $< $@

# +-------------+-----------------------------------------------------
# | Experiments |
# +-------------+

compiled-dir:
	echo $(COMPILED_DIR)
