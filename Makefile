##########################################
# Build the findexpr program             #
# By Scott Pakin <scott-fexpr@pakin.org> #
##########################################

prefix = /usr/local
bindir = $(prefix)/bin

HS = ghc
HSOPTS = -O -optc-O3 -threaded -feager-blackholing -rtsopts -with-rtsopts="-K2G -N"
HADDOCK = haddock
HADDOCK_OPTS = --html --ignore-all-exports

PROGRAM = findexpr
SOURCES = Main.hs ParseInput.hs StackGen.hs FindExpr.hs
OBJECTS = $(patsubst %.hs,%.o,$(SOURCES))
IFACES = $(patsubst %.hs,%.hi,$(SOURCES))
INSTALL = install

all: $(PROGRAM)

$(PROGRAM): $(SOURCES)
	$(HS) $(HSOPTS) --make $(SOURCES) -o $(PROGRAM)

doc:
	$(HADDOCK) $(HADDOCK_OPTS) --odir=doc $(SOURCES)

install: all
	$(INSTALL) -D -m 0755 $(PROGRAM) $(DESTDIR)$(bindir)/$(PROGRAM)

uninstall:
	$(RM) $(DESTDIR)$(bindir)/$(PROGRAM)

clean:
	$(RM) $(PROGRAM) $(OBJECTS) $(IFACES)
	$(RM) -r doc

.PHONY: all doc clean
