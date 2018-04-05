##########################################
# Build the findexpr program             #
# By Scott Pakin <scott-fexpr@pakin.org> #
##########################################

prefix = /usr/local
bindir = $(prefix)/bin
datarootdir = $(prefix)/share
mandir = $(datarootdir)/man
man1dir = $(mandir)/man1

VERSION = 1.1

HS = ghc
HSOPTS = -O -optc-O3 -threaded -feager-blackholing -rtsopts -with-rtsopts="-K2G -N"
HADDOCK = haddock
HADDOCK_OPTS = --html --ignore-all-exports

SOURCES = Main.hs ParseInput.hs StackGen.hs FindExpr.hs
OBJECTS = $(patsubst %.hs,%.o,$(SOURCES))
IFACES = $(patsubst %.hs,%.hi,$(SOURCES))
INSTALL = install

all: findexpr

findexpr: $(SOURCES)
	$(HS) $(HSOPTS) --make $(SOURCES) -o findexpr

findexpr.1: userguide.txt
	rst2man userguide.txt > findexpr.1

devel-doc: $(SOURCES)
	$(HADDOCK) $(HADDOCK_OPTS) --odir=devel-doc $(SOURCES)

install: all findexpr.1
	$(INSTALL) -d -m 0755 $(DESTDIR)$(bindir)
	$(INSTALL) -m 0755 findexpr $(DESTDIR)$(bindir)/findexpr
	$(INSTALL) -d -m 0755 $(DESTDIR)$(man1dir)
	$(INSTALL) -m 0644 findexpr.1 $(DESTDIR)$(man1dir)/findexpr.1

uninstall:
	$(RM) $(DESTDIR)$(bindir)/findexpr
	$(RM) $(DESTDIR)$(man1dir)/findexpr.1

dist: findexpr-$(VERSION).tar.gz

findexpr-$(VERSION).tar.gz: Makefile $(SOURCES) userguide.txt findexpr.1
	$(RM) -r findexpr-$(VERSION)
	$(INSTALL) -d -m 0755 findexpr-$(VERSION)
	$(INSTALL) -m 0755 Makefile $(SOURCES) findexpr-$(VERSION)
	$(INSTALL) -m 0644 userguide.txt findexpr.1 README.md findexpr-$(VERSION)
	tar -czvf findexpr-$(VERSION).tar.gz findexpr-$(VERSION)
	$(RM) -r findexpr-$(VERSION)

clean:
	$(RM) findexpr $(OBJECTS) $(IFACES)
	$(RM) -r devel-doc findexpr-$(VERSION)

maintainer-clean: clean
	$(RM) findexpr.1

.PHONY: all clean maintainer-clean install uninstall dist
