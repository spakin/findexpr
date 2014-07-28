##########################################
# Build the findexpr program             #
# By Scott Pakin <scott-fexpr@pakin.org> #
##########################################

HS = ghc
HSOPTS = -O -rtsopts -with-rtsopts=-K2G

PROGRAM = findexpr
SOURCES = Main.hs ParseInput.hs FindExpr.hs
OBJECTS = $(patsubst %.hs,%.o,$(SOURCES))
IFACES = $(patsubst %.hs,%.hi,$(SOURCES))

all: $(PROGRAM)

$(PROGRAM): $(SOURCES)
	$(HS) $(HSOPTS) --make $(SOURCES) -o $(PROGRAM)

clean:
	$(RM) $(PROGRAM) $(OBJECTS) $(IFACES)

.PHONY: all clean
