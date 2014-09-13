##########################################
# Build the findexpr program             #
# By Scott Pakin <scott-fexpr@pakin.org> #
##########################################

HS = ghc
HSOPTS = -O -optc-O3 -threaded -feager-blackholing -rtsopts -with-rtsopts="-K2G -N"

PROGRAM = findexpr
SOURCES = Main.hs ParseInput.hs StackGen.hs FindExpr.hs
OBJECTS = $(patsubst %.hs,%.o,$(SOURCES))
IFACES = $(patsubst %.hs,%.hi,$(SOURCES))

all: $(PROGRAM)

$(PROGRAM): $(SOURCES)
	$(HS) $(HSOPTS) --make $(SOURCES) -o $(PROGRAM)

clean:
	$(RM) $(PROGRAM) $(OBJECTS) $(IFACES)

.PHONY: all clean
