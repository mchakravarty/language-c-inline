
HC = ghc

LIBDIR  := $(shell $(HC) --print-libdir)
CFLAGS  = -I$(LIBDIR)/include -I$(LIBDIR)/../../includes
HCFLAGS = 		# -ddump-splices
LDFLAGS = -package template-haskell -package language-c-quote


hs_lib_files := $(shell find Language -name \*.hs)

inline_tests := $(shell find Test -name \*.hs)
inline_objs := $(patsubst %.hs,%_c.o, $(inline_tests))

all :
	@ echo $(hs_objs)

check : testsuite
	./testsuite

testsuite : tests/dummy tests/testsuite.hs $(inline_objs)
	$(HC) tests/testsuite.hs $(inline_objs) $(LDFLAGS) -o $@

tests/dummy : tests/dummy.hs
	$(HC) --make -c $@

clean:
	@ rm -f testsuite tests/dummy $(shell find . -name *.o -o -name *.hi -o -name *.dyn_* -o -name *_c.[ch])
	@ cabal clean

#-------------------------------------------------------------------------------
# Rules

%.o: %.hs
	$(HC) -i. -c $< $(HCFLAGS)

%_c.c : %.hs
	$(HC) -c $< $(HCFLAGS)
