HC      = ghc
LIBDIR  = $(shell $(HC) --print-libdir)
CFLAGS  = -fobjc-arc -I$(LIBDIR)/include -I$(LIBDIR)/../../includes -Wno-format-security
HCFLAGS = 
LDFLAGS = -package template-haskell -package language-c-quote -package language-c-inline -framework Foundation

OBJS = Main.o Main_objc.o

default: Simple

%.o: %.hs
	$(HC) -c $< $(HCFLAGS)

Main.o:

Main_objc.m: Main.o

Simple: $(OBJS)
	$(HC) -o $@ $^ $(LDFLAGS)

.PHONY: clean

clean:
	rm -f *.o *.hi Main_objc.[hm] Simple