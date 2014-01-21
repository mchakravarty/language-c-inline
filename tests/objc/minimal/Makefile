HC      = ghc
HCFLAGS = 
LDFLAGS = -package template-haskell -package language-c-quote -package language-c-inline -framework Foundation

OBJS = Main.o Main_objc.o

default: Minimal

%.o: %.hs
	$(HC) -c $< $(HCFLAGS)

Main.o:

Main_objc.m: Main.o

Minimal: $(OBJS)
	$(HC) -o $@ $^ $(LDFLAGS)

.PHONY: clean

clean:
	rm -f *.o *.hi Main_objc.[hm] Minimal