HC      = ghc
CFLAGS  = -fobjc-arc -I$(shell $(HC) --print-libdir)/include
HCFLAGS =
LDFLAGS = -package template-haskell -package language-c-quote -package language-c-inline -framework Foundation

OBJS = Main.o Main_objc.o Particle.o Particle_objc.o 

default: Particle

%.o: %.hs
	$(HC) -c $< $(HCFLAGS)

Particle.o:
Main.o: Particle.o

Main_objc.m: Main.o
Particle_objc.m: Particle.o

Particle: $(OBJS)
	$(HC) -o $@ $^ $(LDFLAGS)

.PHONY: clean

clean:
	rm -f *.o *.hi Main_objc.[hm] Particle_objc.[hm] *_stub.h Particle
