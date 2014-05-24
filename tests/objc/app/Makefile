HC      = ghc
LIBDIR  = $(shell $(HC) --print-libdir)
CFLAGS  = -fobjc-arc -I$(LIBDIR)/include -I$(LIBDIR)/../../includes
HCFLAGS =
LDFLAGS = -package template-haskell -package language-c-quote -package language-c-inline -package hint \
          -framework Cocoa -optl-ObjC -threaded

OBJS = Main.o App.o App_objc.o AppDelegate.o AppDelegate_objc.o Interpreter.o

default: HSApp.app/Contents/MacOS/HSApp

%.o: %.hs
	$(HC) -c $< $(HCFLAGS)

Interpreter.o:
AppDelegate.o: Interpreter.o 
App.o:
Main.o: App.o AppDelegate.o

App_objc.m: App.o
AppDelegate_objc.m: AppDelegate.o

HSApp: $(OBJS)
	$(HC) -o $@ $^ $(LDFLAGS)

HSApp.app/Contents/MacOS/HSApp: HSApp
	cp $< $@

.PHONY: clean

clean:
	rm -f *.o *.hi App_objc.[hm] AppDelegate_objc.[hm] *_stub.h HSApp HSApp.app/Contents/MacOS/HSApp
	