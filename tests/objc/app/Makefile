HC      = ghc
HCFLAGS = 
LDFLAGS = -package template-haskell -package language-c-quote -package language-c-inline -framework Cocoa -optl-ObjC

OBJS = Main.o App.o App_objc.o AppDelegate.o AppDelegate_objc.o

default: HSApp.app/Contents/MacOS/HSApp

%.o: %.hs
	$(HC) -c $< $(HCFLAGS)

AppDelegate.o:
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
	rm -f *.o *.hi App_objc.[hm] AppDelegate_objc.[hm] HSApp HSApp.app/Contents/MacOS/HSApp
	