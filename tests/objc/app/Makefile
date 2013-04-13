HC      = ghc
HCFLAGS = 
LDFLAGS = -package template-haskell -package language-c-quote -package language-c-inline -framework Cocoa

OBJS = Main.o App.o App_objc.o

default: HSApp.app/Contents/MacOS/HSApp

%.o: %.hs
	$(HC) -c $< $(HCFLAGS)

App.o:
Main.o: App.o

App_objc.m: App.o

HSApp: $(OBJS)
	$(HC) -o $@ $^ $(LDFLAGS)

HSApp.app/Contents/MacOS/HSApp: HSApp
	cp $< $@

.PHONY: clean

clean:
	rm -f *.o *.hi App_objc.m HSApp HSApp.app/Contents/MacOS/HSApp
	