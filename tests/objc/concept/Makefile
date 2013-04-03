HC      = ghc
HCFLAGS = 
LDFLAGS = -package template-haskell -package language-c-quote -package language-c-inline -framework Foundation

OBJS = TestInlineObjC.o TestInlineObjC_objc.o MainInlineObjC.o

default: InlineObjC

%.o: %.hs
	$(HC) -c $< $(HCFLAGS)

TestInlineObjC.o:
MainInlineObjC.o: TestInlineObjC.o

TestInlineObjC_objc.m: TestInlineObjC.o

InlineObjC: $(OBJS)
	$(HC) -o $@ $^ $(LDFLAGS)

.PHONY: clean

clean:
	rm -f *.o *.hi TestInlineObjC_objc.m InlineObjC