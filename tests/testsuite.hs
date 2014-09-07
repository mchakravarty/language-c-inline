{-# LANGUAGE CPP #-}

import Language.C.Quote.C
import Language.C.Inline.C
import Language.Haskell.TH
import Test.Hspec

import Test.PosixC


main :: IO ()
main = do
    posixCInit
#if (__APPLE__ && __MACH__)
    hspec $ do
        -- The PosixC test suite should also run on Mac, but the ObjC one
        -- needs Apple's ObjC.
        testPosixC
        testObjC
#else
    hspec
        testPosixC
#endif

#if (__APPLE__ && __MACH__)
testObjC :: Spec
testObjC =
    describe "ObjC:" $
        error "Not ObjC test suite defined yet."
#endif


testPosixC :: Spec
testPosixC =
    describe "PosixC:" $ do
        it "Can pass an Int through a C function." $
            mapM cPlusOne [0, 1, 2] `shouldReturn` [ 1, 2, 3 ]
        it "Can compare two strings using C strcmp." $ do
            cStringCompare "abc" "bbc" `shouldReturn` LT
            cStringCompare "bbc" "abc" `shouldReturn` GT
            cStringCompare "abc" "abc" `shouldReturn` EQ
        it "Can retreve Strings from C code." $
            cGetString `shouldReturn` "Hello Haskell"
        it "Can pass Strings through C code.." $ do
            cPassString "abc" `shouldReturn` "abc"
            cPassString "abcde" `shouldReturn` "abcde"
