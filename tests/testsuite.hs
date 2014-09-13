{-# LANGUAGE CPP #-}

import Language.C.Quote.C
import Language.C.Inline.C
import Language.Haskell.TH
import Test.Hspec
import Test.HUnit (assertBool)

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

-- Hack together a simple type class to represent
-- approximate equality for floating-point types.
-- It is only intended for the simple cases used here;
-- I assume there's something out there on Hackage
-- which has a more-principled approach.
--
class EqualIsh a where
    -- | Returns `True` if the two values are close
    --   enough to be considered equal.
    (=~) :: a -> a -> Bool
    (=~) a b = not $ a /=~ b

    -- | Returns `True` if the two values are not close
    --   enough to be considered equal.
    (/=~) :: a -> a -> Bool
    (/=~) a b = not $ a =~ b

instance EqualIsh a => EqualIsh [a] where
    as =~ bs = and $ zipWith (=~) as bs

-- | This is a very naive definition of approximate
--   eqaulity (absolute difference within 1e-15;
--   this value is just picked at random rather than
--   based on anything sensible like the ulp or a
--   fractional difference).
instance EqualIsh Double where
    a =~ b = let d = abs $ a - b
             in  d < 1e-15

-- | This is a very naive definition of approximate
--   eqaulity (absolute difference within 5e-7;
--   this value is just picked at random rather than
--   based on anything sensible like the ulp or a
--   fractional difference).
instance EqualIsh Float where
    a =~ b = let d = abs $ a - b
             in  d < 5e-7

-- |
-- @actual \`shouldBeIsh\` expected@ sets the expectation that @actual@ is equal
-- to @expected@ (as defined by the `EqualIsh` instance).
shouldBeIsh :: (Show a, EqualIsh a) => a -> a -> Expectation
actual `shouldBeIsh` expected = 
    assertBool 
    ("Value " ++ show actual ++ " not equalIsh to " ++ show expected)
    (actual =~ expected)

-- |
-- @action \`shouldReturnIsh\` expected@ sets the expectation that @action@
-- returns @expected@ (as defined by the `EqualIsh` instance).
shouldReturnIsh :: (Show a, EqualIsh a) => IO a -> a -> Expectation
action `shouldReturnIsh` expected = action >>= (`shouldBeIsh` expected)

testPosixC :: Spec
testPosixC =
    describe "PosixC:" $ do
        it "Can pass an Int through a C function." $
            mapM cPlusOne [0, 1, 2] `shouldReturn` [ 1, 2, 3 ]
        it "Can compare two strings using C strcmp." $ do
            cStringCompare "abc" "bbc" `shouldReturn` LT
            cStringCompare "bbc" "abc" `shouldReturn` GT
            cStringCompare "abc" "abc" `shouldReturn` EQ
            cStringCompare ""    ""    `shouldReturn` EQ
        it "Can retreve Strings from C code." $
            cGetString `shouldReturn` "Hello Haskell"
        it "Can pass Strings through C code.." $ do
            cPassString "abc" `shouldReturn` "abc"
            cPassString "abcde" `shouldReturn` "abcde"
            cPassString "" `shouldReturn` ""

        -- type specifiers are not required; added for
        -- documentation
        it "Can pass a Double through a C function.." $
            mapM cSin [0, pi/2.0, pi] `shouldReturnIsh` [ 0.0, 1.0, 0.0 :: Double ]
        it "Can pass a Float through a C function.." $
            mapM cSinF [0, pi/2.0, pi] `shouldReturnIsh` [ 0.0, 1.0, 0.0 :: Float ]
        it "Can pass a Double through a slightly-complicated C function.." $
            mapM cInvertSin [0.0, 1.0] `shouldReturnIsh` [0.0, 1.0 :: Double ]
        it "Can pass a Float through a slightly-complicated C function.." $
            mapM cInvertSinF [0.0, 1.0] `shouldReturnIsh` [0.0, 1.0 :: Float ]
