{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Test.PosixC
    ( cPlusOne
    , cStringCompare
    , cGetString
    , cPassString

    -- The module initialisation function.
    , posixCInit
    ) where

import Control.Applicative
import Language.C.Quote.C
import Language.C.Inline.C

import Language.Haskell.TH

c_import ["<stdio.h>", "<ctype.h>"]

cPlusOne :: Int -> IO Int
cPlusOne i = $(c [ 'i :> ''Int ] (''Int <: [cexp| (i + 1) |]))

cStringCompare :: String -> String -> IO Ordering
cStringCompare s1 s2 =
    compareInt <$> $(c [ 's1 :> ''String, 's2 :> ''String ] (''Int <: [cexp| strcmp (s1, s2) |]))
  where
    compareInt i
        | i > 0 = GT
        | i < 0 = LT
        | otherwise = EQ


cGetString :: IO String
cGetString = $(c [] (''String <: [cexp| "Hello Haskell" |]))

cPassString :: String -> IO String
cPassString msg = $(c [ 'msg :> ''String ] (''String <: [cexp| msg |]))


c_emit

posixCInit :: IO ()
posixCInit = c_initialise
