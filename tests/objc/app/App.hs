{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- Demonstrates how to write a Cocoa app in Haskell
--

module App (main, objc_initialise) where

import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>"]


main :: IO ()
main = $(objc [] ''()
          [cexp| NSApplicationMain (0, NULL) |])
                   -- 'NSApplicationMain' ignores its argc and argv arguments anyway

objc_emit
