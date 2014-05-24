{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- HSApp: a simple Cocoa app in Haskell
--
-- Main application module entering AppKit's application framework

module App (main, objc_initialise) where

import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>"]


main :: IO ()
main = $(objc [] $
          void [cexp| NSApplicationMain (0, NULL) |])
                        -- 'NSApplicationMain' ignores its argc and argv arguments anyway

objc_emit
