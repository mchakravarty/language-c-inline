{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

import Language.Haskell.TH

objc_import ["<Foundation/Foundation.h>"]

nslog :: String -> IO ()

nslog msg = $(objc ['msg :> ''String] (void [cexp| NSLog(@"Here is a message from Haskell: %@", msg) |]))

objc_emit


main = objc_initialise >> nslog "I like Objective-C!"
