{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TestInlineObjC (objc_initialise, dumpURL) where

import Language.C.Quote.ObjC
import InlineObjC

objc_import ["<Foundation/Foundation.h>"]


dumpURL :: String -> IO ()
dumpURL urlString
  = do
    { urlData <- $(objc 'urlString [cexp| 
                   [NSString stringWithContentsOfURL: [NSURL URLWithString: [NSString stringWithUTF8String: urlString]]
                             encoding: NSUTF8StringEncoding 
                             error: NULL] 
                 |])
    ; putStr urlData
    }

objc_emit
