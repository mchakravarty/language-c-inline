{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TestInlineObjC (objc_initialise, dumpURL) where

import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Foundation/Foundation.h>"]


dumpURL :: String -> IO ()
dumpURL urlString
  = do
    { urlData <- $(objc ['urlString :> ''String] $ ''String <: [cexp| 
                   [NSString stringWithContentsOfURL:[NSURL URLWithString:urlString]
                             encoding:NSUTF8StringEncoding 
                             error:NULL] 
                 |])
    ; putStr urlData
    }

objc_emit
