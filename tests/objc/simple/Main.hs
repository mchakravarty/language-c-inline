{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

import Foreign hiding (void)
import Foreign.C

import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

import Language.Haskell.TH

objc_import ["<Foundation/Foundation.h>"]

testPtrMarshalling
  = withCStringLen "String marshalled explicitly" $ \(ptr, len) ->
      $(objc ['ptr :> [t| Ptr CChar |], 'len :> ''Int] $ void
          [cexp| NSLog([[NSString alloc] initWithBytes:ptr length:len encoding:NSUTF8StringEncoding]) |])

objc_emit

main = objc_initialise >> testPtrMarshalling
