{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveDataTypeable #-}

import Control.Monad hiding (void)
import Data.Typeable
import Foreign.ForeignPtr
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Foundation/Foundation.h>"]


newtype NSString = NSString (ForeignPtr NSString)
  deriving Typeable   -- needed for now until migrating to new TH

objc_typecheck -- make the above type declaration known to Template Haskell

stringToNSString :: String -> IO NSString
stringToNSString str
  = $(objc ['str :> ''String] $ Class ''NSString <: [cexp| str |])

newtype NSMutableArray e = NSMutableArray (ForeignPtr (NSMutableArray e))
  deriving Typeable   -- needed for now until migrating to new TH
newtype NSArray        e = NSArray        (ForeignPtr (NSArray        e))
  deriving Typeable   -- needed for now until migrating to new TH

unsafeFreezeNSMutableArray :: NSMutableArray e -> NSArray e
unsafeFreezeNSMutableArray (NSMutableArray fptr) = NSArray $ castForeignPtr fptr

objc_typecheck

listOfStringToNSArray :: [String] -> IO (NSArray NSString)
listOfStringToNSArray strs
  = do
    { marr <- $(objc [] $ Class [t|NSMutableArray NSString|] <: [cexp| [NSMutableArray arrayWithCapacity:10] |])
    ; mapM_ (addElement marr) strs
    ; return $ unsafeFreezeNSMutableArray marr
    }
  where
    addElement marr str
      = $(objc ['marr :> Class [t|NSMutableArray NSString|], 'str :> ''String] $ void [cexp| [marr addObject:str] |])

go :: IO ()
go 
  = do
    { arr <- listOfStringToNSArray msgs
    ; $(objc ['arr :> Class [t|NSArray NSString|]] $ void [cexp| NSLog(arr.description) |])
    }
  where
    msgs = ["Hello", "World!", "This is a bunch of 'String's!"]

objc_emit


main = objc_initialise >> go
