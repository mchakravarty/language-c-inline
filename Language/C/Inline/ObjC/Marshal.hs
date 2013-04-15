{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- |
-- Module      : Language.C.Inline.ObjC.Marshal
-- Copyright   : [2013] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Objective-C-specific marshalling functions.
--
-- FIXME: Some of the code can go into a module for general marshalling, as only some of it is ObjC-specific.

module Language.C.Inline.ObjC.Marshal (
  -- * Auxilliary functions
  determineVarType, checkTypeName,
  
  -- * Determine corresponding foreign types of Haskell types
  haskellTypeToCType, haskellTypeNameToCType,
  
  -- * Marshaller types
  HaskellMarshaller, CMarshaller,
  
  -- * Compute bridging types and marshallers
  generateHaskellToCMarshaller, generateCToHaskellMarshaller
) where

  -- common libraries
import Foreign.C                  as C
import Foreign.C.String           as C
import Foreign.Marshal            as C
import Language.Haskell.TH        as TH
import Language.Haskell.TH.Syntax as TH

  -- quasi-quotation libraries
import Language.C.Quote           as QC
import Language.C.Quote.ObjC      as QC
import Text.PrettyPrint.Mainland  as QC

  -- friends
import Language.C.Inline.Error


-- Auxilliary functions
-- --------------------

-- Check that the given TH name is that of a Haskell variable and determine its type.
--
determineVarType :: TH.Name -> Q TH.Type
determineVarType vname
  = do
    { vinfo <- reify vname
    ; case vinfo of
        VarI _ ty _ _ -> return ty
        nonVarInfo    -> 
          do
          { reportErrorAndFail QC.ObjC $ 
              "expected '" ++ show vname ++ "' to be a variable name, but it is " ++ 
              show (TH.ppr nonVarInfo)
          }
    }

-- Check that the given TH name is that of a Haskell type constructor.
--
checkTypeName :: TH.Name -> Q ()
checkTypeName tyname
  = do
    { tyinfo <- reify tyname
    ; case tyinfo of
        TyConI (DataD {})    -> return ()
        TyConI (NewtypeD {}) -> return ()
        TyConI (TySynD {})   -> return ()
        nonTyInfo  -> 
          do
          { reportErrorAndFail QC.ObjC $ 
              "expected '" ++ show tyname ++ "' to be a type name, but it is " ++ 
              show (TH.ppr nonTyInfo)
          }
    }


-- Determine foreign types
-- -----------------------

-- Determine the C type that we map a given Haskell type to.
--
haskellTypeToCType :: QC.Extensions -> TH.Type -> Q QC.Type
haskellTypeToCType lang (ListT `AppT` (ConT char))
  | char == ''Char 
  = haskellTypeNameToCType lang ''String
haskellTypeToCType lang (ConT tc)
  = haskellTypeNameToCType lang tc
haskellTypeToCType lang ty
  = reportErrorAndFail lang $ "don't know a foreign type suitable for Haskell type '" ++ TH.pprint ty ++ "'"

-- Determine the C type that we map a given Haskell type constructor to — i.e., we map all Haskell
-- whose outermost constructor is the given type constructor to the returned C type..
--
haskellTypeNameToCType :: QC.Extensions -> TH.Name -> Q QC.Type
haskellTypeNameToCType ObjC tyname
  | tyname == ''String = return [cty| typename NSString * |]
  | tyname == ''()     = return [cty| void |]
haskellTypeNameToCType _lang tyname
  = reportErrorAndFail ObjC $ "don't know a foreign type suitable for Haskell type name '" ++ show tyname ++ "'"


-- Determine marshallers and their bridging types
-- ----------------------------------------------

-- Constructs Haskell code to marshal a value (used to marhsall arguments and results).
--
-- * The first argument is the code referring to the value to be marshalled.
-- * The second argument is the continuation that gets the marshalled value as an argument.
--
type HaskellMarshaller = TH.ExpQ -> TH.ExpQ -> TH.ExpQ

-- Constructs C code to marhsall an argument (used to marshal arguments and results).
--
-- * The argument is the identifier of the value to be marshalled.
-- * The result of the generated expression is the marshalled value.
--
type CMarshaller = TH.Name -> QC.Exp
  
generateHaskellToCMarshaller :: TH.Type -> QC.Type -> Q (TH.TypeQ, QC.Type, HaskellMarshaller, CMarshaller)
generateHaskellToCMarshaller hsTy cTy
  | cTy == [cty| typename NSString * |] 
  = return ( [t| C.CString |]
           , [cty| char * |]
           , \val cont -> [| C.withCString $val $cont |]
           , \argName -> [cexp| [NSString stringWithUTF8String: $id:(show argName)] |]
           )
  | otherwise
  = reportErrorAndFail ObjC $ "cannot marshal '" ++ TH.pprint hsTy ++ "' to '" ++ prettyQC cTy ++ "'"

generateCToHaskellMarshaller :: TH.Name -> QC.Type -> Q (TH.TypeQ, QC.Type, HaskellMarshaller, CMarshaller)
generateCToHaskellMarshaller hsTyName cTy
  | cTy == [cty| typename NSString * |]
  = return ( [t| C.CString |]
           , [cty| char * |]
           , \val cont -> [| do { str <- C.peekCString $val; C.free $val; $cont str } |]
           , \argName -> 
               let arg = show argName 
               in
               [cexp|
                 ({ typename NSUInteger maxLen = [$id:arg maximumLengthOfBytesUsingEncoding:NSUTF8StringEncoding] + 1;
                   char *buffer = malloc (maxLen);
                   if (![$id:arg getCString:buffer maxLength:maxLen encoding:NSUTF8StringEncoding])
                     *buffer = '\0';
                   buffer;
                 })
               |]
           )
  | cTy == [cty| void |]
  = return ( [t| () |]
           , [cty| void |]
           , \val cont -> [| $cont $val |]
           , \argName -> [cexp| $id:(show argName) |]
           )
  | otherwise
  = reportErrorAndFail ObjC $ "cannot marshall '" ++ prettyQC cTy ++ "' to '" ++ pprint hsTyName ++ "'"    

