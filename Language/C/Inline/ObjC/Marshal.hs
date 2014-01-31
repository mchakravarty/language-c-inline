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
import Foreign.Ptr                as C
import Foreign.StablePtr          as C
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

-- |Check that the given TH name is that of a Haskell variable and determine its type.
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

-- |Check that the given TH name is that of a Haskell type constructor.
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

-- |Determine the C type that we map a given Haskell type to.
--
haskellTypeToCType :: QC.Extensions -> TH.Type -> Q QC.Type
haskellTypeToCType lang ty
  = case haskellTypeToCType' lang ty of
      Nothing  -> reportErrorAndFail lang $ "don't know a foreign type suitable for Haskell type '" ++ TH.pprint ty ++ "'"
      Just cty -> return cty

haskellTypeToCType' :: QC.Extensions -> TH.Type -> Maybe QC.Type
haskellTypeToCType' lang (ForallT _tvs _ctxt ty)           -- ignore quantifiers and contexts
  = haskellTypeToCType' lang ty
haskellTypeToCType' lang (ListT `AppT` (ConT char))        -- marshal '[Char]' as 'String'
  | char == ''Char 
  = haskellTypeNameToCType' lang ''String
haskellTypeToCType' lang (ConT maybeC `AppT` argTy)        -- encode a 'Maybe' around a pointer type in the pointer
  | maybeC == ''Maybe && maybe False isCPtrType cargTy
  = cargTy
  where
    cargTy = haskellTypeToCType' lang argTy
haskellTypeToCType' lang (ConT tc)                         -- nullary type constructors are delegated
  = haskellTypeNameToCType' lang tc
haskellTypeToCType' lang ty@(VarT tv)                      -- can't marshal an unknown type
  = Nothing
haskellTypeToCType' _lang ty                               -- everything else is marshalled as a stable pointer
  = Just [cty| typename HsStablePtr |]

-- |Determine the C type that we map a given Haskell type constructor to — i.e., we map all Haskell
-- whose outermost constructor is the given type constructor to the returned C type..
--
haskellTypeNameToCType :: QC.Extensions -> TH.Name -> Q QC.Type
haskellTypeNameToCType ext tyname
  = case haskellTypeNameToCType' ext tyname of
      Nothing  -> reportErrorAndFail ObjC $ "don't know a foreign type suitable for Haskell type '" ++ show tyname ++ "'"
      Just cty -> return cty

haskellTypeNameToCType' :: QC.Extensions -> TH.Name -> Maybe QC.Type
haskellTypeNameToCType' ObjC tyname
  | tyname == ''String = Just [cty| typename NSString * |]       -- 'String' -> '(NSString *)'
  | tyname == ''()     = Just [cty| void |]                      -- '()' -> 'void'
haskellTypeNameToCType' _lang tyname                             -- <everything else> -> 'HsStablePtr'
  = Just [cty| typename HsStablePtr |]

-- Check whether the given C type is an overt pointer.
--
isCPtrType :: QC.Type -> Bool
isCPtrType (Type _ (Ptr {}) _)           = True
isCPtrType (Type _ (BlockPtr {}) _)      = True
isCPtrType (Type _ (Array {}) _)         = True
isCPtrType ty
  | ty == [cty| typename HsStablePtr |]  = True
  | otherwise                            = False


-- Determine marshallers and their bridging types
-- ----------------------------------------------

-- |Constructs Haskell code to marshal a value (used to marshal arguments and results).
--
-- * The first argument is the code referring to the value to be marshalled.
-- * The second argument is the continuation that gets the marshalled value as an argument.
--
type HaskellMarshaller = TH.ExpQ -> TH.ExpQ -> TH.ExpQ

-- |Constructs C code to marshal an argument (used to marshal arguments and results).
--
-- * The argument is the identifier of the value to be marshalled.
-- * The result of the generated expression is the marshalled value.
--
type CMarshaller = TH.Name -> QC.Exp

-- |Generate the type-specific marshalling code for Haskell to C land marshalling for a Haskell-C type pair.
--
-- The result has the following components:
--
-- * Haskell type after Haskell-side marshalling.
-- * C type before C-side marshalling.
-- * Generator for the Haskell-side marshalling code.
-- * Generator for the C-side marshalling code.
--
generateHaskellToCMarshaller :: TH.Type -> QC.Type -> Q (TH.TypeQ, QC.Type, HaskellMarshaller, CMarshaller)
generateHaskellToCMarshaller hsTy@(ConT maybe `AppT` argTy) cTy
  | maybe == ''Maybe && isCPtrType cTy
  = do 
    { (argTy', cTy', hsMarsh, cMarsh) <- generateHaskellToCMarshaller argTy cTy
    ; ty <- argTy'
    ; case ty of
        ConT ptr `AppT` _ 
          | ptr == ''C.Ptr       -> return ( argTy'
                                           , cTy'
                                           , \val cont -> [| case $val of
                                                               Nothing   -> $cont C.nullPtr
                                                               Just val' -> $(hsMarsh [|val'|] cont) |] 
                                           , cMarsh
                                           )
          | ptr == ''C.StablePtr -> return ( argTy'
                                           , cTy'
                                           , \val cont -> [| case $val of
                                                               Nothing   -> $cont (C.castPtrToStablePtr C.nullPtr)
                                                               Just val' -> $(hsMarsh [|val'|] cont) |]
                                                               -- NB: the above cast works for GHC, but is in the grey area
                                                               --     of the FFI spec
                                           , cMarsh
                                           )
        _ -> reportErrorAndFail ObjC $ "missing 'Maybe' marshalling for '" ++ prettyQC cTy ++ "' to '" ++ TH.pprint hsTy ++ "'"
    }
generateHaskellToCMarshaller hsTy cTy
  | cTy == [cty| typename NSString * |] 
  = return ( [t| C.CString |]
           , [cty| char * |]
           , \val cont -> [| C.withCString $val $cont |]
           , \argName -> [cexp| [NSString stringWithUTF8String: $id:(show argName)] |]
           )
  | cTy == [cty| typename HsStablePtr |] 
  = return ( [t| C.StablePtr $(return hsTy) |]
           , cTy
           , \val cont -> [| do { C.newStablePtr $val >>= $cont } |]
           , \argName -> [cexp| $id:(show argName) |]
           )
  | otherwise
  = reportErrorAndFail ObjC $ "cannot marshal '" ++ TH.pprint hsTy ++ "' to '" ++ prettyQC cTy ++ "'"

-- |Generate the type-specific marshalling code for Haskell to C land marshalling for a C-Haskell type pair.
--
-- The result has the following components:
--
-- * Haskell type after Haskell-side marshalling.
-- * C type before C-side marshalling.
-- * Generator for the Haskell-side marshalling code.
-- * Generator for the C-side marshalling code.
--
generateCToHaskellMarshaller :: TH.Type -> QC.Type -> Q (TH.TypeQ, QC.Type, HaskellMarshaller, CMarshaller)
generateCToHaskellMarshaller hsTy cTy
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
  | cTy == [cty| typename HsStablePtr |] 
  = return ( [t| C.StablePtr $(return hsTy) |]
           , cTy
           , \val cont -> [| do { C.deRefStablePtr $val >>= $cont } |]
           , \argName -> [cexp| $id:(show argName) |]
           )
  | cTy == [cty| void |]
  = return ( [t| () |]
           , [cty| void |]
           , \val cont -> [| $cont $val |]
           , \argName -> [cexp| $id:(show argName) |]
           )
  | otherwise
  = reportErrorAndFail ObjC $ "cannot marshall '" ++ prettyQC cTy ++ "' to '" ++ TH.pprint hsTy ++ "'"    

