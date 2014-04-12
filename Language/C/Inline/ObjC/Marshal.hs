{-# LANGUAGE PatternGuards, TemplateHaskell, QuasiQuotes #-}

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
import Data.Map                   as Map
import Data.Word
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
haskellTypeToCType lang (ForallT _tvs _ctxt ty)           -- ignore quantifiers and contexts
  = haskellTypeToCType lang ty
haskellTypeToCType lang (ListT `AppT` (ConT char))        -- marshal '[Char]' as 'String'
  | char == ''Char 
  = haskellTypeNameToCType lang ''String
haskellTypeToCType lang ty@(ConT maybeC `AppT` argTy)     -- encode a 'Maybe' around a pointer type in the pointer
  | maybeC == ''Maybe
  = do
    { cargTy <- haskellTypeToCType lang argTy
    ; if isCPtrType cargTy
      then
        return cargTy
      else
        unknownType lang ty
    }
haskellTypeToCType lang (ConT tc)                         -- nullary type constructors are delegated
  = haskellTypeNameToCType lang tc
haskellTypeToCType lang ty@(VarT tv)                      -- can't marshal an unknown type
  = unknownType lang ty
haskellTypeToCType lang ty@(UnboxedTupleT _)              -- there is nothing like unboxed tuples in C
  = unknownType lang ty
haskellTypeToCType _lang ty                               -- everything else is marshalled as a stable pointer
  = return [cty| typename HsStablePtr |]

unknownType lang ty = reportErrorAndFail lang $ "don't know a foreign type suitable for Haskell type '" ++ TH.pprint ty ++ "'"

-- |Determine the C type that we map a given Haskell type constructor to — i.e., we map all Haskell types
-- whose outermost constructor is the given type constructor to the returned C type.
--
-- All types representing boxed values that are not explicitly mapped to a specific C type, are mapped to
-- stable pointers.
--
haskellTypeNameToCType :: QC.Extensions -> TH.Name -> Q QC.Type
haskellTypeNameToCType ext tyname
  = case Map.lookup tyname (haskellToCTypeMap ext) of
      Just cty -> return cty
      Nothing  -> do
        { info <- reify tyname
        ; case info of
            PrimTyConI _ _ True -> unknownUnboxedType
            _                   -> return [cty| typename HsStablePtr |]
        }
  where
    unknownUnboxedType = reportErrorAndFail ext $ 
                           "don't know a foreign type suitable for the unboxed Haskell type '" ++ show tyname ++ "'"  

haskellToCTypeMap :: QC.Extensions -> Map TH.Name QC.Type
haskellToCTypeMap ObjC
  = Map.fromList
    [ (''CChar,   [cty| char |])
    , (''CSChar,  [cty| signed char |])
    , (''CUChar,  [cty| unsigned char |])
    , (''CShort,  [cty| short |])
    , (''CUShort, [cty| unsigned short |])
    , (''Int,     [cty| int |])
    , (''CInt,    [cty| int |])
    , (''Word,    [cty| unsigned int |])
    , (''CUInt,   [cty| unsigned int |])
    , (''CLong,   [cty| long |])
    , (''CULong,  [cty| unsigned long |])
    , (''CLLong,  [cty| long long |])
    , (''CULLong, [cty| unsigned long long |])
    --
    , (''Float,   [cty| float |])
    , (''CFloat,  [cty| float |])
    , (''Double,  [cty| double |])
    , (''CDouble, [cty| double |])
    --
    , (''String,  [cty| typename NSString * |])
    , (''(),      [cty| void |])
    ]
haskellToCTypeMap _lang
  = Map.empty

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
  | Just hsMarshalTy <- Map.lookup cTy cIntegralMap    -- checking whether it is an integral type
  = return ( hsMarshalTy
           , cTy
           , \val cont -> [| $cont (fromIntegral $val) |]
           , \argName -> [cexp| $id:(show argName) |]
           )
  | Just hsMarshalTy <- Map.lookup cTy cFloatingMap    -- checking whether it is a floating type
  = return ( hsMarshalTy
           , cTy
           , \val cont -> [| $cont (realToFrac $val) |]
           , \argName -> [cexp| $id:(show argName) |]
           )
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
  | Just hsMarshalTy <- Map.lookup cTy cIntegralMap    -- checking whether it is an integral type
  = return ( hsMarshalTy
           , cTy
           , \val cont -> [| $cont (fromIntegral $val) |]
           , \argName -> [cexp| $id:(show argName) |]
           )
  | Just hsMarshalTy <- Map.lookup cTy cFloatingMap    -- checking whether it is a floating type
  = return ( hsMarshalTy
           , cTy
           , \val cont -> [| $cont (realToFrac $val) |]
           , \argName -> [cexp| $id:(show argName) |]
           )
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

cIntegralMap = Map.fromList
               [ ([cty| char |],               [t| C.CChar |])
               , ([cty| signed char |],        [t| C.CChar |])
               , ([cty| unsigned char |],      [t| C.CUChar |])
               , ([cty| short |],              [t| C.CShort |])
               , ([cty| unsigned short |],     [t| C.CUShort |])
               , ([cty| int |],                [t| C.CInt |])
               , ([cty| unsigned int |],       [t| C.CUInt |])
               , ([cty| long |],               [t| C.CLong |])
               , ([cty| unsigned long |],      [t| C.CULong |])
               , ([cty| long long |],          [t| C.CLLong |])
               , ([cty| unsigned long long |], [t| C.CULLong |])
               ]

cFloatingMap = Map.fromList
               [ ([cty| float |] , [t| C.CFloat |])
               , ([cty| double |], [t| C.CDouble |])
               ]