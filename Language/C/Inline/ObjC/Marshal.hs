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
  -- * Determine corresponding foreign types of Haskell types
  haskellTypeToCType,
  
  -- * Marshaller types
  HaskellMarshaller, CMarshaller,
  
  -- * Compute bridging types and marshallers
  generateHaskellToCMarshaller, generateCToHaskellMarshaller
) where

  -- common libraries
import Data.Map                   as Map
import Data.Maybe
import Data.Word
import Foreign.C                  as C
import Foreign.C.String           as C
import Foreign.Marshal            as C
import Foreign.Ptr                as C
import Foreign.ForeignPtr         as C
import Foreign.StablePtr          as C
import Language.Haskell.TH        as TH
import Language.Haskell.TH.Syntax as TH

  -- quasi-quotation libraries
import Language.C.Quote           as QC
import Language.C.Quote.ObjC      as QC
import Text.PrettyPrint.Mainland  as QC

  -- friends
import Language.C.Inline.Error
import Language.C.Inline.State
import Language.C.Inline.TH


-- Determine foreign types
-- -----------------------

-- |Determine the C type that we map a given Haskell type to.
--
haskellTypeToCType :: QC.Extensions -> TH.Type -> Q (Maybe QC.Type)
haskellTypeToCType lang (ForallT _tvs _ctxt ty)                -- ignore quantifiers and contexts
  = haskellTypeToCType lang ty
haskellTypeToCType lang ty                              
  = do
    { maybe_marshaller <- lookupMarshaller ty
    ; case maybe_marshaller of
        Just (_, _, cTy, _, _) -> return $ Just cTy            -- use a custom marshaller if one is available for this type
        Nothing                -> haskellTypeToCType' lang ty  -- otherwise, continue below...
    }
  where
    haskellTypeToCType' lang (ListT `AppT` (ConT char))        -- marshal '[Char]' as 'String'
      | char == ''Char 
      = haskellTypeNameToCType lang ''String
    haskellTypeToCType' lang ty@(ConT maybeC `AppT` argTy)     -- encode a 'Maybe' around a pointer type in the pointer
      | maybeC == ''Maybe
      = do
        { cargTy <- haskellTypeToCType lang argTy
        ; if fmap isCPtrType cargTy == Just True
          then
            return cargTy
          else
            unknownType lang ty
        }
    haskellTypeToCType' lang ty@(ConT ptrC `AppT` argTy)       -- pass vanilla pointers through (as per FFI spec)
      | ptrC == ''Ptr
      = return $ Just [cty| void* |]
      | ptrC == ''FunPtr
      = return $ Just [cty| void*(void) |]
      | ptrC == ''StablePtr
      = return $ Just [cty| void*(void) |]
    haskellTypeToCType' lang (ConT tc)                         -- nullary type constructors are delegated
      = haskellTypeNameToCType lang tc
    haskellTypeToCType' lang ty@(VarT tv)                      -- can't marshal an unknown type
      = unknownType lang ty
    haskellTypeToCType' lang ty@(UnboxedTupleT _)              -- there is nothing like unboxed tuples in C
      = unknownType lang ty
    haskellTypeToCType' _lang ty                               -- everything else is marshalled as a stable pointer
      = return $ Just [cty| typename HsStablePtr |]

    unknownType lang ty 
      = do
        { reportErrorWithLang lang $ "don't know a foreign type suitable for Haskell type '" ++ TH.pprint ty ++ "'"
        ; return Nothing
        }

-- |Determine the C type that we map a given Haskell type constructor to — i.e., we map all Haskell types
-- whose outermost constructor is the given type constructor to the returned C type.
--
-- All types representing boxed values that are not explicitly mapped to a specific C type, are mapped to
-- stable pointers.
--
haskellTypeNameToCType :: QC.Extensions -> TH.Name -> Q (Maybe QC.Type)
haskellTypeNameToCType ext tyname
  = case Map.lookup tyname (haskellToCTypeMap ext) of
      Just cty -> return $ Just cty
      Nothing  -> do
        { info <- reify tyname
        ; case info of
            PrimTyConI _ _ True -> unknownUnboxedType
            _                   -> return $ Just [cty| typename HsStablePtr |]
        }
  where
    unknownUnboxedType = do
                         { reportErrorWithLang ext $ 
                             "don't know a foreign type suitable for the unboxed Haskell type '" ++ show tyname ++ "'"  
                         ; return Nothing
                         }

haskellToCTypeMap :: QC.Extensions -> Map TH.Name QC.Type
haskellToCTypeMap ObjC
  = Map.fromList
    [ (''CChar,   [cty| char |])
    , (''CSChar,  [cty| signed char |])
    , (''CUChar,  [cty| unsigned char |])
    , (''CShort,  [cty| short |])
    , (''CUShort, [cty| unsigned short |])
    , (''Int,     [cty| typename NSInteger |])
    , (''CInt,    [cty| int |])
    , (''Word,    [cty| typename NSUInteger |])
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
    , (''Bool,    [cty| typename BOOL |])
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
generateHaskellToCMarshaller hsTy cTy@(Type (DeclSpec _ _ (Tnamed (Id name _) _ _) _) (Ptr _ (DeclRoot _) _) _)
  | Just name == maybeHeadName                         -- wrapped ForeignPtr mapped to an Objective-C class
  = return ( ptrOfForeignPtrWrapper hsTy
           , cTy
           , \val cont -> [| C.withForeignPtr ($(unwrapForeignPtrWrapper hsTy) $val) $cont |]
           , \argName -> [cexp| $id:(show argName) |]
           )
  | otherwise
  = do
    { maybe_marshaller <- lookupMarshaller hsTy
    ; case maybe_marshaller of
        Just (_, classTy, cTy', haskellToC, _cToHaskell)
          | cTy' == cTy                                -- custom marshaller mapping to an Objective-C class
          -> return ( ptrOfForeignPtrWrapper classTy
                    , cTy
                    , \val cont -> [| do
                                      { nsClass <- $(varE haskellToC) $val
                                      ; C.withForeignPtr ($(unwrapForeignPtrWrapper classTy) nsClass) $cont
                                      } |]
                    , \argName -> [cexp| $id:(show argName) |]
                    )
        Nothing                                        -- other => continue below
          -> generateHaskellToCMarshaller' hsTy cTy
    }
  where
    maybeHeadName = fmap nameBase $ headTyConName hsTy
generateHaskellToCMarshaller hsTy cTy = generateHaskellToCMarshaller' hsTy cTy

generateHaskellToCMarshaller' :: TH.Type -> QC.Type -> Q (TH.TypeQ, QC.Type, HaskellMarshaller, CMarshaller)
generateHaskellToCMarshaller' hsTy@(ConT maybe `AppT` argTy) cTy
  | maybe == ''Maybe && isCPtrType cTy
  = do 
    { (argTy', cTy', hsMarsh, cMarsh) <- generateHaskellToCMarshaller argTy cTy
    ; ty <- argTy'
    ; resolve ty argTy' cTy' hsMarsh cMarsh
    }
  where
    resolve ty argTy' cTy' hsMarsh cMarsh
      = case ty of
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
          ConT con 
            -> do
               { info <- reify con
               ; case info of
                   TyConI (TySynD _name [] tysyn) -> resolve tysyn argTy' cTy' hsMarsh cMarsh
                                                       -- chase type synonyms (only nullary ones at the moment)
                   _ -> missingErr
               }
          _ -> missingErr
    missingErr = reportErrorAndFail ObjC $ 
                   "missing 'Maybe' marshalling for '" ++ prettyQC cTy ++ "' to '" ++ TH.pprint hsTy ++ "'"
generateHaskellToCMarshaller' hsTy@(ConT ptrC `AppT` argTy) cTy
  | ptrC == ''Ptr || ptrC == ''FunPtr || ptrC == ''StablePtr
  = return ( return hsTy
           , cTy
           , \val cont -> [| $cont $val |]
           , \argName -> [cexp| $id:(show argName) |]
           )
generateHaskellToCMarshaller' hsTy cTy
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
  | cTy == [cty| typename BOOL |] 
  = return ( [t| C.CSChar |]
           , cTy
           , \val cont -> [| $cont (C.fromBool $val) |]
           , \argName -> [cexp| ($id:(show argName)) |]
           )
  | cTy == [cty| typename NSString * |] 
  = return ( [t| C.CString |]
           , [cty| char * |]
           , \val cont -> [| C.withCString $val $cont |]
           , \argName -> [cexp| ($id:(show argName)) ? [NSString stringWithUTF8String: $id:(show argName)] : nil |]
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
generateCToHaskellMarshaller hsTy cTy@(Type (DeclSpec _ _ (Tnamed (Id name _) _ _) _) (Ptr _ (DeclRoot _) _) _)
  | Just name == maybeHeadName                         -- ForeignPtr mapped to an Objective-C class
  = return ( ptrOfForeignPtrWrapper hsTy
           , cTy
           , \val cont -> do { let datacon = foreignWrapperDatacon hsTy
                             ; [| do { fptr <- newForeignPtr_ $val; $cont ($datacon fptr) } |] 
                             }
           , \argName -> [cexp| $id:(show argName) |]
           )
  | otherwise
  = do
    { maybe_marshaller <- lookupMarshaller hsTy
    ; case maybe_marshaller of
        Just (_, classTy, cTy', _haskellToC, cToHaskell)
          | cTy' == cTy                                -- custom marshaller mapping to an Objective-C class
          -> return ( ptrOfForeignPtrWrapper classTy
                    , cTy
                    , \val cont -> do { let datacon = foreignWrapperDatacon classTy
                                      ; [| do 
                                           { fptr <- newForeignPtr_ $val
                                           ; hsVal <- $(varE cToHaskell) ($datacon fptr) 
                                           ; $cont hsVal
                                           } |] 
                                      }
                    , \argName -> [cexp| $id:(show argName) |]
                    )
        Nothing                                        -- other => continue below
          -> generateCToHaskellMarshaller' hsTy cTy
    }
  where
    maybeHeadName = fmap nameBase $ headTyConName hsTy
generateCToHaskellMarshaller hsTy cTy = generateCToHaskellMarshaller' hsTy cTy

generateCToHaskellMarshaller' :: TH.Type -> QC.Type -> Q (TH.TypeQ, QC.Type, HaskellMarshaller, CMarshaller)
generateCToHaskellMarshaller' hsTy@(ConT maybe `AppT` argTy) cTy
  | maybe == ''Maybe && isCPtrType cTy
  = do 
    { (argTy', cTy', hsMarsh, cMarsh) <- generateCToHaskellMarshaller argTy cTy
    ; ty <- argTy'
    ; resolve ty argTy' cTy' hsMarsh cMarsh
    }
  where
    resolve ty argTy' cTy' hsMarsh cMarsh
      = case ty of
          ConT ptr `AppT` _ 
            | ptr == ''C.Ptr       -> return ( argTy'
                                             , cTy'
                                             , \val cont -> [| if $val == C.nullPtr 
                                                               then $cont Nothing 
                                                               else $(hsMarsh val [| $cont . Just |]) |]
                                             , cMarsh
                                             )
            | ptr == ''C.StablePtr -> return ( argTy'
                                             , cTy'
                                             , \val cont -> [| if (C.castStablePtrToPtr $val) == C.nullPtr
                                                               then $cont Nothing 
                                                               else $(hsMarsh val [| $cont . Just |]) |]
                                                                 -- NB: the above cast works for GHC, but is in the grey area
                                                                 --     of the FFI spec
                                             , cMarsh
                                             )
          ConT con 
            -> do
               { info <- reify con
               ; case info of
                   TyConI (TySynD _name [] tysyn) -> resolve tysyn argTy' cTy' hsMarsh cMarsh
                                                       -- chase type synonyms (only nullary ones at the moment)
                   _ -> missingErr
               }
          _ -> missingErr
    missingErr = reportErrorAndFail ObjC $ 
                   "missing 'Maybe' marshalling for '" ++ prettyQC cTy ++ "' to '" ++ TH.pprint hsTy ++ "'"
generateCToHaskellMarshaller' hsTy@(ConT ptrC `AppT` argTy) cTy
  | ptrC == ''Ptr || ptrC == ''FunPtr || ptrC == ''StablePtr
  = return ( return hsTy
           , cTy
           , \val cont -> [| $cont $val |]
           , \argName -> [cexp| $id:(show argName) |]
           )
generateCToHaskellMarshaller' hsTy cTy
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
  | cTy == [cty| typename BOOL |]
  = return ( [t| C.CSChar |]
           , cTy
           , \val cont -> [| $cont (C.toBool $val) |]
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
                 ( $id:arg )
                 ? ({ typename NSUInteger maxLen = [$id:arg maximumLengthOfBytesUsingEncoding:NSUTF8StringEncoding] + 1;
                     char *buffer = malloc (maxLen);
                     if (![$id:arg getCString:buffer maxLength:maxLen encoding:NSUTF8StringEncoding])
                       *buffer = '\0';
                     buffer;
                   })
                 : nil
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
               [ ([cty| char |],                [t| C.CChar |])
               , ([cty| signed char |],         [t| C.CChar |])
               , ([cty| unsigned char |],       [t| C.CUChar |])
               , ([cty| short |],               [t| C.CShort |])
               , ([cty| unsigned short |],      [t| C.CUShort |])
               , ([cty| int |],                 [t| C.CInt |])
               , ([cty| unsigned int |],        [t| C.CUInt |])
               , ([cty| long |],                [t| C.CLong |])
               , ([cty| unsigned long |],       [t| C.CULong |])
               , ([cty| long long |],           [t| C.CLLong |])
               , ([cty| unsigned long long |],  [t| C.CULLong |])
               , ([cty| typename NSInteger |],  [t| Int |])
               , ([cty| typename NSUInteger |], [t| Word |])
               ]

cFloatingMap = Map.fromList
               [ ([cty| float |] , [t| C.CFloat |])
               , ([cty| double |], [t| C.CDouble |])
               ]