{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- |
-- Module      : Language.C.Inline.C
-- Copyright   : [2013] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module exports the principal API for inline C.

module Language.C.Inline.C (

  -- * Re-export types from 'Foreign.C'
  module Foreign.C.Types, CString, CStringLen, CWString, CWStringLen, Errno, ForeignPtr, castForeignPtr,

  -- * Re-export types from Template Haskell
  Name,

  -- * Combinators for inline Objective-C
  c_import, c_interface, c_implementation, c_typecheck, c, c_emit,

  -- * Marshalling annotations
  Annotated(..), (<:), void, Class(..), IsType,

) where

  -- common libraries
import Control.Applicative
import Control.Monad              hiding (void)
import Data.Array
import Data.Char
import Data.Dynamic
import Data.IORef
import Data.List
import Data.Maybe
import Foreign.C                  as C
import Foreign.C.String           as C
import Foreign.C.Types
import Foreign.ForeignPtr         as C
import Foreign.Marshal            as C  hiding (void)
import Language.Haskell.TH        as TH
import Language.Haskell.TH.Syntax as TH
import System.FilePath
import System.IO.Unsafe                 (unsafePerformIO)

  -- quasi-quotation libraries
import Language.C.Quote           as QC
import Language.C.Quote.C         as QC
import Text.PrettyPrint.Mainland  as QC

  -- friends
import Language.C.Inline.Error
import Language.C.Inline.Hint
import Language.C.Inline.State
import Language.C.Inline.TH
import Language.C.Inline.C.Hint
import Language.C.Inline.C.Marshal


-- |Specify imported Objective-C files. Needs to be spliced where an import declaration can appear. (Just put it
-- straight after all the import statements in the module.)
--
-- FIXME: need to use TH.addDependentFile on each of the imported ObjC files & read headers
--
c_import :: [FilePath] -> Q [TH.Dec]
c_import headers
  = do
    { mapM_ stashHeader headers
    ; c_jumptable <- newName "c_jumptable"
    ; setForeignTable $ varE c_jumptable
    ; sequence $ [ sigD c_jumptable [t|IORef (Array Int Dynamic)|]
                 , pragInlD c_jumptable NoInline FunLike AllPhases    -- reqs template-haskell 2.8.0.0
                 -- , pragInlD c_jumptable (inlineSpecNoPhase False False)
                 , valD (varP c_jumptable) (normalB [|unsafePerformIO $ newIORef (array (0, 0) [])|]) []
                 ]
    -- ; return $ [d|import Language.C.Quote      as ObjC;
    --               import Language.C.Quote.ObjC as ObjC;
    --               import Foreign.C             as C
    --            |]
    }
    -- FIXME: Should this also add the Language.C.Quote imports? (We might not need to generate any imports at all?!?)

-- |Inline Objective-C top-level definitions for a header file ('.h').
--
c_interface :: [QC.Definition] -> Q [TH.Dec]
c_interface defs
  = do
    { stashObjC_h defs
    ; return []
    }

-- |Inline C top-level definitions for an implementation file ('.c').
--
-- The top-level Haskell variables given in the first argument will be foreign exported to be accessed from the
-- generated Objective-C code. In C, these Haskell variables will always be represented as functions. (In particular, if
-- the Haskell variable refers to a CAF, it will be a nullary function in C — after all, a thunk may still need to be
-- evaluated.)
--
c_implementation :: [Annotated TH.Name] -> [QC.Definition] -> Q [TH.Dec]
c_implementation ann_vars defs
  = do
    { mapM_ exportVar ann_vars
    ; stashObjC_m defs
    ; return []
    }
  where
    exportVar ann_var
      = do
        {   -- Determine the argument and result types of the exported Haskell function
        ; let var = stripAnnotation ann_var
        ; (tvs, argTys, inIO, resTy) <- splitHaskellType <$> haskellTypeOf ann_var

            -- Determine C types
        ; maybe_cArgTys <- mapM (haskellTypeToCType C11) argTys
        ; maybe_cResTy  <- haskellTypeToCType C11 resTy
        ; let cannotMapAllTypes = Nothing `elem` (maybe_cResTy : maybe_cArgTys)
              cArgTys           = map maybeErrorCtype maybe_cArgTys
              cResTy            = maybeErrorCtype maybe_cResTy

        ; if cannotMapAllTypes
          then do {str <- annotatedShowQ ann_var; reportErrorWithLang C11 $ "invalid marshalling: " ++ str}
          else do

        {   -- Determine the bridging type and the marshalling code
        ; (bridgeArgTys, cBridgeArgTys, hsArgMarshallers, cArgMarshallers) <-
            unzip4 <$> zipWithM generateCToHaskellMarshaller argTys cArgTys
        ; (bridgeResTy,  cBridgeResTy,  hsResMarshaller,  cResMarshaller)  <- generateHaskellToCMarshaller resTy cResTy

            -- Haskell type of the foreign wrapper function
        ; let hsWrapperTy = haskellWrapperType tvs bridgeArgTys bridgeResTy

            -- Generate the Haskell wrapper
        ; let cwrapperName = mkName . nameBase $ var
        ; hswrapperName <- newName (nameBase var ++ "_hswrapper")
        ; hsArgVars     <- mapM (const $ newName "arg") bridgeArgTys
        ; stashHS
            [ forExpD CCall (show hswrapperName) hswrapperName hsWrapperTy
            , sigD hswrapperName hsWrapperTy
            , funD hswrapperName
                [ clause (map varP hsArgVars)
                         (normalB $ generateHSCall hsArgVars hsArgMarshallers (varE var) hsResMarshaller inIO)
                         []
                ]
            ]

            -- Generate the C wrapper code (both prototype and definition)
        ; cArgVars <- mapM (\n -> newName $ "arg" ++ show n) [1..length cBridgeArgTys]
        ; let cArgVarExps = [ [cexp| $id:(nameBase var) |] | var <- cArgVars]
              call        = [cexp| $id:(show hswrapperName) ( $args:cArgVarExps ) |]
              (_wrapperProto, wrapperDef)
                       = generateCWrapper cwrapperName cBridgeArgTys cArgVars cArgMarshallers cArgTys cArgVars
                                   call
                                   resTy cBridgeResTy cResMarshaller cResTy
        ; stashObjC_m $
              -- C prototype of the foreign exported Haskell-side wrapper
            [cunit|
              $ty:cBridgeResTy $id:(show hswrapperName) ($params:(cParams cBridgeArgTys cArgVars));
            |]
            ++
            map makeStaticFunc wrapperDef
        } }

    splitHaskellType (ForallT tvs _ctxt ty)                   -- collect quantified variables (drop the context)
      = let (tvs', args, inIO, res) = splitHaskellType ty
        in
        (tvs ++ tvs', args, inIO, res)
    splitHaskellType (ArrowT `AppT` arg `AppT` res)           -- collect argument types
      = let (tvs, args, inIO, res') = splitHaskellType res
        in
        (tvs, arg:args, inIO, res')
    splitHaskellType (ConT io `AppT` res) | io == ''IO        -- is it an 'IO' function?
      = ([], [], True, res)
    splitHaskellType res
      = ([], [], False, res)

    makeStaticFunc (FuncDef (Func    dspec f decl ps    body loc1) loc2)
      = FuncDef (Func    (addStatic dspec) f decl ps    body loc1) loc2
    makeStaticFunc (FuncDef (OldFunc dspec f decl ps ig body loc1) loc2)
      = FuncDef (OldFunc (addStatic dspec) f decl ps ig body loc1) loc2
    makeStaticFunc def = def

    addStatic (DeclSpec         st tqs ts loc) = DeclSpec         (Tstatic loc:st) tqs ts loc
    addStatic (AntiTypeDeclSpec st tqs ts loc) = AntiTypeDeclSpec (Tstatic loc:st) tqs ts loc
    addStatic declSpec                         = declSpec

maybeErrorCtype :: Maybe QC.Type -> QC.Type
maybeErrorCtype Nothing   = [cty| typename __UNDEFINED_TYPE |]    -- placeholder to make progress in the face of errors
maybeErrorCtype (Just ty) = ty

forExpD :: Callconv -> String -> Name -> TypeQ -> DecQ
forExpD cc str n ty
  = do
    { ty' <- ty
    ; return $ ForeignD (ExportF cc str n ty')
    }


-- |Inline C expression.
--
-- The inline expression will be wrapped in a C function whose arguments are marshalled versions of the Haskell
-- variables given in the first argument. The marshalling of the variables and of the result is determined by the
-- marshalling annotations at the variables and the inline expression.
--
c :: [Annotated TH.Name] -> Annotated QC.Exp -> Q TH.Exp
c ann_vars ann_e
  = {- tryWithPlaceholder $ -} do  -- FIXME: catching the 'fail' purges all reported errors :(
    {   -- Sanity check of arguments
    ; let vars = map stripAnnotation ann_vars
    ; varTys <- mapM haskellTypeOf ann_vars
    ; resTy  <- haskellTypeOf ann_e

        -- Determine C types
    ; maybe_cArgTys <- mapM annotatedHaskellTypeToCType ann_vars
    ; maybe_cResTy  <- annotatedHaskellTypeToCType ann_e
    ; let cannotMapAllTypes = Nothing `elem` (maybe_cResTy : maybe_cArgTys)
          cArgTys           = map maybeErrorCtype maybe_cArgTys
          cResTy            = maybeErrorCtype maybe_cResTy

    ; if cannotMapAllTypes
      then failOn [ann_var | (ann_var, Nothing) <- zip ann_vars maybe_cArgTys] maybe_cResTy
      else do

    {   -- Determine the bridging type and the marshalling code
    ; (bridgeArgTys, cBridgeArgTys, hsArgMarshallers, cArgMarshallers) <-
        unzip4 <$> zipWithM generateHaskellToCMarshaller varTys cArgTys
    ; (bridgeResTy,  cBridgeResTy,  hsResMarshaller,  cResMarshaller)  <-
        generateCToHaskellMarshaller resTy cResTy

        -- Haskell type of the foreign wrapper function
    ; let hsWrapperTy = haskellWrapperType [] bridgeArgTys bridgeResTy

        -- FFI setup for the C wrapper
    ; cwrapperName <- show <$> newName "cwrapper" >>= newName   -- Don't ask...
    ; stashHS
        [ forImpD CCall Safe (show cwrapperName) cwrapperName hsWrapperTy
        ]
    ; idx <- extendJumpTable cwrapperName

        -- Generate the C wrapper code (both prototype and definition)
    ; cArgVars <- mapM (newName . nameBase) vars
    ; let (wrapperProto, wrapperDef)
            = generateCWrapper cwrapperName cArgTys vars cArgMarshallers cBridgeArgTys cArgVars
                               (stripAnnotation ann_e)
                               resTy cResTy cResMarshaller cBridgeResTy
    ; stashObjC_h wrapperProto
    ; stashObjC_m wrapperDef

        -- Generate invocation of the C wrapper sandwiched into Haskell-side marshalling
    ; generateHSCall vars hsArgMarshallers (callThroughTable idx hsWrapperTy) hsResMarshaller True
    } }
  where
    callThroughTable idx ty
      = do { jumptable <- getForeignTable
           ; [|fromDyn
                 ((unsafePerformIO $ readIORef $jumptable) ! $(TH.lift idx))
                 (error "InlineC: INTERNAL ERROR: type mismatch in jumptable")
               :: $ty |]
           }

    failOn err_ann_vars maybe_cResTy
      = do
        { unless (null err_ann_vars) $ do
            { var_strs <- mapM annotatedShowQ err_ann_vars
            ; reportErrorWithLang C11 $ "invalid marshalling: " ++ intercalate ", " var_strs
            }
        ; unless (isJust maybe_cResTy) $ do
            { ty <- haskellTypeOf ann_e
            ; reportErrorWithLang C11 $ "invalid marshalling for result type " ++ show ty
            }
        ; [| error "error in inline Objective-C expression" |]
        }

    annotatedHaskellTypeToCType ann_e
      = do
        { maybe_cType <- foreignTypeOf ann_e
        ; case maybe_cType of
            Nothing       -> haskellTypeOf ann_e >>= haskellTypeToCType C11
            Just cType -> return $ Just cType
        }

-- Turn a list of argument types and a result type into a Haskell wrapper signature.
--
-- > haskellWrapperType [tv1, .., tvm] [a1, .., an] r = [| forall tv1 .. tvm. a1 -> .. -> an -> IO r |]
--
haskellWrapperType :: [TH.TyVarBndr] -> [TH.TypeQ] -> TH.TypeQ -> TH.TypeQ
haskellWrapperType []  argTys resTy = wrapperBodyType argTys resTy                          -- monotype
haskellWrapperType tvs argTys resTy = forallT tvs (cxt []) (wrapperBodyType argTys resTy)   -- polytype

wrapperBodyType :: [TH.TypeQ] -> TH.TypeQ -> TH.TypeQ
wrapperBodyType []             resTy = [t| IO $resTy |]
wrapperBodyType (argTy:argTys) resTy = [t| $argTy -> $(wrapperBodyType argTys resTy) |]

-- Generate the prototype of and function definition of a C marshalling wrapper.
--
-- Given a C expression to be executed, this generator produces a C function that executes the expression with all
-- arguments and the result marshalled using the provided marshallers.
--
generateCWrapper :: TH.Name
                 -> [QC.Type]
                 -> [TH.Name]       -- name of arguments after marshalling (will be the original name without unique)
                 -> [CMarshaller]
                 -> [QC.Type]
                 -> [TH.Name]
                 -> QC.Exp          -- C expression containing occurences of the arguments (using names without uniques)
                 -> TH.Type
                 -> QC.Type
                 -> CMarshaller
                 -> QC.Type
                 -> ([QC.Definition], [QC.Definition])
generateCWrapper cwrapperName argTys vars argMarshallers cWrapperArgTys argVars e hsResTy resTy resMarshaller cWrapperResTy
  = let cMarshalling = [ [citem| $ty:argTy $id:(nameBase var) = $exp:(argMarshaller argVar); |]
                       | (argTy, var, argMarshaller, argVar) <- zip4 argTys vars argMarshallers argVars]
        resultName  = mkName "result"
        cInvocation | hsResTy == (ConT ''()) = [citem| $exp:e; |]                            -- void result
                    | otherwise              = [citem| {
                                                 $ty:resTy $id:(show resultName) = $exp:e;   // non-void result...
                                                 return $exp:(resMarshaller resultName);     // ...marshalled to Haskell
                                               }|]
    in
    ([cunit|
       $ty:cWrapperResTy $id:(show cwrapperName) ($params:(cParams cWrapperArgTys argVars));
     |],
     [cunit|
       $ty:cWrapperResTy $id:(show cwrapperName) ($params:(cParams cWrapperArgTys argVars))
       {
         $items:cMarshalling
         $item:cInvocation
       }
     |])

-- cParams [a1, .., an] [v1, .., vn] = [[cparam| a1 v1 |], .., [cparam| an vn |]]
--
-- * If the list is empty, we will return a singleton 'void' parameter.
--
cParams :: [QC.Type] -> [TH.Name] -> [QC.Param]
cParams []  []    = [ [cparam| void |] ]
cParams tys names = cParams' tys names
  where
    cParams' [] []                     = []
    cParams' (argTy:argTys) (var:vars) = [cparam| $ty:argTy $id:(show var) |] : cParams' argTys vars


-- Produce a Haskell expression that calls a function with all arguments and the result marshalled with the supplied
-- marshallers.
--
generateHSCall :: [TH.Name]
               -> [HaskellMarshaller]
               -> TH.ExpQ
               -> HaskellMarshaller
               -> Bool
               -> TH.ExpQ
generateHSCall vars hsArgMarshallers f hsResMarshaller inIO
  = invoke [hsArgMarshaller (varE var) | (var, hsArgMarshaller) <- zip vars hsArgMarshallers]
           f
           (if inIO then [| \call -> do {      cresult <- call ; $(hsResMarshaller [|cresult|] [|return|]) } |]
                    else [| \call -> do { let {cresult =  call}; $(hsResMarshaller [|cresult|] [|return|]) } |])
  where
      -- invoke [v1, .., vn] [a1, .., an] call r = [| a1 (\v1 -> .. -> an (\vn -> r (call v1 .. vn))..) |]
    invoke :: [TH.ExpQ -> TH.ExpQ] -> TH.ExpQ -> TH.ExpQ -> TH.ExpQ
    invoke []         call ret = [| $ret $call |]
    invoke (arg:args) call ret = arg [| \name -> $(invoke args [| $call name |] ret)|]

-- |Emit the C file and return the foreign declarations. Needs to be the last use of an 'c...' function.
-- (Just put it at the end of the Haskell module.)
--
c_emit :: Q [TH.Dec]
c_emit
  = do
    { loc <- location
    ; let origFname   = loc_filename loc
          cFname   = dropExtension origFname ++ "_c"
          cFname_h = cFname `addExtension` "h"
          cFname_m = cFname `addExtension` "c"
    ; headers          <- getHeaders
    ; (c_h, c_m) <- getHoistedObjC
    ; runIO $
        do
        { writeFile  cFname_h (info origFname)
        ; appendFile cFname_h (unlines (map mkImport headers) ++ "\n")
        ; appendFile cFname_h (show $ QC.ppr c_h)
        ; writeFile  cFname_m (info origFname)
        ; appendFile cFname_m ("#include \"HsFFI.h\"\n")
        ; appendFile cFname_m ("#include \"" ++ takeFileName cFname_h ++ "\"\n\n")
        ; appendFile cFname_m (show $ QC.ppr c_m)
        }
    ; c_jumptable <- getForeignTable
    ; labels         <- getForeignLabels
    ; initialize     <- [d|c_initialise :: IO ()
                           c_initialise
                            = -- unsafePerformIO $
                                writeIORef $c_jumptable $
                                  listArray ($(lift (1::Int)), $(lift $ length labels)) $
                                    $(listE [ [|toDyn $(varE label)|] | label <- labels])
                        |]
    ; (initialize ++) <$> getHoistedHS
    }
  where
    mkImport h@('<':_) = "#include " ++ h ++ ""
    mkImport h         = "#include \"" ++ h ++ "\""

    info fname = "// Generated code: DO NOT EDIT\n\
                 \//   generated from '" ++ fname ++ "'\n\
                 \//   by package 'language-c-inline'\n\n"

-- |Force type checking of all declaration appearing earlier in this module.
--
-- Template Haskell performs type checking on declaration groups seperated by toplevel splices. In order for a type
-- declaration to be available to an Objective-C inline directive, the type declaration must be in an earlier
-- declaration group than the Objective-C inline directive. A toplevel Objective-C inline directive always is the start
-- of a new declaration group; hence, it can be considered to be implicitly preceded by an 'c_typecheck'.
--
c_typecheck :: Q [TH.Dec]
c_typecheck = return []
