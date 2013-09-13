{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- |
-- Module      : Language.C.Inline.ObjC
-- Copyright   : [2013] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module exports the principal API for inline Objective-C.

module Language.C.Inline.ObjC (
  objc_import, objc_interface, objc_implementation, objc, objc_emit
) where

  -- common libraries
import Control.Applicative
import Control.Monad
import Data.Array
import Data.Dynamic
import Data.IORef
import Data.List
import Foreign.C                  as C
import Foreign.C.String           as C
import Foreign.Marshal            as C
import Language.Haskell.TH        as TH
import Language.Haskell.TH.Syntax as TH
import System.FilePath
import System.IO.Unsafe                 (unsafePerformIO)

  -- quasi-quotation libraries
import Language.C.Quote           as QC
import Language.C.Quote.ObjC      as QC
import Text.PrettyPrint.Mainland  as QC

  -- friends
import Language.C.Inline.Error
import Language.C.Inline.State
import Language.C.Inline.ObjC.Marshal


-- |Specify imported Objective-C files. Needs to be spliced where an import declaration can appear. (Just put it
-- straight after all the import statements in the module.)
--
-- FIXME: need to use TH.addDependentFile on each of the imported ObjC files & read headers
--
objc_import :: [FilePath] -> Q [TH.Dec]
objc_import headers
  = do
    { mapM_ stashHeader headers
    ; objc_jumptable <- newName "objc_jumptable"
    ; setForeignTable $ varE objc_jumptable
    ; sequence $ [ sigD objc_jumptable [t|IORef (Array Int Dynamic)|]
                 , pragInlD objc_jumptable NoInline FunLike AllPhases    -- reqs template-haskell 2.8.0.0
                 -- , pragInlD objc_jumptable (inlineSpecNoPhase False False)
                 , valD (varP objc_jumptable) (normalB [|unsafePerformIO $ newIORef (array (0, 0) [])|]) []
                 ]
    -- ; return $ [d|import Language.C.Quote      as ObjC;
    --               import Language.C.Quote.ObjC as ObjC;
    --               import Foreign.C             as C
    --            |]
    }
    -- FIXME: Should this also add the Language.C.Quote imports? (We might not need to generate any imports at all?!?)

-- |Inline Objective-C top-level definitions for a header file ('.h').
--
objc_interface :: [QC.Definition] -> Q [TH.Dec]
objc_interface defs
  = do
    { stashObjC_h defs
    ; return []
    }

-- |Inline Objective-C top-level definitions for an implementation file ('.m').
--
-- The top-level Haskell variables given in the first argument will be foreign exported to be accessed from the
-- generated Objective-C code. In C, these Haskell variables will always be represented as functions. (In particular, if
-- the Haskell variable refers to a CAF, it will be a nullary function in C — after all, a thunk may still need to be
-- evaluated.)
--
objc_implementation :: [TH.Name] -> [QC.Definition] -> Q [TH.Dec]
objc_implementation vars defs
  = do
    { mapM_ exportVar vars
    ; stashObjC_m defs
    ; return []
    }
  where
    exportVar var
      = do
        {   -- Determine the argument and result types of the exported Haskell function
        ; (argTys, inIO, resTy) <- splitHaskellType <$> determineVarType var

            -- Determine C types
        ; cArgTys <- mapM (haskellTypeToCType ObjC) argTys
        ; cResTy  <- haskellTypeToCType ObjC resTy

            -- Determine the bridging type and the marshalling code
        ; (bridgeArgTys, cBridgeArgTys, hsArgMarshallers, cArgMarshallers) <-
            unzip4 <$> zipWithM generateCToHaskellMarshaller argTys cArgTys
        ; (bridgeResTy,  cBridgeResTy,  hsResMarshaller,  cResMarshaller)  <- generateHaskellToCMarshaller resTy cResTy

            -- Haskell type of the foreign wrapper function
        ; let hsWrapperTy = haskellWrapperType bridgeArgTys bridgeResTy

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
            wrapperDef
        }

    splitHaskellType (ArrowT `AppT` arg `AppT` res)
      = let (args, inIO, res') = splitHaskellType res
        in
        (arg:args, inIO, res')
    splitHaskellType (ConT io `AppT` res) | io == ''IO
      = ([], True, res)
    splitHaskellType res
      = ([], False, res)

forExpD :: Callconv -> String -> Name -> TypeQ -> DecQ
forExpD cc str n ty
 = do
   { ty' <- ty
   ; return $ ForeignD (ExportF cc str n ty')
   }

-- |Inline Objective-C expression.
--
-- The inline expression will be wrapped in a C function whose arguments are marshalled versions of the Haskell
-- variables given in the first argument and whose return value will be marshalled to the Haskell type given by the
-- second argument.
--
objc :: [TH.Name] -> TH.Name -> QC.Exp -> Q TH.Exp
objc vars resTy e
  = {- tryWithPlaceholder $ -} do  -- FIXME: catching the 'fail' purges all reported errors :(
    {   -- Sanity check of arguments
    ; varTys <- mapM determineVarType vars
    ; checkTypeName resTy

        -- Determine C types
    ; cArgTys <- mapM (haskellTypeToCType ObjC) varTys
    ; cResTy  <- haskellTypeNameToCType ObjC resTy

        -- Determine the bridging type and the marshalling code
    ; (bridgeArgTys, cBridgeArgTys, hsArgMarshallers, cArgMarshallers) <-
        unzip4 <$> zipWithM generateHaskellToCMarshaller varTys cArgTys
    ; (bridgeResTy,  cBridgeResTy,  hsResMarshaller,  cResMarshaller)  <-
        generateCToHaskellMarshaller (ConT resTy) cResTy

        -- Haskell type of the foreign wrapper function
    ; let hsWrapperTy = haskellWrapperType bridgeArgTys bridgeResTy

        -- FFI setup for the C wrapper
    ; cwrapperName <- newName "cwrapper"
    ; stashHS
        [ forImpD CCall Safe (show cwrapperName) cwrapperName hsWrapperTy
        ]
    ; idx <- extendJumpTable cwrapperName

        -- Generate the C wrapper code (both prototype and definition)
    ; cArgVars <- mapM (newName . nameBase) vars
    ; let (wrapperProto, wrapperDef)
            = generateCWrapper cwrapperName cArgTys vars cArgMarshallers cBridgeArgTys cArgVars
                               e
                               (ConT resTy) cResTy cResMarshaller cBridgeResTy
    ; stashObjC_h wrapperProto
    ; stashObjC_m wrapperDef

        -- Generate invocation of the C wrapper sandwiched into Haskell-side marshalling
    ; generateHSCall vars hsArgMarshallers (callThroughTable idx hsWrapperTy) hsResMarshaller True
    }
  where
    callThroughTable idx ty
      = do { jumptable <- getForeignTable
           ; [|fromDyn
                 ((unsafePerformIO $ readIORef $jumptable) ! $(TH.lift idx))
                 (error "InlineObjC: INTERNAL ERROR: type mismatch in jumptable")
               :: $ty |]
           }

-- Turn a list of argument types and a result type into a Haskell wrapper signature.
--
-- > haskellWrapperType [a1, .., an] r = [| a1 -> .. -> an -> IO r |]
--
haskellWrapperType :: [TH.TypeQ] -> TH.TypeQ -> TH.TypeQ
haskellWrapperType []             resTy = [t| IO $resTy |]
haskellWrapperType (argTy:argTys) resTy = [t| $argTy -> $(haskellWrapperType argTys resTy) |]

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
cParams :: [QC.Type] -> [TH.Name] -> [QC.Param]
cParams [] []                     = []
cParams (argTy:argTys) (var:vars) = [cparam| $ty:argTy $id:(show var) |] : cParams argTys vars

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

-- |Emit the Objective-C file and return the foreign declarations. Needs to be the last use of an 'objc...' function.
-- (Just put it at the end of the Haskell module.)
--
objc_emit :: Q [TH.Dec]
objc_emit
  = do
    { loc <- location
    ; let origFname   = loc_filename loc
          objcFname   = dropExtension origFname ++ "_objc"
          objcFname_h = objcFname `addExtension` "h"
          objcFname_m = objcFname `addExtension` "m"
    ; headers          <- getHeaders
    ; (objc_h, objc_m) <- getHoistedObjC
    ; runIO $
        do
        { writeFile  objcFname_h (info origFname)
        ; appendFile objcFname_h (unlines (map mkImport headers) ++ "\n")
        ; appendFile objcFname_h (show $ QC.ppr objc_h)
        ; writeFile  objcFname_m (info origFname)
        ; appendFile objcFname_m ("#import \"" ++ objcFname_h ++ "\"\n\n")
        ; appendFile objcFname_m (show $ QC.ppr objc_m)
        }
    ; objc_jumptable <- getForeignTable
    ; labels         <- getForeignLabels
    ; initialize     <- [d|objc_initialise :: IO ()
                           objc_initialise
                            = -- unsafePerformIO $
                                writeIORef $objc_jumptable $
                                  listArray ($(lift (1::Int)), $(lift $ length labels)) $
                                    $(listE [ [|toDyn $(varE label)|] | label <- labels])
                        |]
    ; (initialize ++) <$> getHoistedHS
    }
  where
    mkImport h@('<':_) = "#import " ++ h ++ ""
    mkImport h         = "#import \"" ++ h ++ "\""

    info fname = "// Generated code: DO NOT EDIT\n\
                 \//   generated from '" ++ fname ++ "'\n\
                 \//   by package 'language-c-inline'\n\n"
