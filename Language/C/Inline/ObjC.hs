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
                 -- , pragInlD objc_jumptable NoInline FunLike AllPhases    -- reqs template-haskell 2.8.0.0
                 , pragInlD objc_jumptable (inlineSpecNoPhase False False)
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
    { stashObjC_h =<< objcUnit [] defs
    ; return []
    }

-- |Inline Objective-C top-level definitions for an implementation file ('.m').
--
-- The top-level Haskell variables given in the first argument will be foreign exported to be accessed from the
-- generated Objective-C code.
--
objc_implementation :: [TH.Name] -> [QC.Definition] -> Q [TH.Dec]
objc_implementation vars defs
  = do
    { stashObjC_m =<< objcUnit vars defs
    ; return []
    }

-- Resolve free variables in a set of Objective-C top-level definitions.
--
objcUnit :: [TH.Name] -> [QC.Definition] -> Q [QC.Definition]
objcUnit vars defs
-- FIXME: vars!!!
  = return defs

-- |Inline Objective-C expression.
--
-- The inline expression will be wrapped in a C function whose arguments are marshalled versions of the Haskell
-- variables given in the first argument and whose return value will be marhsalled to the Haskell type given by the
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
    ; (bridgeResTy,  cBridgeResTy,  hsResMarshaller,  cResMarshaller)  <- generateCToHaskellMarshaller resTy cResTy
    
        -- Haskell type of the foreign wrapper function
    ; let hsWrapperTy = haskellWrapperType bridgeArgTys bridgeResTy

        -- FFI setup for the C wrapper    
    ; cwrapperName <- newName "cwrapper"
    ; stashHS $ 
        forImpD CCall Safe (show cwrapperName) cwrapperName hsWrapperTy
    ; idx <- extendJumpTable cwrapperName

        -- Generate the C wrapper code (both prototype and definition)
    ; cArgVars <- mapM (newName . nameBase) vars
    ; let cMarshalling = [ [citem| $ty:cArgTy $id:(nameBase var) = $exp:(cArgMarshaller cArgVar); |] 
                         | (cArgTy, var, cArgMarshaller, cArgVar) <- zip4 cArgTys vars cArgMarshallers cArgVars]
          resultName  = mkName "result"
          cInvocation | resTy == ''() = [citem| $exp:e; |]                                  -- void result
                      | otherwise     = [citem| {
                                          $ty:cResTy $id:(show resultName) = $exp:e;        // non-void result...
                                          return $exp:(cResMarshaller resultName);          // ...marshalled to Haskell
                                        }|]
    ; stashObjC_h $ [cunit|
                      $ty:cBridgeResTy $id:(show cwrapperName) ($params:(cParams cArgVars cBridgeArgTys));
                    |]
    ; stashObjC_m $ [cunit|
                      $ty:cBridgeResTy $id:(show cwrapperName) ($params:(cParams cArgVars cBridgeArgTys))
                      {
                        $items:cMarshalling
                        $item:cInvocation
                      }
                    |]
                  -- FIXME: we need to specify somwhere that NSString needs to be available

        -- Generate invocation of the C wrapper sandwiched into Haskell-side marshalling
    ; invoke [hsArgMarshaller (varE var) | (var, hsArgMarshaller) <- zip vars hsArgMarshallers]
             (callThroughTable idx hsWrapperTy)
             [| \call -> do { cresult <- call; $(hsResMarshaller [|cresult|] [|return|]) } |]
    }
  where
    callThroughTable idx ty
      = do { jumptable <- getForeignTable
           ; [|fromDyn 
                 ((unsafePerformIO $ readIORef $jumptable) ! $(TH.lift idx))
                 (error "InlineObjC: INTERNAL ERROR: type mismatch in jumptable")
               :: $ty |]
           }

      -- haskellWrapperType [a1, .., an] r = [| a1 -> .. -> an -> IO r |]
    haskellWrapperType :: [TH.TypeQ] -> TH.TypeQ -> TH.TypeQ
    haskellWrapperType []             resTy = [t| IO $resTy |]
    haskellWrapperType (argTy:argTys) resTy = [t| $argTy -> $(haskellWrapperType argTys resTy) |]

      -- invoke [v1, .., vn] [a1, .., an] call r = [| a1 (\v1 -> .. -> an (\vn -> r (call v1 .. vn))..) |]    
    invoke :: [TH.ExpQ -> TH.ExpQ] -> TH.ExpQ -> TH.ExpQ -> TH.ExpQ
    invoke []         call ret = [| $ret $call |]
    invoke (arg:args) call ret = arg [| \name -> $(invoke args [| $call name |] ret)|]
    
      -- cParams [v1, .., vn] [a1, .., an] = [[cparam| a1 v1 |], .., [cparam| an vn |]]
    cParams [] []                     = []
    cParams (var:vars) (argTy:argTys) = [cparam| $ty:argTy $id:(show var) |] : cParams vars argTys
    
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
