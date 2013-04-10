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
  objc_import, objc, objc_emit
) where

  -- common libraries
import Control.Applicative
import Control.Monad
import Data.Array
import Data.Dynamic
import Data.IORef
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


-- FIXME: Can we use a foreign export combined with a foreign import to tie the knot between using and
--        filling the 'objc_jumptable'.

-- Specify imported Objective-C files. Needs to be spliced where an import declaration can appear.
--
-- FIXME: TODO; need to use TH.addDependentFile on each of the imported ObjC files
--
objc_import :: [FilePath] -> Q [TH.Dec]
objc_import headers
  = do
    { mapM_ stashHeader headers
    ; objc_jumptable <- newName "objc_jumptable"
    ; modifyState (\s -> s {foreignTable = varE objc_jumptable})
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


-- Inline Objective-C.
--
objc :: [TH.Name] -> TH.Name -> QC.Exp -> Q TH.Exp
objc vars resTy e
  = do
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
    
    ; cwrapperName <- newName "cwrapper"
      -- FIXME: should we produce a .h as well??
    ; stashHS $ 
        forImpD CCall Safe (show cwrapperName) cwrapperName hsWrapperTy
    ; idx <- extendJumpTable cwrapperName
    ; let resultName = mkName "result"
    ; stashObjC $ [cedecl|
                    $ty:cBridgeResTy $id:(show cwrapperName) ($params:(cParams vars cBridgeArgTys))
                    {
                      $ty:cResTy $id:(show resultName) = $exp:e;
                      return $exp:(cResMarshaller resultName);
                    }
                  |]
                  -- FIXME: we need to specify somwhere that NSString needs to be available

        -- Generate invocation of the C wrapper sandwiched into Haskell-side marshalling
    ; invoke [hsArgMarshaller (varE var) | (var, hsArgMarshaller) <- zip vars hsArgMarshallers]
             (callThroughTable idx)
             [| \call -> do { cresult <- call; $(hsResMarshaller [|cresult|] [|return|]) } |]
    }
  where
    callThroughTable idx
      = do { jumptable <- readState foreignTable
           ; [|fromDyn 
                 ((unsafePerformIO $ readIORef $jumptable) ! $(TH.lift idx))
                 (error "InlineObjC: INTERNAL ERROR: type mismatch in jumptable")
               :: CString -> IO CString|]
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
    cParams (var:vars) (argTy:argTys) = [cparam| $ty:argTy $id:(showOccName var) |] : cParams vars argTys

    unzip4 []                   = ([], [], [], [])
    unzip4 ((a, b, c, d):abcds) = let (as, bs, cs, ds) = unzip4 abcds in (a:as, b:bs, c:cs, d:ds)
    
-- FIXME: The following six functions need to go into a support module. The type mapping functions are language dependent
--   (i.e., would be different for plain C and Objective-C).

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

-- Determine the C type that we map a given Haskell type to.
--
haskellTypeToCType :: QC.Extensions -> TH.Type -> Q QC.Type
haskellTypeToCType lang (ListT `AppT` (ConT char))
  | char == ''Char 
  = haskellTypeNameToCType lang ''String
haskellTypeToCType lang (ConT tc `AppT` _)
  = haskellTypeNameToCType lang tc
haskellTypeToCType lang ty
  = reportErrorAndFail lang $ "don't know a foreign type suitable for Haskell type '" ++ show ty ++ "'"

-- Determine the C type that we map a given Haskell type constructor to — i.e., we map all Haskell
-- whose outermost constructor is the given type constructor to the returned C type..
--
haskellTypeNameToCType :: QC.Extensions -> TH.Name -> Q QC.Type
haskellTypeNameToCType ObjC tyname
  | tyname == ''String = return [cty| typename NSString * |]
  | tyname == ''()     = return [cty| void |]
haskellTypeNameToCType _lang tyname
  = reportErrorAndFail ObjC $ "don't know a foreign type suitable for Haskell type name '" ++ show tyname ++ "'"

-- Constructs Haskell code to marshall a value (used to marhsall arguments and results).
--
-- * The first argument is the code referring to the value to be marshalled.
-- * The second argument is the continuation that gets the marshalled value as an argument.
--
type HaskellMarshaller = TH.ExpQ -> TH.ExpQ -> TH.ExpQ

-- Constructs C code to marhsall an argument (used to marshall arguments and results).
--
-- * The argument is the identifier of the value to be marshalled.
-- * The result of the generated expression is the marshalled value.
--
type CMarshaller = TH.Name -> QC.Exp
  
generateHaskellToCMarshaller :: TH.Type -> QC.Type -> Q (TH.TypeQ, QC.Type, HaskellMarshaller, CMarshaller)
generateHaskellToCMarshaller hsTy cTy
  | cTy == [cty| typename NSString |] 
  = return ( [t| C.CString |]
           , [cty| char * |]
           , \val cont -> [| C.withCString $val $cont |]
           , \argName -> [cexp| [NSString stringWithUTF8String: $id:(show argName)] |]
           )
  | otherwise
  = reportErrorAndFail ObjC $ "cannot marshall '" ++ show hsTy ++ "' to '" ++ show cTy ++ "'"

generateCToHaskellMarshaller :: TH.Name -> QC.Type -> Q (TH.TypeQ, QC.Type, HaskellMarshaller, CMarshaller)
generateCToHaskellMarshaller hsTyName cTy
  | cTy == [cty| typename NSString |]
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
           , \_ _ -> [| id |]
           , \argName -> [cexp| $id:(show argName) |]
           )
  | otherwise
  = reportErrorAndFail ObjC $ "cannot marshall '" ++ show cTy ++ "' to '" ++ show hsTyName ++ "'"    

-- Emit the Objective-C file and return the foreign declarations. Needs to be spliced below the last
-- use of 'objc'.
--
-- FIXME: TODO
--
objc_emit :: Q [TH.Dec]
objc_emit
  = do
    { loc <- location
    ; let objcFname = dropExtension (loc_filename loc) ++ "_objc" `addExtension` "m"
    ; headers <- getHeaders
    ; objc    <- getHoistedObjC
    ; runIO $
        do
        { writeFile  objcFname (unlines $ map mkImport headers)
        ; appendFile objcFname (show $ QC.ppr objc)
        }
    ; objc_jumptable <- readState foreignTable
    ; labels         <- readState foreignLabels
    ; initialize     <-  [d|objc_initialise :: IO ()
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


-- Internal
-- --------

data State 
  = State
    { foreignTable  :: Q TH.Exp         -- table of foreign labels
    , foreignLabels :: [Name]              -- list of foreign imported names to populate 'foreignTable'
    , headers       :: [String]            -- imported Objective-C headers
    , hoistedObjC   :: [QC.Definition]     -- Objective-C that goes into the .m
    , hoistedHS     :: [TH.Dec]            -- Haskell that goes at the end of the module
    }
    -- FIXME: Using 'Dynamic' is good for debugging, but for production, 'unsafeCoerce' would be faster.

state :: IORef State
{-# NOINLINE state #-}
state = unsafePerformIO $ 
          newIORef $ 
            State
            { foreignTable  = error "InlineObjC: internal error: 'foreignTable' undefined"
            , foreignLabels = []
            , headers       = []
            , hoistedObjC   = []
            , hoistedHS     = []
            }

readState :: (State -> a) -> Q a
readState read = runIO $ read <$> readIORef state

modifyState :: (State -> State) -> Q ()
modifyState modify = runIO $ modifyIORef state modify
  -- atomic???

stashHeader :: String -> Q ()
stashHeader header = modifyState (\s -> s {headers = header : headers s})

stashObjC :: QC.Definition -> Q ()
stashObjC def = modifyState (\s -> s {hoistedObjC = def : hoistedObjC s})

stashHS :: TH.DecQ -> Q ()
stashHS decQ 
  = do
    { dec <- decQ
    ; modifyState (\s -> s {hoistedHS = dec : hoistedHS s})
    }

extendJumpTable :: Name -> Q Int
extendJumpTable foreignName
  = do
    { modifyState (\s -> s {foreignLabels = foreignLabels s ++ [foreignName]})   -- FIXME: *urgh*
    ; length <$> readState foreignLabels
    }

getHeaders :: Q [String]
getHeaders = reverse <$> readState headers

getHoistedObjC :: Q [QC.Definition]
getHoistedObjC = readState hoistedObjC

getHoistedHS :: Q [TH.Dec]
getHoistedHS = readState hoistedHS

reportErrorAndFail :: QC.Extensions -> String -> Q a
reportErrorAndFail lang msg
  = do
    { reportError lang msg
    ; fail "Failure"
    }

-- reportErrorAndBail :: String -> Q TH.Exp
-- reportErrorAndBail msg
--   = do
--     { reportError msg
--     ; Just undefinedName <- TH.lookupValueName "Prelude.undefined"
--     ; return $ VarE undefinedName
--     }

reportError :: QC.Extensions -> String -> Q ()
reportError lang msg
  = do
    { loc <- location
    -- FIXME: define a Show instance for 'Loc' and use it to prefix position to error
    ; TH.report True $ "inline " ++ showLang lang ++ ": " ++ msg 
    -- ; TH.reportError msg -- reqs template-haskell 2.8.0.0
    }
  where
    showLang QC.Antiquotation = "C"
    showLang QC.Gcc           = "GCC C"
    showLang QC.CUDA          = "CUDA C"
    showLang QC.OpenCL        = "OpenCL"
    showLang QC.ObjC          = "Objective-C"

showOccName :: Name -> String
showOccName (Name occ _) = occString occ
