{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module InlineObjC (objc_import, objc, objc_emit) where

import Language.C.Quote           as ObjC
import Language.C.Quote.ObjC      as ObjC
import Text.PrettyPrint.Mainland  as ObjC

import Data.Loc

import Control.Applicative
import Data.Array
import Data.Dynamic
import Data.IORef
import Foreign.C                  as C
import Foreign.Marshal            as C
import Language.Haskell.TH        as TH
import Language.Haskell.TH.Syntax as TH
import System.FilePath
import System.IO.Unsafe                 (unsafePerformIO)


-- FIXME: Can we use a foreign export combined with a foreign import to tie the knot between using and
--        filling the 'objc_jumptable'.

-- Specify imported Objective-C files. Needs to be spliced where an import declaration can appear.
--
-- FIXME: TODO; need to use TH.addDependentFile on each of the imported ObjC files
--
objc_import :: [FilePath] -> TH.Q [TH.Dec]
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
-- FIXME: Needs to be generalised to multiple free names. (Probably a list.)
--
objc :: TH.Name -> ObjC.Exp -> TH.Q TH.Exp
objc vname e
  = do
    { vinfo <- reify vname
    ; case vinfo of
        VarI _ ty _ _ -> 
          do
          { -- FIXME: * need to check the 'ty' to be marshalable
            --        * need to compute HS FFI C types and native C types as well as marshaling code
            --          (all hardcoded below for this specific example)
          ; cwrapperName <- newName "cwrapper"
            -- FIXME: should we produce a .h as well??
          ; stashHS $ 
              forImpD CCall Safe (show cwrapperName) cwrapperName [t|CString -> IO CString|]
          -- ; stashHS $ [d|foreign import ccall $(show cwrapperName) $cwrapperName :: CString -> IO CString|]
                                                                                    -- ^^^FIXME: hardcoded for now
          ; idx <- extendJumpTable cwrapperName
          ; stashObjC $ [cedecl|
                          char * $id:(show cwrapperName) (char *$id:(showOccName vname))
                          {
                            typename NSString *result = $exp:e;
                            char *buffer = malloc (65536);
                            if (![result getCString:buffer maxLength:65536 encoding:NSUTF8StringEncoding])
                              *buffer = '\0';
                            return buffer;
                          }
                        |]
                        -- FIXME: we need to specify somwhere that NSString needs to be available
          ; [| do 
               { cresult <- C.withCString $(varE vname) (\vnameC -> $(callThroughTable idx) vnameC)
               ; result  <- C.peekCString cresult
               ; C.free cresult
               ; return result
               } |]
          }
        nonVarInfo    -> 
          do
          { reportErrorAndBail $ 
              "first argument to 'objc' must be a variable name, but found " ++ 
              show (TH.ppr nonVarInfo)
          }
    }
  where
    callThroughTable idx
      = do { jumptable <- readState foreignTable
           ; [|fromDyn 
                 ((unsafePerformIO $ readIORef $jumptable) ! $(TH.lift idx))
                 (error "InlineObjC: internal error")
               :: CString -> IO CString|]
           }

-- Emit the Objective-C file and return the foreign declarations. Needs to be spliced below the last
-- use of 'objc'.
--
-- FIXME: TODO
--
objc_emit :: TH.Q [TH.Dec]
objc_emit
  = do
    { loc <- location
    ; let objcFname = dropExtension (loc_filename loc) ++ "_objc" `addExtension` "m"
    ; headers <- getHeaders
    ; objc    <- getHoistedObjC
    ; runIO $
        do
        { writeFile  objcFname (unlines $ map mkImport headers)
        ; appendFile objcFname (show $ ObjC.ppr objc)
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
    { foreignTable  :: TH.Q TH.Exp         -- table of foreign labels
    , foreignLabels :: [Name]              -- list of foreign imported names to populate 'foreignTable'
    , headers       :: [String]            -- imported Objective-C headers
    , hoistedObjC   :: [ObjC.Definition]   -- Objective-C that goes into the .m
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

stashObjC :: ObjC.Definition -> Q ()
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

getHoistedObjC :: Q [ObjC.Definition]
getHoistedObjC = readState hoistedObjC

getHoistedHS :: Q [TH.Dec]
getHoistedHS = readState hoistedHS

reportErrorAndBail :: String -> Q TH.Exp
reportErrorAndBail msg
  = do
    { loc <- location
    -- FIXME: define a Show instance for 'Loc' and use it to prefix position to error
    ; TH.report True msg 
    -- ; TH.reportError msg -- reqs template-haskell 2.8.0.0
    ; Just undefinedName <- TH.lookupValueName "Prelude.undefined"
    ; return $ VarE undefinedName
    }

showOccName :: Name -> String
showOccName (Name occ _) = occString occ
