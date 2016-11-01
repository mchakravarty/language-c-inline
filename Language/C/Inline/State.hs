{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- |
-- Module      : Language.C.Inline.State
-- Copyright   : [2013] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module manages the state accumulated during the compilation of one module.

module Language.C.Inline.State (
  -- * Abstract application state
  State,
  initialiseState,
  
  -- ** State query and update operations
  setForeignTable, stashHeader, stashMarshaller, stashObjC_h, stashObjC_m, stashHS, 
  extendJumpTable,
  getForeignTable, getForeignLabels, getHeaders, getMarshallers, lookupMarshaller, getHoistedObjC, getHoistedHS
) where

  -- common libraries
import Control.Applicative
import Data.IORef
import Language.Haskell.TH        as TH
import System.IO.Unsafe                 (unsafePerformIO)

  -- quasi-quotation libraries
import Language.C.Quote           as QC


type CustomMarshaller = ( TH.Type         -- Haskell type
                        , TH.Type         -- Haskell-side class type
                        , QC.Type         -- C type
                        , TH.Name         -- Haskell->C marshaller function
                        , TH.Name)        -- C->Haskell marshaller function

data State
  = State
    { foreignTable  :: Q TH.Exp            -- table of foreign labels
    , foreignLabels :: [Name]              -- list of foreign imported names to populate 'foreignTable'
    , headers       :: [String]            -- imported Objective-C headers
    , marshallers   :: [CustomMarshaller]  -- User defined marshallers
    , hoistedObjC_h :: [QC.Definition]     -- Objective-C that goes into the .h
    , hoistedObjC_m :: [QC.Definition]     -- Objective-C that goes into the .m
    , hoistedHS     :: [TH.Dec]            -- Haskell that goes at the end of the module
    }

state :: IORef State
{-# NOINLINE state #-}
state = unsafePerformIO $ 
          newIORef initialState

initialState :: State
initialState 
  = State
    { foreignTable  = error "Language.C.Inline.State: internal error: 'foreignTable' undefined"
    , foreignLabels = []
    , headers       = []
    , marshallers   = []
    , hoistedObjC_h = []
    , hoistedObjC_m = []
    , hoistedHS     = []
    }
    
initialiseState :: Q ()
initialiseState = modifyState (const initialState)

readState :: (State -> a) -> Q a
readState reader = runIO $ reader <$> readIORef state

modifyState :: (State -> State) -> Q ()
modifyState modify = runIO $ modifyIORef state modify
  -- atomic???

setForeignTable :: Q TH.Exp -> Q ()
setForeignTable e = modifyState (\s -> s {foreignTable = e})

stashHeader :: String -> Q ()
stashHeader header = modifyState (\s -> s {headers = header : headers s})

stashMarshaller :: CustomMarshaller -> Q ()
stashMarshaller marshaller = modifyState (\s -> s {marshallers = marshaller : marshallers s})

stashObjC_h :: [QC.Definition] -> Q ()
stashObjC_h defs = modifyState (\s -> s {hoistedObjC_h = hoistedObjC_h s ++ defs})

stashObjC_m :: [QC.Definition] -> Q ()
stashObjC_m defs = modifyState (\s -> s {hoistedObjC_m = hoistedObjC_m s ++ defs})

stashHS :: [TH.DecQ] -> Q ()
stashHS decQs
  = do
    { decs <- sequence decQs
    ; modifyState (\s -> s {hoistedHS = hoistedHS s ++ decs})
    }

extendJumpTable :: Name -> Q Int
extendJumpTable foreignName
  = do
    { modifyState (\s -> s {foreignLabels = foreignLabels s ++ [foreignName]})   -- FIXME: *urgh*
    ; length <$> readState foreignLabels
    }

getForeignTable :: Q (Q TH.Exp)
getForeignTable = readState foreignTable

getForeignLabels :: Q [Name]
getForeignLabels = readState foreignLabels

getHeaders :: Q [String]
getHeaders = reverse <$> readState headers

getMarshallers :: Q [CustomMarshaller]
getMarshallers = readState marshallers

lookupMarshaller :: TH.Type -> Q (Maybe CustomMarshaller)
lookupMarshaller ty
  = do
    { mshs <- getMarshallers
    ; case filter (\(hsTy, _, _, _, _) -> hsTy == ty) mshs of
        []           -> return Nothing
        marshaller:_ -> return $ Just marshaller
    }


getHoistedObjC :: Q ([QC.Definition], [QC.Definition])
getHoistedObjC = (,) <$> readState hoistedObjC_h <*> readState hoistedObjC_m

getHoistedHS :: Q [TH.Dec]
getHoistedHS = readState hoistedHS
