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
  
  -- * State query and update operations
  setForeignTable, stashHeader, stashObjC_h, stashObjC_m, stashHS, 
  extendJumpTable,
  getForeignTable, getForeignLabels, getHeaders, getHoistedObjC, getHoistedHS
) where

  -- common libraries
import Control.Applicative
import Data.IORef
import Foreign.C                  as C
import Language.Haskell.TH        as TH
import Language.Haskell.TH.Syntax as TH
import System.IO.Unsafe                 (unsafePerformIO)

  -- quasi-quotation libraries
import Language.C.Quote           as QC


data State 
  = State
    { foreignTable  :: Q TH.Exp            -- table of foreign labels
    , foreignLabels :: [Name]              -- list of foreign imported names to populate 'foreignTable'
    , headers       :: [String]            -- imported Objective-C headers
    , hoistedObjC_h :: [QC.Definition]     -- Objective-C that goes into the .h
    , hoistedObjC_m :: [QC.Definition]     -- Objective-C that goes into the .m
    , hoistedHS     :: [TH.Dec]            -- Haskell that goes at the end of the module
    }

state :: IORef State
{-# NOINLINE state #-}
state = unsafePerformIO $ 
          newIORef $ 
            State
            { foreignTable  = error "InlineObjC: internal error: 'foreignTable' undefined"
            , foreignLabels = []
            , headers       = []
            , hoistedObjC_h = []
            , hoistedObjC_m = []
            , hoistedHS     = []
            }

readState :: (State -> a) -> Q a
readState read = runIO $ read <$> readIORef state

modifyState :: (State -> State) -> Q ()
modifyState modify = runIO $ modifyIORef state modify
  -- atomic???

setForeignTable :: Q TH.Exp -> Q ()
setForeignTable e = modifyState (\s -> s {foreignTable = e})

stashHeader :: String -> Q ()
stashHeader header = modifyState (\s -> s {headers = header : headers s})

stashObjC_h :: [QC.Definition] -> Q ()
stashObjC_h defs = modifyState (\s -> s {hoistedObjC_h = defs ++ hoistedObjC_h s})

stashObjC_m :: [QC.Definition] -> Q ()
stashObjC_m defs = modifyState (\s -> s {hoistedObjC_m = defs ++ hoistedObjC_m s})

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

getForeignTable :: Q (Q TH.Exp)
getForeignTable = readState foreignTable

getForeignLabels :: Q [Name]
getForeignLabels = readState foreignLabels

getHeaders :: Q [String]
getHeaders = reverse <$> readState headers

getHoistedObjC :: Q ([QC.Definition], [QC.Definition])
getHoistedObjC = (,) <$> readState hoistedObjC_h <*> readState hoistedObjC_m

getHoistedHS :: Q [TH.Dec]
getHoistedHS = readState hoistedHS
