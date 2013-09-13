{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- |
-- Module      : Language.C.Inline.Error
-- Copyright   : [2013] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module provides support for error reporting.

module Language.C.Inline.Error (
  -- * Error reporting
  reportErrorAndFail,

  -- * Exception handling
  tryWithPlaceholder,

  -- * Pretty printing for error messages
  prettyQC
) where

import Language.Haskell.TH        as TH
import Language.Haskell.TH.Syntax as TH

  -- quasi-quotation libraries
import Language.C.Quote           as QC
import Language.C.Quote.ObjC      as QC
import Text.PrettyPrint.Mainland  as QC


reportErrorAndFail :: QC.Extensions -> String -> Q a
reportErrorAndFail lang msg
  = do
    { reportError' lang msg
    ; fail "Failure"
    }

-- reportErrorAndBail :: String -> Q TH.Exp
-- reportErrorAndBail msg
--   = do
--     { reportError msg
--     ; Just undefinedName <- TH.lookupValueName "Prelude.undefined"
--     ; return $ VarE undefinedName
--     }

reportError' :: QC.Extensions -> String -> Q ()
reportError' lang msg
  = do
    { loc <- location
    -- FIXME: define a Show instance for 'Loc' and use it to prefix position to error
    ; TH.report True $ "Inline " ++ showLang lang ++ ": " ++ msg
    -- ; TH.reportError msg -- reqs template-haskell 2.8.0.0
    }
  where
    showLang QC.Antiquotation = "C"
    showLang QC.Gcc           = "GCC C"
    showLang QC.CUDA          = "CUDA C"
    showLang QC.OpenCL        = "OpenCL"
    showLang QC.ObjC          = "Objective-C"

-- If the tried computation fails, insert a placeholder expression.
--
-- We report all errors explicitly. Failing would just duplicate errors.
--
tryWithPlaceholder :: Q TH.Exp -> Q TH.Exp
tryWithPlaceholder = ([| error "language-c-quote: internal error: tryWithPlaceholder" |] `recover`)

prettyQC :: QC.Pretty a => a -> String
prettyQC = QC.pretty 80 . QC.ppr
