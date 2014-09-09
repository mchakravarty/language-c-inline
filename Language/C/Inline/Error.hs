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
  reportErrorWithLang, reportErrorAndFail,

  -- * Exception handling
  tryWithPlaceholder,

  -- * Pretty printing for error messages
  prettyQC
) where

import Language.Haskell.TH        as TH

  -- quasi-quotation libraries
import Language.C.Quote           as QC
import Text.PrettyPrint.Mainland  as QC


reportErrorWithLang :: QC.Extensions -> String -> Q ()
reportErrorWithLang lang msg
  = do
    { _loc <- location
    -- FIXME: define a Show instance for 'Loc' and use it to prefix position to error
    ; TH.reportError $ "Inline " ++ showLang lang ++ ": " ++ msg
    }

reportErrorAndFail :: QC.Extensions -> String -> Q a
reportErrorAndFail lang msg
  = reportErrorAndFail' $ "Inline " ++ showLang lang ++ ": " ++ msg

reportErrorAndFail' :: String -> Q a
reportErrorAndFail' msg
  = do
    { TH.reportError msg
    ; fail "Fatal error due to inline code"
    }

-- reportErrorAndBail :: String -> Q TH.Exp
-- reportErrorAndBail msg
--   = do
--     { reportError msg
--     ; Just undefinedName <- TH.lookupValueName "Prelude.undefined"
--     ; return $ VarE undefinedName
--     }

showLang :: QC.Extensions -> String
showLang QC.Antiquotation = "C"
showLang QC.C11           = "C 11"
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
