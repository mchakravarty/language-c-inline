{-# LANGUAGE TemplateHaskell, GADTs, FlexibleInstances #-}

-- |
-- Module      : Language.C.Inline.Hint
-- Copyright   : [2013..2014] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module provides the definition of marshalling hints.

module Language.C.Inline.Hint (
  -- * Annotations
  Annotated(..), (<:), void, annotatedShowQ,
  
  -- * Hints
  Hint(..), 
  
  -- * Querying of annotated entities
  haskellTypeOf, foreignTypeOf, stripAnnotation
) where

  -- common libraries
import Control.Applicative
import Language.Haskell.TH        as TH
import Language.Haskell.TH.Syntax as TH

  -- quasi-quotation libraries
import Language.C.Quote           as QC

  -- friends
import Language.C.Inline.Error


-- |Annotating entities with hints.
--
-- The alternatives are to provide an explicit marshalling hint with '(:>)', or to leave the marshalling
-- implicitly defined by the name's type.
--
data Annotated e where
  (:>)  :: Hint hint => e    -> hint -> Annotated e         -- explicit marshalling hint
  Typed ::              Name         -> Annotated Name      -- marshalling implicitly defined by name's type

-- |We provide additional syntax where the hint is to the left of the annotated entity.
--
(<:) :: Hint hint => hint -> e -> Annotated e
(<:) = flip (:>)

-- |Annotation for irrelevant results
--
void :: e -> Annotated e
void = (''() <:)

-- |Pretty print an annotated entity.
--
annotatedShowQ :: Show e => Annotated e -> Q String
annotatedShowQ (e :> hint)  = ((show e ++ " :> ") ++) <$> showQ hint
annotatedShowQ (Typed name) = return $ "Typed " ++ show name

-- |Hints imply marshalling strategies, which include source and destination types for marshalling.
--
class Hint hint where
  haskellType :: hint -> Q TH.Type
  foreignType :: hint -> Q (Maybe QC.Type)    -- ^In case of 'Nothing', the foreign type is determined by the Haskell type.
  showQ       :: hint -> Q String
  
instance Hint Name where   -- must be a type name
  haskellType = conT
  foreignType = const (return Nothing)
  showQ       = return . show
      
instance Hint (Q TH.Type) where
  haskellType = id
  foreignType = const (return Nothing)
  showQ       = (show <$>)

-- |Determine the Haskell type implied for the given annotated entity.
--
haskellTypeOf :: Annotated e -> Q TH.Type
haskellTypeOf (_ :> hint)  = haskellType hint
haskellTypeOf (Typed name)
  = do
    { info <- reify name
    ; case info of
        ClassOpI _ ty _ _ -> return ty
        VarI     _ ty _ _ -> return ty
        nonVarInfo    -> 
          do
          { reportErrorAndFail QC.ObjC $ 
              "expected '" ++ show name ++ "' to be a typed variable name, but it is " ++ 
              show (TH.ppr nonVarInfo)
          }
    }

-- |Determine the foreign type *directly* implied for the given annotated entity if any.
--
foreignTypeOf :: Annotated e -> Q (Maybe QC.Type)
foreignTypeOf (_ :> hint)  = foreignType hint
foreignTypeOf (Typed name) = return Nothing

-- |Remove the annotation.
--
stripAnnotation :: Annotated e -> e
stripAnnotation (e :> hint)  = e
stripAnnotation (Typed name) = name
