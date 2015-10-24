{-# LANGUAGE TemplateHaskell, GADTs, FlexibleInstances #-}

-- |
-- Module      : Language.C.Inline.TH
-- Copyright   : 2014 Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@justtesting.org>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module provides Template Haskell convenience functions.

module Language.C.Inline.TH (
  -- * Decompose type expressions
  headTyConName, headTyConNameOrError,
  
  -- * Decompose idiomatic declarations
  foreignWrapperDatacon, ptrOfForeignPtrWrapper, unwrapForeignPtrWrapper
) where

  -- standard libraries
import Control.Applicative
import Foreign.Ptr
import Foreign.ForeignPtr
import Language.Haskell.TH        as TH
import Language.Haskell.TH.Syntax as TH

  -- quasi-quotation libraries
import Language.C.Quote           as QC

  -- friends
import Language.C.Inline.Error


-- |Project the name of the head of a type term if it is a type constructor.
--
headTyConName :: TH.Type -> Maybe TH.Name
headTyConName ty
  = case splitAppTy ty of
      (ConT name, _) -> Just name
      _              -> Nothing

-- |Like 'headTyConName', but fail if the head is not a type constructor.
--
headTyConNameOrError :: QC.Extensions -> TH.Type -> Q TH.Name
headTyConNameOrError lang ty
  = case headTyConName ty of
      Just name -> return name
      Nothing   -> reportErrorAndFail lang $ "expected the head of '" ++ show ty ++ "' to be a type constructor"

-- |Decompose an n-ary type application into its head and arguments.
--
splitAppTy :: TH.Type -> (TH.Type, [TH.Type])
splitAppTy = split []
  where
    split args (ty `AppT` arg) = split (arg:args) ty
    split args (SigT ty _)     = split args       ty
    split args ty              = (ty, args)

-- |Obtain the data constructor of the newtype in an idiomatic 'ForeignPtr' wrapper of the form
--
-- > newtype Wrapper <tvs> = Wrapper (ForeignPtr (Wrapper <tvs>))
--
foreignWrapperDatacon :: TH.Type -> Q TH.Exp
foreignWrapperDatacon ty
  = do 
    { (datacon, _) <- decomposeForeignPtrWrapper ty
    ; return $ ConE datacon
    }

-- |Unwraps a newtype wrapper around a foreign pointer and turns the 'ForeignPtr' into a 'Ptr'.
--
ptrOfForeignPtrWrapper :: TH.Type -> Q TH.Type
ptrOfForeignPtrWrapper ty = [t| Ptr $(snd <$> decomposeForeignPtrWrapper ty) |]

-- |Generate code that unwraps the foreign pointer inside the given foreign pointer wrapper type.
--
unwrapForeignPtrWrapper :: TH.Type -> Q TH.Exp
unwrapForeignPtrWrapper ty 
  = do
    { (datacon, _) <- decomposeForeignPtrWrapper ty
    ; v <- newName "v"
    ; [| \e -> $(caseE [| e |] [match (conP datacon [varP v]) (normalB $ varE v) []]) |]
    }

-- |Given a type whose head is a newtype wrapper around a foreign pointer of the form
--
-- > newtype Wrapper <tvs> = Wrapper (ForeignPtr (Wrapper <tvs>))
--
-- return the name of the wrapper data constructor and type argument of the 'ForeignPtr', where all '<tvs>' have been
-- substituted by the arguments in the type application constituting the input type (might be nullary).
--
decomposeForeignPtrWrapper :: TH.Type -> Q (TH.Name, TH.Type)
decomposeForeignPtrWrapper ty
  = do 
    { let (tycon, args) = splitAppTy ty
    ; name <- case tycon of
                ConT name -> return name
                _         -> 
                  do
                  { reportErrorAndFail QC.ObjC $ 
                      "expected '" ++ show tycon ++ "' be a type constructor of a 'ForeignPtr' wrapper"
                  }

    ; info <- reify name
    ; case info of
        TyConI (NewtypeD [] _name tvs (NormalC dataconName [(_strict, ConT fptr `AppT` ptrArg)]) _deriv) 
          | fptr == ''ForeignPtr
          -> return (dataconName, substitute (zip args tvs) ptrArg)
        nonForeign -> 
          do
          { reportErrorAndFail QC.ObjC $ 
              "expected '" ++ show name ++ "' to refer to a 'ForeignPtr' wrapped into a newtype, but it is " ++ 
              show (TH.ppr nonForeign)
          }
    }
  where
    substitute :: [(TH.Type, TH.TyVarBndr)] -> TH.Type -> TH.Type
    substitute subst (ForallT boundTvs cxt body)
      = ForallT boundTvs (substituteCxt subst' cxt) (substitute subst' body)
      where
        subst' = filter (`notShadowedBy` map theTV boundTvs) subst
        --
        (_, tv) `notShadowedBy` boundTvs = theTV tv `notElem` boundTvs
        --
        theTV (PlainTV tv)    = tv
        theTV (KindedTV tv _) = tv
    substitute subst (t1 `AppT` t2)
      = (substitute subst t1) `AppT` (substitute subst t2)
    substitute subst (SigT ty ki)
      = SigT (substitute subst ty) ki
    substitute subst (VarT tv)
      = substituteName subst tv
    substitute _subst ty
      = ty
    
    substituteCxt subst cxt = map (substitute subst) cxt
    
    substituteName []               tv     = VarT tv
    substituteName ((arg, tv):args) thisTv
      | tv `matches` thisTv = arg
      | otherwise           = VarT thisTv
      
    PlainTV  name     `matches` thisTv = name == thisTv
    KindedTV name _ki `matches` thisTv = name == thisTv
