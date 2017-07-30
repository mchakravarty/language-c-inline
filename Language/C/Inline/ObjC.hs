{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- |
-- Module      : Language.C.Inline.ObjC
-- Copyright   : [2013..2016] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This module exports the principal API for inline Objective-C.

module Language.C.Inline.ObjC (

  -- * Re-export types from 'Foreign.C'
  module Foreign.C.Types, CString, CStringLen, CWString, CWStringLen, Errno, ForeignPtr, castForeignPtr,

  -- * Re-export types from Template Haskell
  Name,

  -- * Objective-C memory management support
  objc_retain, objc_release, objc_release_ptr, newForeignClassPtr, newForeignStructPtr,

  -- * Combinators for inline Objective-C
  objc_import, objc_interface, objc_implementation, objc_record, objc_marshaller, objc_class_marshaller,
  objc_struct_marshaller, objc_typecheck, objc, objc_emit,

  -- * Marshalling annotations
  Annotated(..), (<:), void, Class(..), Struct(..), IsType,

  -- * Property maps
  PropertyAccess, (==>), (-->)
) where

  -- common libraries
import Control.Applicative
import Control.Monad              hiding (void)
import Data.Array
import Data.Char
import Data.Dynamic
import Data.IORef
import Data.List
import Data.Maybe
import Foreign.C                  as C
import Foreign.C.String           as C
import Foreign.C.Types
import Foreign.ForeignPtr         as C
import Foreign.Marshal            as C  hiding (void)
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
import Language.C.Inline.Hint
import Language.C.Inline.State
import Language.C.Inline.TH
import Language.C.Inline.ObjC.Hint
import Language.C.Inline.ObjC.Marshal


-- Combinators for inline Objective-C
-- ----------------------------------

-- |Specify imported Objective-C files. Needs to be spliced where an import declaration can appear. (Just put it
-- straight after all the import statements in the module.)
--
-- NB: This inline splice must appear before any other use of inline code in a module.
--
-- FIXME: need to use TH.addDependentFile on each of the imported ObjC files & read headers
--
objc_import :: [FilePath] -> Q [TH.Dec]
objc_import headers
  = do
    {   -- explicitly initialise the state as we can run multiple times in a --make compile
    ; initialiseState

    ; mapM_ stashHeader headers
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
objc_implementation :: [Annotated TH.Name] -> [QC.Definition] -> Q [TH.Dec]
objc_implementation ann_vars defs
  = do
    { mapM_ exportVar ann_vars
    ; stashObjC_m defs
    ; return []
    }
  where
    exportVar ann_var
      = do
        {   -- Determine the argument and result types of the exported Haskell function
        ; let var = stripAnnotation ann_var
        ; (tvs, argTys, inIO, resTy) <- splitHaskellType <$> haskellTypeOf ann_var

            -- Determine C types
        ; maybe_cArgTys <- mapM (haskellTypeToCType ObjC) argTys
        ; maybe_cResTy  <- haskellTypeToCType ObjC resTy
        ; let cannotMapAllTypes = Nothing `elem` (maybe_cResTy : maybe_cArgTys)
              cArgTys           = map maybeErrorCtype maybe_cArgTys
              cResTy            = maybeErrorCtype maybe_cResTy

        ; if cannotMapAllTypes
          then do {str <- annotatedShowQ ann_var; reportErrorWithLang ObjC $ "invalid marshalling: " ++ str}
          else do

        {   -- Determine the bridging type and the marshalling code
        ; (bridgeArgTys, cBridgeArgTys, hsArgMarshallers, cArgMarshallers) <-
            unzip4 <$> zipWithM (generateCToHaskellMarshaller Nothing) argTys cArgTys
        ; (bridgeResTy,  cBridgeResTy,  hsResMarshaller,  cResMarshaller)  <- generateHaskellToCMarshaller resTy cResTy

            -- Haskell type of the foreign wrapper function
        ; let hsWrapperTy = haskellWrapperType tvs bridgeArgTys bridgeResTy

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
            map makeStaticFunc wrapperDef
        } }

    splitHaskellType (ForallT tvs _ctxt ty)                   -- collect quantified variables (drop the context)
      = let (tvs', args, inIO, res) = splitHaskellType ty
        in
        (tvs ++ tvs', args, inIO, res)
    splitHaskellType (ArrowT `AppT` arg `AppT` res)           -- collect argument types
      = let (tvs, args, inIO, res') = splitHaskellType res
        in
        (tvs, arg:args, inIO, res')
    splitHaskellType (ConT io `AppT` res) | io == ''IO        -- is it an 'IO' function?
      = ([], [], True, res)
    splitHaskellType res
      = ([], [], False, res)

    makeStaticFunc (FuncDef (Func    dspec f decl ps    body loc1) loc2)
      = FuncDef (Func    (addStatic dspec) f decl ps    body loc1) loc2
    makeStaticFunc (FuncDef (OldFunc dspec f decl ps ig body loc1) loc2)
      = FuncDef (OldFunc (addStatic dspec) f decl ps ig body loc1) loc2
    makeStaticFunc def = def

    addStatic (DeclSpec         st tqs ts loc) = DeclSpec         (Tstatic loc:st) tqs ts loc
    addStatic (AntiTypeDeclSpec st tqs ts loc) = AntiTypeDeclSpec (Tstatic loc:st) tqs ts loc
    addStatic declSpec                         = declSpec

maybeErrorCtype :: Maybe QC.Type -> QC.Type
maybeErrorCtype Nothing   = [cty| typename __UNDEFINED_TYPE |]    -- placeholder to make progress in the face of errors
maybeErrorCtype (Just ty) = ty

forExpD :: Callconv -> String -> Name -> TypeQ -> DecQ
forExpD cc str n ty
  = do
    { ty' <- ty
    ; return $ ForeignD (ExportF cc str n ty')
    }

-- |Maps a quoted property to a quoted projection and a quoted update function in addition to the type of the projected
-- value.
--
data PropertyAccess = QC.ObjCIfaceDecl :==> (TH.TypeQ, TH.ExpQ, TH.ExpQ)

-- |Map a property to explicit projection and update functions.
--
(==>) = (:==>)

-- |Map a property to a field label. This function assumes that the field name is typed and can be reified.
--
(-->) :: QC.ObjCIfaceDecl -> Name -> PropertyAccess
prop --> fieldName = prop ==> (fieldTy,
                               [| $(varE fieldName) |],
                               [| \s v -> $(recUpdE [|s|] [do { vE <- [|v|]; return (fieldName, vE) }]) |])
  where
    fieldTy
      = do
        { info <- reify fieldName
        ; case info of
            VarI _ (ArrowT `AppT` _ `AppT` resTy) _ -> return resTy
            nonVarInfo ->
              do
              { reportErrorAndFail QC.ObjC $
                  "expected '" ++ show fieldName ++ "' to be a typed record field name, but it is " ++
                  show (TH.ppr nonVarInfo)
              }
        }

-- |Specification of a bridge for a Haskell structure that can be queried and updated from Objective-C.
--
-- The first argument is the name of the Objective-C class that will be a proxy for the Haskell structure.
-- The second argument the name of the Haskell type of the bridged Haskell structure.
--
-- The generated class is immutable. When a property is updated, a new instance is allocated. This closely
-- mirrors the behaviour of the Haskell structure for which the class is a proxy.
--
-- The designated initialiser of the generated class is '[-initWith<HsName>HsPtr:(HsStablePtr)particleHsPtr]',
-- where '<HsName>' is the type name of the Haskell structure. This initialiser is generated if it is not
-- explicitly provided. The generated method '[-init]' calls the designated initialiser with 'nil' for the
-- stable pointer.
--
-- WARNING: This is a very experimental feature and it will SURELY change in the future!!!
--
--FIXME: don't generate the designated initialiser if it is explicitly provided
objc_record :: String                 -- ^prefix of the class name
            -> String                 -- ^class name
            -> TH.Name                -- ^name of the Haskell type of the bridged Haskell structure
            -> [Annotated TH.Name]    -- ^Haskell variables used in Objective-C code
            -> [PropertyAccess]       -- ^Objective-C properties with corresponding Haskell projections and update functions
            -> [QC.ObjCIfaceDecl]     -- ^extra interface declarations
            -> [QC.Definition]        -- ^extra implementation declarations
            -> Q [TH.Dec]
objc_record prefix objcClassName hsTyName ann_vars properties ifaceDecls impDecls
  | null objcClassName
  = reportErrorAndFail ObjC "empty class name"
  | otherwise
  = do
    {   -- Turn projection and update functions into Haskell top-level function definitions
    ; let (propTys, propProjFuns, propUpdFuns) = unzip3 [(ty, proj, upd) | (_ :==> (ty, proj, upd)) <- properties]
    ; projNames <- sequence [ return . mkName $ "proj" ++ objcClassName ++ show i | (_, i) <- zip propProjFuns [1..]]
    ; updNames  <- sequence [ return . mkName $ "upd"  ++ objcClassName ++ show i | (_, i) <- zip propProjFuns [1..]]
    ; let projUpd_defs = [ funD name [clause [] (normalB propFun) []]
                         | (name, propFun) <- zip projNames propProjFuns ++ zip updNames propUpdFuns]

        -- All new top-level functions are in the set of free variables for the implementation code
    ; let all_ann_vars = ann_vars ++ zipWith addProjType projNames propTys ++ zipWith addUpdType updNames propTys

        -- Construct the class interface
    ; let propertyDecls     = [prop | (prop :==> _) <- properties]
          updateMethodDecls = concatMap mkUpdateMethodDecl propertyDecls
          iface             = [cunit|
            @interface $id:prefixedClassName : NSObject

            $ifdecls:propertyDecls
            $ifdecls:updateMethodDecls
            $ifdecls:ifaceDecls

            @end
          |]

        -- Construct the class implementation
    ; let updateMethodDefs     = concat $ zipWith mkUpdateMethodDef     propertyDecls updNames
          projectionMethodDefs = concat $ zipWith mkProjectionMethodDef propertyDecls projNames
          imp                  = [cunit|
            @interface $id:prefixedClassName ()
            @property (readonly, assign, nonatomic) typename HsStablePtr $id:hsPtrName;
            @end

            @implementation $id:prefixedClassName

            $edecls:updateMethodDefs
            $edecls:impDecls

            - (instancetype)init
            {
              return [self $id:initWithHsPtrName:nil];
            }

            - (instancetype)$id:initWithHsPtrName:(typename HsStablePtr)$id:hsPtrName
            {
              self = [super init];
              if (self)
                $id:("_" ++ hsPtrName) = $id:hsPtrName;
              return self;
            }

            - (void)dealloc
            {
              hs_free_stable_ptr($id:("_" ++ hsPtrName));
            }

            $edecls:projectionMethodDefs

            @end
          |]

        -- Inline the class interface and class implementation; then, return all new Haskell bindings
    ; iface_defs <- objc_interface iface
    ; imp_defs   <- objc_implementation all_ann_vars imp
    ; fun_defs   <- sequence projUpd_defs
    ; return $ iface_defs ++ imp_defs ++ fun_defs
    }
  where
    addProjType name ty = name :> [t| $(conT hsTyName) -> $ty |]
    addUpdType  name ty = name :> [t| $(conT hsTyName) -> $ty -> $(conT hsTyName) |]

    prefixedClassName = prefix ++ objcClassName
    lowerClassName    = toLower (head objcClassName) : tail objcClassName
    hsTyNameBase      = nameBase hsTyName
    lowerHsTyName     = toLower (head hsTyNameBase) : tail hsTyNameBase
    hsPtrName         = lowerHsTyName ++ "HsPtr"
    initWithHsPtrName = "initWith" ++ hsTyNameBase ++ "HsPtr"

    mkUpdateMethodDecl propDecl@(ObjCIfaceProp _attrs
                                   (FieldGroup spec [Field (Just (Id propName _)) (Just decl) _exp _] loc)
                                   _)
      = [objcifdecls|
          + (instancetype)$id:lowerClassName:(typename $id:prefixedClassName *)$id:lowerClassName
                          $id:("with" ++ upperPropName):($ty:propTy)$id:propName;
        |]
      where
        upperPropName = toUpper (head propName) : tail propName
        propTy        = QC.Type spec decl loc

    mkUpdateMethodDef propDecl@(ObjCIfaceProp _attrs
                                  (FieldGroup spec [Field (Just (Id propName _)) (Just decl) _exp _] loc)
                                  _)
                      updName
      = [objcimdecls|
          + (instancetype)$id:lowerClassName:(typename $id:prefixedClassName *)$id:lowerClassName
                          $id:("with" ++ upperPropName):($ty:propTy)$id:propName
          {
            return [[$id:prefixedClassName alloc] $id:initWithHsPtrName:$id:(show updName)($id:lowerClassName.$id:hsPtrName,
                                                                                       $id:propName)];
          }
        |]
      where
        upperPropName = toUpper (head propName) : tail propName
        propTy        = QC.Type spec decl loc

    mkProjectionMethodDef propDecl@(ObjCIfaceProp _attrs
                                      (FieldGroup spec [Field (Just (Id propName _)) (Just decl) _exp _] loc)
                                      _)
                          updName
      = [objcimdecls|
          - ($ty:propTy)$id:propName
          {
            return $id:(show updName)(self.$id:hsPtrName);
          }
        |]
      where
        propTy = QC.Type spec decl loc

-- |Deprecated: use 'objc_class_marshaller' or 'objc_struct_marshaller' instead
--
objc_marshaller :: TH.Name -> TH.Name -> Q [TH.Dec]
{-# DEPRECATED objc_marshaller "use 'objc_class_marshaller' or 'objc_struct_marshaller' instead" #-}
objc_marshaller = objc_class_marshaller

-- |Declare a Haskell<->Objective-C marshaller pair to be used in all subsequent marshalling code generation.
--
-- On the Objective-C side, the marshallers must use a wrapped foreign pointer to an Objective-C class (just as those
-- of 'Class' hints). The domain and codomain of the two marshallers must be the opposite and both are executing in 'IO'.
--
objc_class_marshaller :: TH.Name -> TH.Name -> Q [TH.Dec]
objc_class_marshaller = objc_marshaller' 'newForeignClassPtr

-- |Declare a Haskell<->Objective-C marshaller pair to be used in all subsequent marshalling code generation.
--
-- On the Objective-C side, the marshallers must use a wrapped foreign pointer to an C struct (just as those
-- of 'Struct' hints). The domain and codomain of the two marshallers must be the opposite and both are executing in 'IO'.
--
objc_struct_marshaller :: TH.Name -> TH.Name -> Q [TH.Dec]
objc_struct_marshaller = objc_marshaller' 'newForeignStructPtr

objc_marshaller' :: TH.Name -> TH.Name -> TH.Name -> Q [TH.Dec]
objc_marshaller' newForeignPtrFun haskellToObjCName objcToHaskellName
  = do
    {   -- check that the marshallers have compatible types
    ; (hsTy1, classTy1) <- argAndResultTy haskellToObjCName
    ; (classTy2, hsTy2) <- argAndResultTy objcToHaskellName
    ; unless (hsTy1 == hsTy2 && classTy1 == classTy2) $
        reportErrorAndFail QC.ObjC $
          "the two marshallers must map between the same types"

    ; tyconName <- headTyConNameOrError QC.ObjC classTy1
    ; let cTy = [cty| typename $id:(nameBase tyconName) * |]
    ; stashMarshaller (hsTy1, classTy1, cTy, haskellToObjCName, objcToHaskellName, newForeignPtrFun)
    ; return []
    }
  where
    argAndResultTy name
      = do
        { info <- reify name
        ; case info of
            VarI _ (ArrowT `AppT` argTy `AppT` (ConT io `AppT` resTy)) _
              | io == ''IO
              -> return (argTy, resTy)
            VarI _ ty _ -> reportErrorAndFail QC.ObjC $
                               show name ++ "'s type must match 'a -> IO r'"
            other         -> reportErrorAndFail QC.ObjC $
                               show name ++ " must be a function"
        }

-- |Inline Objective-C expression.
--
-- The inline expression will be wrapped in a C function whose arguments are marshalled versions of the Haskell
-- variables given in the first argument. The marshalling of the variables and of the result is determined by the
-- marshalling annotations at the variables and the inline expression.
--
objc :: [Annotated TH.Name] -> Annotated QC.Exp -> Q TH.Exp
objc ann_vars ann_e
  = {- tryWithPlaceholder $ -} do  -- FIXME: catching the 'fail' purges all reported errors :(
    {   -- Sanity check of arguments
    ; let vars = map stripAnnotation ann_vars
    ; varTys <- mapM haskellTypeOf ann_vars
    ; resTy  <- haskellTypeOf ann_e
    ; newFP  <- newForeignPtrOf ann_e

        -- Determine C types
    ; maybe_cArgTys <- mapM annotatedHaskellTypeToCType ann_vars
    ; maybe_cResTy  <- annotatedHaskellTypeToCType ann_e
    ; let cannotMapAllTypes = Nothing `elem` (maybe_cResTy : maybe_cArgTys)
          cArgTys           = map maybeErrorCtype maybe_cArgTys
          cResTy            = maybeErrorCtype maybe_cResTy

    ; if cannotMapAllTypes
      then failOn [ann_var | (ann_var, Nothing) <- zip ann_vars maybe_cArgTys] maybe_cResTy
      else do

    {   -- Determine the bridging type and the marshalling code
    ; (bridgeArgTys, cBridgeArgTys, hsArgMarshallers, cArgMarshallers) <-
        unzip4 <$> zipWithM generateHaskellToCMarshaller varTys cArgTys
    ; (bridgeResTy,  cBridgeResTy,  hsResMarshaller,  cResMarshaller)  <-
        generateCToHaskellMarshaller newFP resTy cResTy

        -- Haskell type of the foreign wrapper function
    ; let hsWrapperTy = haskellWrapperType [] bridgeArgTys bridgeResTy

        -- FFI setup for the C wrapper
    ; loc <- location
    ; let modName = dropExtension . takeFileName . loc_filename $ loc
    ; cwrapperName <- show <$> newName ("cwrapper_" ++ modName) >>= newName   -- Don't ask...
    ; stashHS
        [ forImpD CCall Safe (show cwrapperName) cwrapperName hsWrapperTy
        ]
    ; idx <- extendJumpTable cwrapperName

        -- Generate the C wrapper code (both prototype and definition)
    ; cArgVars <- mapM (newName . nameBase) vars
    ; let (wrapperProto, wrapperDef)
            = generateCWrapper cwrapperName cArgTys vars cArgMarshallers cBridgeArgTys cArgVars
                               (stripAnnotation ann_e)
                               resTy cResTy cResMarshaller cBridgeResTy
    ; stashObjC_h wrapperProto
    ; stashObjC_m wrapperDef

        -- Generate invocation of the C wrapper sandwiched into Haskell-side marshalling
    ; generateHSCall vars hsArgMarshallers (callThroughTable idx hsWrapperTy) hsResMarshaller True
    } }
  where
    callThroughTable idx ty
      = do { jumptable <- getForeignTable
           ; [|fromDyn
                 ((unsafePerformIO $ readIORef $jumptable) ! $(TH.lift idx))
                 (error "InlineObjC: INTERNAL ERROR: type mismatch in jumptable")
               :: $ty |]
           }

    failOn err_ann_vars maybe_cResTy
      = do
        { unless (null err_ann_vars) $ do
            { var_strs <- mapM annotatedShowQ err_ann_vars
            ; reportErrorWithLang ObjC $ "invalid marshalling: " ++ intercalate ", " var_strs
            }
        ; unless (isJust maybe_cResTy) $ do
            { ty <- haskellTypeOf ann_e
            ; reportErrorWithLang ObjC $ "invalid marshalling for result type " ++ show ty
            }
        ; [| error "error in inline Objective-C expression" |]
        }

    annotatedHaskellTypeToCType ann_e
      = do
        { maybe_objcType <- foreignTypeOf ann_e
        ; case maybe_objcType of
            Nothing       -> haskellTypeOf ann_e >>= haskellTypeToCType ObjC
            Just objcType -> return $ Just objcType
        }

-- Turn a list of argument types and a result type into a Haskell wrapper signature.
--
-- > haskellWrapperType [tv1, .., tvm] [a1, .., an] r = [| forall tv1 .. tvm. a1 -> .. -> an -> IO r |]
--
haskellWrapperType :: [TH.TyVarBndr] -> [TH.TypeQ] -> TH.TypeQ -> TH.TypeQ
haskellWrapperType []  argTys resTy = wrapperBodyType argTys resTy                          -- monotype
haskellWrapperType tvs argTys resTy = forallT tvs (cxt []) (wrapperBodyType argTys resTy)   -- polytype

wrapperBodyType :: [TH.TypeQ] -> TH.TypeQ -> TH.TypeQ
wrapperBodyType []             resTy = [t| IO $resTy |]
wrapperBodyType (argTy:argTys) resTy = [t| $argTy -> $(wrapperBodyType argTys resTy) |]

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
-- * If the list is empty, we will return a singleton 'void' parameter.
--
cParams :: [QC.Type] -> [TH.Name] -> [QC.Param]
cParams []  []    = [ [cparam| void |] ]
cParams tys names = cParams' tys names
  where
    cParams' [] []                     = []
    cParams' (argTy:argTys) (var:vars) = [cparam| $ty:argTy $id:(show var) |] : cParams' argTys vars


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
    ; allHeaders       <- getHeaders
    ; (objc_h, objc_m) <- getHoistedObjC
    ; let (hsFFIHeader, headers) = separateHsFFI allHeaders
    ; runIO $
        do
        { writeFile  objcFname_h (info origFname)
        ; appendFile objcFname_h (unlines (map mkImport headers) ++ "\n")
        ; appendFile objcFname_h (pretty 100 $ QC.ppr objc_h)
        ; writeFile  objcFname_m (info origFname)
        ; appendFile objcFname_m ("#import \"" ++ takeFileName objcFname_h ++ "\"\n")
        ; appendFile objcFname_m (mkImport hsFFIHeader ++ "\n\n")
        ; appendFile objcFname_m (pretty 100 $ QC.ppr objc_m)
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
    hsFFI = "HsFFI.h"     -- Haskell C FFI header as prescribed in the standard

      -- If the user supplies the FFI header (presumably at a non-standard location), use that; otherwise, we include
      -- the header without a path. (The FFI header should by default only be included into the .m file; otherwise, we
      -- get into problems with framework modules.)
    separateHsFFI headers
      = case break ((== hsFFI) . takeFileName) headers of
          (before, [])        -> (hsFFI, before)
          (before, ffi:after) -> (ffi, before ++ after)

    mkImport h@('<':_) = "#import " ++ h ++ ""
    mkImport h         = "#import \"" ++ h ++ "\""

    info fname = "// Generated code: DO NOT EDIT\n\
                 \//   generated from '" ++ fname ++ "'\n\
                 \//   by package 'language-c-inline'\n\n"

-- |Force type checking of all declaration appearing earlier in this module.
--
-- Template Haskell performs type checking on declaration groups seperated by toplevel splices. In order for a type
-- declaration to be available to an Objective-C inline directive, the type declaration must be in an earlier
-- declaration group than the Objective-C inline directive. A toplevel Objective-C inline directive always is the start
-- of a new declaration group; hence, it can be considered to be implicitly preceded by an 'objc_typecheck'.
--
objc_typecheck :: Q [TH.Dec]
objc_typecheck = return []
