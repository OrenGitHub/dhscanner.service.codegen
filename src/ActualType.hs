{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ActualType

where

-- project imports
import qualified Fqn

-- project (qualified) imports
import Location
import qualified Ast
import qualified Token

-- general imports
import Data.Map ( Map )

-- general (qualified) imports
import GHC.Generics

data ActualType
   = Any
   | NativeTypeInt
   | NativeTypeStr
   | NativeTypeBool
   | NativeTypeConstInt Int
   | NativeTypeConstStr String
   | NativeTypeConstBool Bool
   | NativeTypeConstNull
   | ClassName ClassNameContent
   | ClassInstance ClassInstanceContent
   | Lambda LambdaContent
   | MethodOfClass MethodOfClassContent
   | UntypedNamedParam Token.ParamName
   | CallMethodOfClass Location String Token.ClassName
   | CallMethodOfUntypedNamedParam Location String Token.ParamName
   | CallFuncFromImportedDir Location String FilePath
   | CallFuncFromImportedFile Location String FilePath
   | Function FunctionContent
   | FirstPartyImport FirstPartyImportContent
   | FirstPartyDirImport FirstPartyDirImportContent
   | ThirdPartyImport ThirdPartyImportContent
   | FieldedAccess ActualType Token.FieldName
   deriving ( Show, Eq, Ord, Generic )

newtype Classes = Classes { actualClasses :: Map String ClassNameContent } deriving ( Show )

newtype ClassNameContent
   = ClassNameContent
     {
         className :: Token.ClassName
     }
     deriving ( Show, Eq, Ord, Generic )

data InstanceName = This | Self deriving ( Show, Eq, Ord, Generic )

data ClassInstanceContent
   = ClassInstanceContent
     {
         instanceName :: InstanceName,
         instanceOfClass :: Token.ClassName
     }
     deriving ( Show, Eq, Ord, Generic )

newtype Methods
   = Methods
     {
         actualMethods :: Map String MethodOfClassContent
     }
     deriving ( Show, Eq, Ord, Generic )

newtype Params
   = Params
     {
         actualParams :: [ Param ]
     }
     deriving ( Show, Eq, Ord, Generic )

data Param
   = Param
     {
         paramName :: Token.ParamName,
         paramSerialIdx :: Int,
         paramActualType :: ActualType
     }
     deriving ( Show, Eq, Ord, Generic )

data FunctionContent
   = FunctionContent
     {
         funcName :: Token.FuncName,
         params :: Params,
         returnType :: ActualType
     }
     deriving ( Show, Eq, Ord )

data MethodOfClassContent
   = MethodOfClassContent
     {
         methodName :: Token.MethodName,
         methodParams :: Params,
         methodOfClass :: Token.ClassName,
         methodReturnType :: ActualType
     }
     deriving ( Show, Eq, Ord, Generic )

newtype LambdaContent
   = LambdaContent
     {
         lambdaLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic )

data FirstPartyImportContent
   = FirstPartyImportContent
     {
         firstPartyImportedLocation :: FilePath,
         firstPartyImportedName :: Maybe String
     }
     deriving ( Show, Eq, Ord, Generic )

data FirstPartyDirImportContent
   = FirstPartyDirImportContent
     {
         firstPartyDirImportLocation :: FilePath,
         firstPartyDirImportAlias :: Maybe String
     }
     deriving ( Show, Eq, Ord, Generic )

data ThirdPartyImportContent
   = ThirdPartyImportContent
     {
         thirdPartyImportedPackageName :: String,
         thirdPartyImportedPartsFromPackage :: [ String ],
         thirdPartyImportJustOneName :: Maybe String,
         thirdPartyImportWithAlias :: Maybe String
     }
     deriving ( Show, Eq, Ord, Generic )

inferFromBinop :: ActualType -> ActualType -> Ast.Operator -> ActualType
inferFromBinop NativeTypeStr NativeTypeStr Ast.PLUS = NativeTypeStr
inferFromBinop NativeTypeInt NativeTypeInt Ast.PLUS = NativeTypeInt
inferFromBinop _ _ _ = Any

toFqn :: ActualType -> Fqn.Fqn
toFqn (ThirdPartyImport (ThirdPartyImportContent x ys z w)) = Fqn.ThirdPartyImport (Fqn.ThirdPartyImportContent x ys z w)
toFqn (FirstPartyImport (FirstPartyImportContent f name )) = Fqn.FirstPartyImport (Fqn.FirstPartyImportContent f name)
toFqn (FirstPartyDirImport (FirstPartyDirImportContent d a )) = Fqn.FirstPartyDirImport (Fqn.FirstPartyDirImportContent d a)
toFqn NativeTypeStr = Fqn.NativeTypeString
toFqn (ClassName (ClassNameContent c)) = Fqn.ClassName (Fqn.ClassNameContent c)
toFqn (FieldedAccess t field) = Fqn.FieldedAccess (toFqn t) field
toFqn (ClassInstance (ClassInstanceContent This name)) = Fqn.ClassInstance (Fqn.ClassInstanceContent Fqn.This name)
toFqn (ClassInstance (ClassInstanceContent Self name)) = Fqn.ClassInstance (Fqn.ClassInstanceContent Fqn.Self name)
toFqn (CallMethodOfClass call method c) = Fqn.CallMethodOfClass call method c
toFqn (CallMethodOfUntypedNamedParam call method p) = Fqn.CallMethodOfUntypedNamedParam call method p
toFqn (CallFuncFromImportedDir call func d) = Fqn.CallFuncFromImportedDir call func d
toFqn (CallFuncFromImportedFile call func f) = Fqn.CallFuncFromImportedFile call func f
toFqn _ = Fqn.Unknown
