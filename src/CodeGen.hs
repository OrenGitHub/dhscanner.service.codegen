{-# OPTIONS -Werror=missing-fields #-}
{-# OPTIONS -Wno-missing-signatures #-}

module CodeGen

where

-- project imports
import Fqn
import Cfg
import Callable
import Location
import ActualType hiding ( returnType )
import SymbolTable ( SymbolTable, emptySymbolTable)

-- project (qualified) imports
import qualified Ast
import qualified Token
import qualified Bitcode
import qualified ActualType
import qualified SymbolTable

-- general imports
import Data.Maybe
import Data.List hiding ( init )
import Prelude hiding ( exp, init )
import Control.Monad.State.Lazy
import Data.Functor (void)
import System.FilePath

-- general (qualified) imports
import qualified Data.Map

data CodeGenState
   = CodeGenState
     {
         symbolTable :: SymbolTable,
         returnValue :: Maybe Bitcode.TmpVariable,
         returnTo :: Maybe Cfg,
         continueTo :: Maybe Cfg,
         breakTo :: Maybe Cfg,
         callables :: [ Callable ]
     }
     deriving ( Show )

-- | Internal use only
data GeneratedExp
   = GeneratedExp
     {
         generatedCfg :: Cfg,
         generatedValue :: Bitcode.Value,
         inferredActualType :: ActualType
     }
     deriving ( Show )

-- | for the sake of documentation
type Callee = GeneratedExp
type Args = [ GeneratedExp ]

type CodeGenContext = State CodeGenState

initCodeGenState :: CodeGenState
initCodeGenState = CodeGenState {
    symbolTable = emptySymbolTable,
    returnValue = Nothing,
    returnTo = Nothing,
    continueTo = Nothing,
    breakTo = Nothing,
    callables = []
}

-- | API: generates code packed as a collection of callables
codeGen :: Ast.Root -> Callables
codeGen ast = Callables (callables (execState (codeGenRoot ast) initCodeGenState))

codeGenRoot :: Ast.Root -> CodeGenContext ()
codeGenRoot (Ast.Root filename' stmts) = do
    beginScope
    scriptCfg <- codeGenStmts stmts -- script part
    endScope
    ctx <- get; -- function + lambda callables
    let callables' = scriptToCallable filename' scriptCfg : callables ctx -- combine
    put $ ctx { callables = callables' } -- write back to state

codeGenStmtClass :: Ast.StmtClassContent -> CodeGenContext Cfg
codeGenStmtClass stmtClass = do
    beginScope
    insertSelf (Ast.stmtClassName stmtClass)
    codeGenDataMembers (Ast.stmtClassDataMembers stmtClass)
    codeGenMethods (Ast.stmtClassMethods stmtClass)
    endScope
    return Cfg.Empty

insertSelf :: Token.ClassName -> CodeGenContext ()
insertSelf name = do
    ctx <- get
    let classNameToken = Token.getClassNameToken name
    let selfVarName = Token.VarName (Token.Named "self" (Token.location classNameToken))
    let actualType = ActualType.ClassInstance (ActualType.ClassInstanceContent ActualType.Self name)
    let classFqn = ActualType.toFqn actualType
    let selfSrcVar = Bitcode.SrcVariable classFqn selfVarName
    let selfVar = Bitcode.SrcVariableCtor selfSrcVar
    let symbolTable' = SymbolTable.insertVar selfVarName selfVar actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }

codeGenMethods :: Ast.Methods -> CodeGenContext ()
codeGenMethods = codeGenMethods' . Data.Map.elems . Ast.actualMethods

codeGenDataMembers :: Ast.DataMembers -> CodeGenContext ()
codeGenDataMembers _ = return () -- TODO: implement me ...

codeGenMethods' :: [ Ast.StmtMethodContent ] -> CodeGenContext ()
codeGenMethods' = mapM_ codeGenStmtMethod

insertEmptyClass :: Token.ClassName -> CodeGenContext ()
insertEmptyClass name = do
    ctx <- get
    let classNameToken = Token.getClassNameToken name
    let classVarName = Token.VarName classNameToken
    let actualType = ActualType.ClassName (ActualType.ClassNameContent name)
    let classFqn = ActualType.toFqn actualType
    let classSrcVar = Bitcode.SrcVariable classFqn classVarName
    let classVar = Bitcode.SrcVariableCtor classSrcVar
    let symbolTable' = SymbolTable.insertClass name classVar actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }

codeGenStmtMethodSingle :: Ast.StmtMethodContent -> CodeGenContext Cfg
codeGenStmtMethodSingle stmtMethod = do
    insertEmptyClass (Ast.hostingClassName stmtMethod)
    beginScope
    params' <- codeGenParams (Ast.stmtMethodParams stmtMethod)
    body <- codeGenStmts (Ast.stmtMethodBody stmtMethod)
    endScope
    ctx <- get
    let symbolTable' = symbolTable ctx
    let callables' = (stmtMethodToCallable stmtMethod symbolTable' (Cfg.concat params' body)) : (callables ctx)
    put (ctx { callables = callables' })
    return Cfg.Empty

codeGenStmtMethod :: Ast.StmtMethodContent -> CodeGenContext ()
codeGenStmtMethod stmtMethod = do
    beginScope
    params' <- codeGenParams (Ast.stmtMethodParams stmtMethod)
    body <- codeGenStmts (Ast.stmtMethodBody stmtMethod)
    endScope
    ctx <- get
    let hostingClass = Ast.hostingClassName stmtMethod
    let rawClassName = Token.getClassNameToken hostingClass
    let classType = ActualType.ClassName (ActualType.ClassNameContent hostingClass)
    let classVar = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable (ActualType.toFqn classType) (Token.VarName rawClassName)
    let symbolTable' = SymbolTable.insertClass hostingClass classVar classType (symbolTable ctx)
    let symbolTable'' = case SymbolTable.lookupVar (Token.VarName rawClassName) (symbolTable ctx) of {
        Nothing -> symbolTable';
        _ -> symbolTable ctx
    }
    let location' = Ast.stmtMethodLocation stmtMethod
    let nop = Bitcode.Instruction location' Bitcode.Nop
    let entrypoint = Cfg.Normal (Cfg.atom (Cfg.Node nop))
    let cfg = Cfg.concat entrypoint (Cfg.concat params' body)
    let callables' = stmtMethodToCallable stmtMethod symbolTable' cfg : callables ctx
    put (ctx { callables = callables', symbolTable = symbolTable'' })

codeGenStmts :: [ Ast.Stmt ] -> CodeGenContext Cfg
codeGenStmts = fmap (foldl' Cfg.concat Cfg.Empty) . codeGenStmts'

codeGenStmts' :: [ Ast.Stmt ] -> CodeGenContext [ Cfg ]
codeGenStmts' = mapM codeGenStmt

-- | A simple dispatcher for code gen statements
-- see `codeGenStmt<TheStatementKindYouWantToInspect>`
codeGenStmt :: Ast.Stmt -> CodeGenContext Cfg
codeGenStmt (Ast.StmtIf     stmtIf    ) = codeGenStmtIf stmtIf
codeGenStmt (Ast.StmtTry    stmtTry   ) = codeGenStmtTry stmtTry
codeGenStmt (Ast.StmtExp    stmtExp   ) = codeGenStmtExp stmtExp
codeGenStmt (Ast.StmtFunc   stmtFunc  ) = codeGenStmtFunc stmtFunc
codeGenStmt (Ast.StmtClass  stmtClass ) = codeGenStmtClass stmtClass
codeGenStmt (Ast.StmtWhile  stmtWhile ) = codeGenStmtWhile stmtWhile
codeGenStmt (Ast.StmtBlock  stmtBlock ) = codeGenStmtBlock stmtBlock
codeGenStmt (Ast.StmtVardec stmtVardec) = codeGenStmtVardec stmtVardec
codeGenStmt (Ast.StmtAssign stmtAssign) = codeGenStmtAssign stmtAssign
codeGenStmt (Ast.StmtImport stmtImport) = codeGenStmtImport stmtImport
codeGenStmt (Ast.StmtReturn stmtReturn) = codeGenStmtReturn stmtReturn
codeGenStmt (Ast.StmtMethod stmtMethod) = codeGenStmtMethodSingle stmtMethod
codeGenStmt _                           = return Cfg.Empty

codeGenStmtTry :: Ast.StmtTryContent -> CodeGenContext Cfg
codeGenStmtTry = codeGenStmts . Ast.stmtTryPart

codeGenStmtExp :: Ast.Exp -> CodeGenContext Cfg
codeGenStmtExp = fmap generatedCfg . codeGenExp

codeGenStmtBlock :: Ast.StmtBlockContent -> CodeGenContext Cfg
codeGenStmtBlock = codeGenStmts . Ast.stmtBlockContent

codeGenStmtReturnNothing :: Ast.StmtReturnContent -> CodeGenContext Cfg
codeGenStmtReturnNothing stmtReturn = do
    let location' = Ast.stmtReturnLocation stmtReturn
    let content' = Bitcode.ReturnContent Nothing
    let returnInstruction = Bitcode.Return content'
    let instruction = Bitcode.Instruction location' returnInstruction
    return $ Cfg.Normal (Cfg.atom (Cfg.Node instruction))

codeGenStmtReturnValue :: Location -> Ast.Exp -> CodeGenContext Cfg
codeGenStmtReturnValue loc exp = do
    returnValue' <- codeGenExp exp
    let content' = Bitcode.ReturnContent (Just (generatedValue returnValue'))
    let returnInstruction = Bitcode.Return content'
    let instruction = Bitcode.Instruction loc returnInstruction
    let returnCfg = Cfg.Normal (Cfg.atom (Cfg.Node instruction))
    return $ generatedCfg returnValue' `Cfg.concat` returnCfg

codeGenStmtReturn :: Ast.StmtReturnContent -> CodeGenContext Cfg
codeGenStmtReturn stmtReturn = case Ast.stmtReturnValue stmtReturn of
    Nothing -> codeGenStmtReturnNothing stmtReturn
    Just returnedValue -> codeGenStmtReturnValue (Ast.stmtReturnLocation stmtReturn) returnedValue

codeGenStmtIf :: Ast.StmtIfContent -> CodeGenContext Cfg
codeGenStmtIf stmtIf = do
    cond <- codeGenExp (Ast.stmtIfCond stmtIf)
    thenPart <- codeGenStmts (Ast.stmtIfBody stmtIf)
    elsePart <- codeGenStmts (Ast.stmtElseBody stmtIf)
    let assumeIfTaken = assumeIfTakenGen (generatedValue cond) (Ast.stmtIfLocation stmtIf)
    let assumeIfNotTaken = assumeIfNotTakenGen (generatedValue cond) (Ast.stmtIfLocation stmtIf)
    let thenPartCfg = assumeIfTaken `Cfg.concatNormalContentFirst` thenPart
    let elsePartCfg = assumeIfNotTaken `Cfg.concatNormalContentFirst` elsePart
    return (generatedCfg cond `Cfg.concat` (thenPartCfg `Cfg.parallelNormalCfgs` elsePartCfg))

codeGenStmtWhile :: Ast.StmtWhileContent -> CodeGenContext Cfg
codeGenStmtWhile stmtWhile = do
    cond <- codeGenExp (Ast.stmtWhileCond stmtWhile)
    body <- codeGenStmts (Ast.stmtWhileBody stmtWhile)
    return $ (generatedCfg cond) `Cfg.concat` body

assumeIfTakenGen :: Bitcode.Value -> Location -> Cfg.Content
assumeIfTakenGen cond loc = let
    content' = Bitcode.Assume $ Bitcode.AssumeContent cond True
    instruction = Bitcode.Instruction loc content'
    in Cfg.atom (Cfg.Node instruction)

assumeIfNotTakenGen :: Bitcode.Value -> Location -> Cfg.Content
assumeIfNotTakenGen cond loc = let
    content' = Bitcode.Assume $ Bitcode.AssumeContent cond False
    instruction = Bitcode.Instruction loc content'
    in Cfg.atom (Cfg.Node instruction)

codeGenAnnotations :: [ Ast.Exp ] -> CodeGenContext [ Callable.Annotation ]
codeGenAnnotations = mapM codeGenAnnotation

stringOrNothing :: Ast.Exp -> Maybe Token.ConstStr
stringOrNothing (Ast.ExpStr (Ast.ExpStrContent s)) = Just s
stringOrNothing _ = Nothing

keepStrings :: [ Ast.Exp ] -> [ Token.ConstStr ]
keepStrings = mapMaybe stringOrNothing

codeGenAnnotation :: Ast.Exp -> CodeGenContext Callable.Annotation
codeGenAnnotation exp = do
    -- exp' <- codeGenExp exp
    -- let fqn = ActualType.toFqn (inferredActualType exp')
    let args = case exp of { (Ast.ExpCall (Ast.ExpCallContent _ a _)) -> a; _ -> [] }
    return $ Callable.Annotation "FIXME-ANNOTATIONS" (map Token.constStrValue (keepStrings args))

codeGenStmtFunc :: Ast.StmtFuncContent -> CodeGenContext Cfg
codeGenStmtFunc stmtFunc = do
    annotations <- codeGenAnnotations (Ast.stmtFuncAnnotations stmtFunc)
    beginScope
    params' <- codeGenParams (Ast.stmtFuncParams stmtFunc)
    body <- codeGenStmts (Ast.stmtFuncBody stmtFunc)
    endScope
    ctx <- get
    let callable = decFuncToCallable stmtFunc (Cfg.concat params' body) annotations
    let callables' = callable : callables ctx
    let funcName' = Token.VarName $ Token.getFuncNameToken (Ast.stmtFuncName stmtFunc)
    let r = Ast.stmtFuncReturnType stmtFunc
    returnType <- case r of { Nothing -> return ActualType.Any; Just r' -> inferredActualType <$> codeGenVar r' }
    let funcContent = ActualType.FunctionContent (Ast.stmtFuncName stmtFunc) (ActualType.Params []) returnType
    let funcType = ActualType.Function funcContent
    let funcFqn = ActualType.toFqn funcType
    let funcVar = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable funcFqn funcName'
    let symbolTable' = SymbolTable.insertVar funcName' funcVar funcType (symbolTable ctx)
    put (ctx { symbolTable = symbolTable', callables = callables' })
    return Cfg.Empty

codeGenStmtImportAllFromLocalFile :: FilePath -> Location -> CodeGenContext Cfg
codeGenStmtImportAllFromLocalFile src l = do
    ctx <- get
    let srcVarName = Token.VarName (Token.Named (takeFileName src) l)
    let actualType = ActualType.FirstPartyImport (ActualType.FirstPartyImportContent src Nothing)
    let srcVar = Bitcode.SrcVariableCtor (Bitcode.SrcVariable (ActualType.toFqn actualType) srcVarName)
    let symbolTable' = SymbolTable.insertVar srcVarName srcVar actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }
    return Cfg.Empty

codeGenStmtImportAllFromLocalDir :: FilePath -> Location -> CodeGenContext Cfg
codeGenStmtImportAllFromLocalDir src l = do
    ctx <- get
    let srcVarName = Token.VarName (Token.Named (takeFileName src) l)
    let actualType = ActualType.FirstPartyDirImport (ActualType.FirstPartyDirImportContent src Nothing)
    let srcVar = Bitcode.SrcVariableCtor (Bitcode.SrcVariable (ActualType.toFqn actualType) srcVarName)
    let symbolTable' = SymbolTable.insertVar srcVarName srcVar actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }
    return Cfg.Empty

codeGenStmtImportAll' :: Ast.ImportLocalContent -> Location -> CodeGenContext Cfg
codeGenStmtImportAll' (Ast.ImportLocalFile src) = codeGenStmtImportAllFromLocalFile src
codeGenStmtImportAll' (Ast.ImportLocalDir src) = codeGenStmtImportAllFromLocalDir src

codeGenStmtImportAll'' :: Ast.ImportThirdPartyContent -> Location -> CodeGenContext Cfg
codeGenStmtImportAll'' (Ast.ImportThirdPartyContent src) l = do
    ctx <- get
    let srcVarName = Token.VarName (Token.Named src l)
    let actualType = ActualType.ThirdPartyImport (ActualType.ThirdPartyImportContent src [] Nothing Nothing)
    let srcVar = Bitcode.SrcVariableCtor (Bitcode.SrcVariable (ActualType.toFqn actualType) srcVarName)
    let symbolTable' = SymbolTable.insertVar srcVarName srcVar actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }
    return Cfg.Empty

codeGenStmtImportAll :: Ast.ImportSource -> Location -> CodeGenContext Cfg
codeGenStmtImportAll (Ast.ImportLocal src) = codeGenStmtImportAll' src
codeGenStmtImportAll (Ast.ImportThirdParty src) = codeGenStmtImportAll'' src

codeGenStmtImportSpecificFromLocalFile :: FilePath -> Ast.ImportSpecific -> Location -> CodeGenContext Cfg
codeGenStmtImportSpecificFromLocalFile src (Ast.ImportSpecific specific) l = do
    ctx <- get
    let specificVarName = Token.VarName (Token.Named specific l)
    let actualType = ActualType.FirstPartyImport (ActualType.FirstPartyImportContent src (Just specific))
    let specificVar = Bitcode.SrcVariableCtor (Bitcode.SrcVariable (ActualType.toFqn actualType) specificVarName)
    let symbolTable' = SymbolTable.insertVar specificVarName specificVar actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }
    return Cfg.Empty

codeGenStmtImportSpecificFromLocalDir :: FilePath -> Ast.ImportSpecific -> Location -> CodeGenContext Cfg
codeGenStmtImportSpecificFromLocalDir src (Ast.ImportSpecific specific) l = do
    ctx <- get
    let specificVarName = Token.VarName (Token.Named specific l)
    let actualType = ActualType.FirstPartyDirImport (ActualType.FirstPartyDirImportContent src Nothing)
    let specificVar = Bitcode.SrcVariableCtor (Bitcode.SrcVariable (ActualType.toFqn actualType) specificVarName)
    let symbolTable' = SymbolTable.insertVar specificVarName specificVar actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }
    return Cfg.Empty

codeGenStmtImportSpecific' :: Ast.ImportLocalContent -> Ast.ImportSpecific -> Location -> CodeGenContext Cfg
codeGenStmtImportSpecific' (Ast.ImportLocalFile src) = codeGenStmtImportSpecificFromLocalFile src
codeGenStmtImportSpecific' (Ast.ImportLocalDir src) = codeGenStmtImportSpecificFromLocalDir src

codeGenStmtImportSpecific'' :: Ast.ImportThirdPartyContent -> Ast.ImportSpecific -> Location -> CodeGenContext Cfg
codeGenStmtImportSpecific'' (Ast.ImportThirdPartyContent src) (Ast.ImportSpecific specific) l = do
    ctx <- get
    let specificVarName = Token.VarName (Token.Named specific l)
    let actualType = ActualType.ThirdPartyImport (ActualType.ThirdPartyImportContent src [] (Just specific) Nothing)
    let specificVar = Bitcode.SrcVariableCtor (Bitcode.SrcVariable (ActualType.toFqn actualType) specificVarName)
    let symbolTable' = SymbolTable.insertVar specificVarName specificVar actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }
    return Cfg.Empty

codeGenStmtImportSpecific :: Ast.ImportSource -> Ast.ImportSpecific -> Location -> CodeGenContext Cfg
codeGenStmtImportSpecific (Ast.ImportLocal src) = codeGenStmtImportSpecific' src
codeGenStmtImportSpecific (Ast.ImportThirdParty src) = codeGenStmtImportSpecific'' src

codeGenStmtImportSpecificWithAliasFromFile :: FilePath -> Ast.ImportSpecific -> Ast.ImportAlias -> Location -> CodeGenContext Cfg
codeGenStmtImportSpecificWithAliasFromFile src (Ast.ImportSpecific specific) (Ast.ImportAlias alias) l = do
    ctx <- get
    let aliasVarName = Token.VarName (Token.Named alias l)
    let actualType = ActualType.FirstPartyImport (ActualType.FirstPartyImportContent src (Just specific))
    let aliasVar = Bitcode.SrcVariableCtor (Bitcode.SrcVariable (ActualType.toFqn actualType) aliasVarName)
    let symbolTable' = SymbolTable.insertVar aliasVarName aliasVar actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }
    return Cfg.Empty

codeGenStmtImportSpecificWithAliasFromDir :: FilePath -> Ast.ImportSpecific -> Ast.ImportAlias -> Location -> CodeGenContext Cfg
codeGenStmtImportSpecificWithAliasFromDir src _ (Ast.ImportAlias alias) l = do
    ctx <- get
    let aliasVarName = Token.VarName (Token.Named alias l)
    let actualType = ActualType.FirstPartyDirImport (ActualType.FirstPartyDirImportContent src (Just alias))
    let aliasVar = Bitcode.SrcVariableCtor (Bitcode.SrcVariable (ActualType.toFqn actualType) aliasVarName)
    let symbolTable' = SymbolTable.insertVar aliasVarName aliasVar actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }
    return Cfg.Empty

codeGenStmtImportSpecificWithAlias' :: Ast.ImportLocalContent -> Ast.ImportSpecific -> Ast.ImportAlias -> Location -> CodeGenContext Cfg
codeGenStmtImportSpecificWithAlias' (Ast.ImportLocalFile src) = codeGenStmtImportSpecificWithAliasFromFile src
codeGenStmtImportSpecificWithAlias' (Ast.ImportLocalDir src) = codeGenStmtImportSpecificWithAliasFromDir src

codeGenStmtImportSpecificWithAlias'' :: Ast.ImportThirdPartyContent -> Ast.ImportSpecific -> Ast.ImportAlias -> Location -> CodeGenContext Cfg
codeGenStmtImportSpecificWithAlias'' (Ast.ImportThirdPartyContent src) (Ast.ImportSpecific specific) (Ast.ImportAlias alias) l = do
    ctx <- get
    let aliasVarName = Token.VarName (Token.Named alias l)
    let actualType = ActualType.ThirdPartyImport (ActualType.ThirdPartyImportContent src [] (Just specific) (Just alias))
    let aliasVar = Bitcode.SrcVariableCtor (Bitcode.SrcVariable (ActualType.toFqn actualType) aliasVarName)
    let symbolTable' = SymbolTable.insertVar aliasVarName aliasVar actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }
    return Cfg.Empty

codeGenStmtImportSpecificWithAlias :: Ast.ImportSource -> Ast.ImportSpecific -> Ast.ImportAlias -> Location -> CodeGenContext Cfg
codeGenStmtImportSpecificWithAlias (Ast.ImportLocal src) = codeGenStmtImportSpecificWithAlias' src
codeGenStmtImportSpecificWithAlias (Ast.ImportThirdParty src) = codeGenStmtImportSpecificWithAlias'' src

codeGenStmtImportAllWithAliasFromLocalFile :: FilePath -> Ast.ImportAlias -> Location -> CodeGenContext Cfg
codeGenStmtImportAllWithAliasFromLocalFile src (Ast.ImportAlias alias) l = do
    ctx <- get
    let aliasVarName = Token.VarName (Token.Named alias l)
    let actualType = ActualType.FirstPartyImport (ActualType.FirstPartyImportContent src (Just alias))
    let aliasVar = Bitcode.SrcVariableCtor (Bitcode.SrcVariable (ActualType.toFqn actualType) aliasVarName)
    let symbolTable' = SymbolTable.insertVar aliasVarName aliasVar actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }
    return Cfg.Empty

codeGenStmtImportAllWithAliasFromLocalDir :: FilePath -> Ast.ImportAlias -> Location -> CodeGenContext Cfg
codeGenStmtImportAllWithAliasFromLocalDir src (Ast.ImportAlias alias) l = do
    ctx <- get
    let aliasVarName = Token.VarName (Token.Named alias l)
    let actualType = ActualType.FirstPartyDirImport (ActualType.FirstPartyDirImportContent src (Just alias))
    let aliasVar = Bitcode.SrcVariableCtor (Bitcode.SrcVariable (ActualType.toFqn actualType) aliasVarName)
    let symbolTable' = SymbolTable.insertVar aliasVarName aliasVar actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }
    return Cfg.Empty

codeGenStmtImportAllWithAlias' :: Ast.ImportLocalContent -> Ast.ImportAlias -> Location -> CodeGenContext Cfg
codeGenStmtImportAllWithAlias' (Ast.ImportLocalFile src) = codeGenStmtImportAllWithAliasFromLocalFile src
codeGenStmtImportAllWithAlias' (Ast.ImportLocalDir src) = codeGenStmtImportAllWithAliasFromLocalDir src

codeGenStmtImportAllWithAlias'' :: Ast.ImportThirdPartyContent -> Ast.ImportAlias -> Location -> CodeGenContext Cfg
codeGenStmtImportAllWithAlias'' (Ast.ImportThirdPartyContent src) (Ast.ImportAlias alias) l = do
    ctx <- get
    let aliasVarName = Token.VarName (Token.Named alias l)
    let actualType = ActualType.ThirdPartyImport (ActualType.ThirdPartyImportContent src [] Nothing (Just alias))
    let aliasVar = Bitcode.SrcVariableCtor (Bitcode.SrcVariable (ActualType.toFqn actualType) aliasVarName)
    let symbolTable' = SymbolTable.insertVar aliasVarName aliasVar actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }
    return Cfg.Empty

codeGenStmtImportAllWithAlias :: Ast.ImportSource -> Ast.ImportAlias -> Location -> CodeGenContext Cfg
codeGenStmtImportAllWithAlias (Ast.ImportLocal src) = codeGenStmtImportAllWithAlias' src
codeGenStmtImportAllWithAlias (Ast.ImportThirdParty src) = codeGenStmtImportAllWithAlias'' src

codeGenStmtImport :: Ast.StmtImportContent -> CodeGenContext Cfg
codeGenStmtImport stmtImport = do
    let loc = Ast.stmtImportLocation stmtImport
    let src = Ast.stmtImportSource stmtImport
    let specific = Ast.stmtImportSpecific stmtImport
    let alias = Ast.stmtImportAlias stmtImport
    case specific of
        Nothing -> case alias of
            Nothing -> codeGenStmtImportAll src loc
            Just alias' -> codeGenStmtImportAllWithAlias src alias' loc
        Just specific' -> case alias of
            Nothing -> codeGenStmtImportSpecific src specific' loc
            Just alias' -> codeGenStmtImportSpecificWithAlias src specific' alias' loc

-- |
--
-- dispatch pure codegen exp handlers:
--
-- * 'codeGenExpInt'
-- * 'codeGenExpStr'
-- * 'codeGenExpBool'
-- * 'codeGenExpNull'
--
-- and monadic ones:
--
-- * 'codeGenExpVar'
-- * 'codeGenExpCall'
-- * 'codeGenExpBinop'
-- * 'codeGenExpAssign'
-- * 'codeGenExpLambda'
--
codeGenExp :: Ast.Exp -> CodeGenContext GeneratedExp
codeGenExp (Ast.ExpInt    expInt    ) = pure $ codeGenExpInt expInt
codeGenExp (Ast.ExpStr    expStr    ) = pure $ codeGenExpStr expStr
codeGenExp (Ast.ExpBool   expBool   ) = pure $ codeGenExpBool expBool
codeGenExp (Ast.ExpNull   expNull   ) = pure $ codeGenExpNull expNull
codeGenExp (Ast.ExpVar    expVar    ) = codeGenExpVar expVar
codeGenExp (Ast.ExpCall   expCall   ) = codeGenExpCall expCall
codeGenExp (Ast.ExpBinop  expBinop  ) = codeGenExpBinop expBinop
codeGenExp (Ast.ExpKwArg  expKwArg  ) = codeGenExpKwArg expKwArg
codeGenExp (Ast.ExpAssign expAssign ) = codeGenExpAssign expAssign
codeGenExp (Ast.ExpLambda expLambda ) = codeGenExpLambda expLambda

codeGenExpAssignToSimpleVar :: Token.VarName -> Ast.Exp -> CodeGenContext GeneratedExp
codeGenExpAssignToSimpleVar varName init = do
    init' <- codeGenExp init
    ctx <- get;
    let initCfg = generatedCfg init'
    let initVariable = generatedValue init'
    let actualType = inferredActualType init'
    let fqn = ActualType.toFqn actualType
    let location' = Token.getVarNameLocation varName
    let existingVar = SymbolTable.lookupVar varName (symbolTable ctx)
    let newVariable = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable fqn varName
    let symbolTable' = SymbolTable.insertVar varName newVariable actualType (symbolTable ctx)
    let updateSymbolTable = ctx { symbolTable = symbolTable' }
    let dontChangeCtx = ctx
    let actualVar = case existingVar of { Nothing -> newVariable; Just (v, _) -> v }
    put $ case existingVar of { Nothing -> updateSymbolTable; _ -> dontChangeCtx }
    let cfg = initCfg `Cfg.concat` (createAssignCfg location' actualVar initVariable)
    return $ GeneratedExp cfg (Bitcode.VariableCtor actualVar) actualType

codeGenExpAssignToFieldVar'' :: Bitcode.Variable -> Cfg -> GeneratedExp -> Token.FieldName -> Location -> CodeGenContext GeneratedExp
codeGenExpAssignToFieldVar'' varLhs varCfg init fieldName loc = do
    let initValue = generatedValue init
    let content' = Bitcode.FieldWriteContent varLhs fieldName initValue
    let fieldWrite = Bitcode.FieldWrite content'
    let instruction = Bitcode.Instruction loc fieldWrite
    let initCfg = generatedCfg init
    let instructionCfg = Cfg.Normal (Cfg.atom (Cfg.Node instruction))
    let cfg = initCfg `Cfg.concat` varCfg `Cfg.concat` instructionCfg
    let actualType = inferredActualType init
    return $ GeneratedExp cfg (Bitcode.VariableCtor varLhs) actualType

codeGenExpAssignToFieldVar' :: GeneratedExp -> GeneratedExp -> Token.FieldName -> Location -> CodeGenContext GeneratedExp
codeGenExpAssignToFieldVar' (GeneratedExp c (Bitcode.VariableCtor v) _) init fieldName loc = codeGenExpAssignToFieldVar'' v c init fieldName loc
codeGenExpAssignToFieldVar' _ init _ _ = return init

codeGenExpAssignToFieldVar :: Ast.VarFieldContent -> Ast.Exp -> CodeGenContext GeneratedExp
codeGenExpAssignToFieldVar v init = do
    init' <- codeGenExp init
    v' <- codeGenExp (Ast.varFieldLhs v)
    codeGenExpAssignToFieldVar' v' init' (Ast.varFieldName v) (Ast.varFieldLocation v)

codeGenExpAssignToSubscriptVar'' :: Bitcode.Variable -> Cfg -> GeneratedExp -> GeneratedExp -> Location -> CodeGenContext GeneratedExp
codeGenExpAssignToSubscriptVar'' v vCfg index init loc = do
    let initValue = generatedValue init
    let indexValue = generatedValue index
    let initCfg = generatedCfg init
    let indexCfg = generatedCfg index
    let actualType = inferredActualType init
    let content' = Bitcode.SubscriptWriteContent v indexValue initValue
    let subscriptWrite = Bitcode.SubscriptWrite content'
    let instruction = Bitcode.Instruction loc subscriptWrite
    let cfg = initCfg `Cfg.concat` indexCfg `Cfg.concat` vCfg
    let finalCfg = cfg `Cfg.concat` Cfg.Normal (Cfg.atom (Cfg.Node instruction))
    return $ GeneratedExp finalCfg (Bitcode.VariableCtor v) actualType

codeGenExpAssignToSubscriptVar' :: GeneratedExp -> GeneratedExp -> GeneratedExp -> Location -> CodeGenContext GeneratedExp
codeGenExpAssignToSubscriptVar' (GeneratedExp c (Bitcode.VariableCtor v) _) index init loc = codeGenExpAssignToSubscriptVar'' v c index init loc
codeGenExpAssignToSubscriptVar' _ _ init _ = return init

codeGenExpAssignToSubscriptVar :: Ast.VarSubscriptContent -> Ast.Exp -> CodeGenContext GeneratedExp
codeGenExpAssignToSubscriptVar v init = do
    init' <- codeGenExp init
    index <- codeGenExp (Ast.varSubscriptIdx v)
    v' <- codeGenExp (Ast.varSubscriptLhs v)
    codeGenExpAssignToSubscriptVar' v' index init' (Ast.varSubscriptLocation v)

codeGenExpAssign' :: Ast.Var -> Ast.Exp -> CodeGenContext GeneratedExp
codeGenExpAssign' (Ast.VarSimple    v) e = codeGenExpAssignToSimpleVar (Ast.varName v) e
codeGenExpAssign' (Ast.VarField     v) e = codeGenExpAssignToFieldVar v e
codeGenExpAssign' (Ast.VarSubscript v) e = codeGenExpAssignToSubscriptVar v e

codeGenExpAssign :: Ast.ExpAssignContent -> CodeGenContext GeneratedExp
codeGenExpAssign e = codeGenExpAssign' (Ast.expAssignLhs e) (Ast.expAssignRhs e)

codeGenExpKwArg :: Ast.ExpKwArgContent -> CodeGenContext GeneratedExp
codeGenExpKwArg = codeGenExp . Ast.expKwArgValue

codeGenExpBinop :: Ast.ExpBinopContent -> CodeGenContext GeneratedExp
codeGenExpBinop expBinop = do
    lhs <- codeGenExp (Ast.expBinopLeft expBinop)
    rhs <- codeGenExp (Ast.expBinopRight expBinop)
    let location' = Ast.expBinopLocation expBinop
    let binopLhs = generatedValue lhs
    let binopRhs = generatedValue rhs
    let lhsActualType = inferredActualType lhs
    let rhsActualType = inferredActualType rhs
    let astOp = Ast.expBinopOperator expBinop
    let actualType = ActualType.inferFromBinop lhsActualType rhsActualType astOp
    let binopOutput = Bitcode.TmpVariableCtor $ Bitcode.TmpVariable (ActualType.toFqn actualType) location'
    let binop = Bitcode.BinopContent binopOutput binopLhs binopRhs (astOperatorToBinopOp astOp)
    let content' = Bitcode.Binop binop
    let instruction = Bitcode.Instruction location' content'
    let cfg = Cfg.Normal (Cfg.atom (Cfg.Node instruction))
    let binopCfg = generatedCfg lhs `Cfg.concat` generatedCfg rhs
    return $ GeneratedExp (binopCfg `Cfg.concat` cfg) (Bitcode.VariableCtor binopOutput) actualType

-- | Lower an AST-level operator to the bitcode-level 'Bitcode.BinopOp'
-- coarse tag. Only the equality variants are preserved; everything else
-- collapses to 'Bitcode.OtherOp' so downstream analyses ( e.g.
-- @kb_comparison@ ) can filter on the equality shape without carrying
-- the full 'Ast.Operator' surface into the bitcode layer.
astOperatorToBinopOp :: Ast.Operator -> Bitcode.BinopOp
astOperatorToBinopOp Ast.EQ  = Bitcode.EqOp
astOperatorToBinopOp Ast.NEQ = Bitcode.NeqOp
astOperatorToBinopOp _       = Bitcode.OtherOp

-- |
-- two things happen during codegen of lambdas:
--
-- * the lambda callable is inserted to the state
--
-- * a temporary variable is created, indicative of the lambda (by location)
--
codeGenExpLambda :: Ast.ExpLambdaContent -> CodeGenContext GeneratedExp
codeGenExpLambda e = do { insertLambdaCallable e; codeGenExpLambda' e }

beginScope = do { ctx <- get; put $ ctx { symbolTable = SymbolTable.beginScope (symbolTable ctx) } }
endScope   = do { ctx <- get; put $ ctx { symbolTable = SymbolTable.endScope   (symbolTable ctx) } }

-- The parser-inserted sentinel whose presence in a callable body means
-- `ensureCallableBodyEndsWithReturn` ( in TsParserActions.hs ) appended
-- a two-stmt trailer -- a marker call to
-- `<dhscanner-instrumentation>[fallthrough_return_null]` followed by
-- `return null` -- to a body that did not already end with an explicit
-- `StmtReturn`. By construction this name is syntactically illegal as a
-- user-written TS identifier ( `<>` and `[]` inside the ident ), so
-- finding the call anywhere in the top-level stmts is a sufficient
-- signal to shave two stmts off the source-level count.
fallthroughReturnNullMarkerCallee :: String
fallthroughReturnNullMarkerCallee = "<dhscanner-instrumentation>[fallthrough_return_null]"

-- Recognises the parser's fall-through marker at a single stmt : an
-- `Ast.StmtExp` wrapping an `Ast.ExpCall` whose callee is a bare
-- `Ast.VarSimple` naming the marker.
isFallthroughReturnNullMarkerStmt :: Ast.Stmt -> Bool
isFallthroughReturnNullMarkerStmt (Ast.StmtExp (Ast.ExpCall (Ast.ExpCallContent (Ast.ExpVar (Ast.ExpVarContent (Ast.VarSimple (Ast.VarSimpleContent v)))) _ _))) = Token.content (Token.getVarNameToken v) == fallthroughReturnNullMarkerCallee
isFallthroughReturnNullMarkerStmt _ = False

-- Body-level predicate : did the parser's fall-through instrumentation
-- kick in for this callable ? The check is position-agnostic on purpose
-- ( see `fallthroughReturnNullMarkerCallee` ) -- any occurrence in the
-- top-level stmts is enough.
returnNullWasInstrumentedForFallthrough :: [ Ast.Stmt ] -> Bool
returnNullWasInstrumentedForFallthrough = any isFallthroughReturnNullMarkerStmt

-- Source-level `[ Ast.Stmt ]` length, corrected for the parser's
-- fall-through instrumentation. When the marker is present the raw
-- length is inflated by exactly 2 ( marker + `return null` ) ; we
-- subtract those here so the count stored in bitcode / kbgen / KB
-- facts stays source-truthful. Callable bodies receive at most one
-- such trailer ( `ensureCallableBodyEndsWithReturn` runs exactly once
-- per callable during parsing ), so the subtraction is bounded.
numSourceStmts :: [ Ast.Stmt ] -> Word
numSourceStmts [] = 0
numSourceStmts [_] = 1
numSourceStmts stmts = numSourceStmts' (returnNullWasInstrumentedForFallthrough stmts) (length stmts)

numSourceStmts' :: Bool -> Int -> Word
numSourceStmts' True  n = fromIntegral (n - 2)
numSourceStmts' False n = fromIntegral n

handleLambda :: Ast.ExpLambdaContent -> CodeGenContext Callable
handleLambda lambda = do
    paramDeclsCfg <- codeGenLambdaParams lambda
    lambdaBodyCfg <- codeGenStmts (Ast.expLambdaBody lambda)
    let srcLen = numSourceStmts (Ast.expLambdaBody lambda)
    return $ lambdaToCallable
        (Cfg.concat paramDeclsCfg lambdaBodyCfg)
        (Ast.expLambdaLocation lambda)
        srcLen

handleLambdaCallable :: Ast.ExpLambdaContent -> CodeGenContext Callable
handleLambdaCallable lambda = do { beginScope; callable <- handleLambda lambda; endScope; return callable }

-- | insert the callable to the state
insertLambdaCallable :: Ast.ExpLambdaContent -> CodeGenContext ()
insertLambdaCallable expLambda = do { c <- handleLambdaCallable expLambda; ctx <- get; put $ ctx { callables = c:(callables ctx) } }

codeGenLambdaParams :: Ast.ExpLambdaContent -> CodeGenContext Cfg
codeGenLambdaParams = codeGenParams . Ast.expLambdaParams

codeGenParams :: [ Ast.Param ] -> CodeGenContext Cfg
codeGenParams params' = do
    cfgs <- codeGenParams' 0 params'
    return $ foldl' Cfg.concat Cfg.Empty cfgs

codeGenParams' :: Word -> [ Ast.Param ] -> CodeGenContext [ Cfg ]
codeGenParams' _ [] = return []
codeGenParams' i (p:ps) = do { cfg <- codeGenParam i p; cfgs <- codeGenParams' (i+1) ps; return (cfg:cfgs) }

-- | The nominal type is represented as an Ast.Var
codeGenParam' :: Word -> Token.ParamName -> Ast.Var -> CodeGenContext Cfg
codeGenParam' i name nominalType = do
    ctx <- get
    nominalType' <- codeGenVar nominalType
    let actualType = inferredActualType nominalType'
    let fqn = ActualType.toFqn actualType
    let location' = Token.getParamNameLocation name
    let paramVar = Bitcode.ParamVariable fqn i name
    let v = Bitcode.ParamVariableCtor paramVar
    let paramDecl = Bitcode.ParamDecl $ Bitcode.ParamDeclContent paramVar
    let instruction = Bitcode.Instruction location' paramDecl
    put (ctx { symbolTable = SymbolTable.insertParam name v actualType (symbolTable ctx) })
    return (Cfg.Normal (Cfg.atom (Cfg.Node instruction)))

mkParamDeclCfg :: Word -> Ast.Param -> Fqn -> Token.ParamName -> Cfg
mkParamDeclCfg i p fqn name = let
    paramVar = Bitcode.ParamVariable fqn i (Ast.paramName p)
    paramDecl = Bitcode.ParamDecl (Bitcode.ParamDeclContent paramVar)
    instruction = Bitcode.Instruction (Token.getParamNameLocation name) paramDecl
    in Cfg.Normal (Cfg.atom (Cfg.Node instruction))

existingHostingClassOrAny :: SymbolTable -> ActualType
existingHostingClassOrAny = SymbolTable.lookupSelfOrAny

lookupHostingClassActualType :: CodeGenContext ActualType
lookupHostingClassActualType = existingHostingClassOrAny . symbolTable <$> get

-- no need to insert self to symbol table
-- it is already there, unless something unexpected happened
-- even if something bad did happen, inserting self now will /not/ fix it ...
-- so, we just create the param decl instruction, and return the cfg
-- see 'mkParamDeclCfg'
codeGenParamSelf :: Word -> Ast.Param -> CodeGenContext Cfg
codeGenParamSelf i p = do
    actualType <- lookupHostingClassActualType
    return $ mkParamDeclCfg i p (ActualType.toFqn actualType) (Ast.paramName p)

codeGenParamNonSelf :: Word -> Ast.Param -> CodeGenContext Cfg
codeGenParamNonSelf i p = do
    let actualType = ActualType.UntypedNamedParam (Ast.paramName p)
    insertParamWithActualTypeToSymbolTable (Ast.paramName p) actualType i
    return $ mkParamDeclCfg i p (ActualType.toFqn actualType) (Ast.paramName p)

codeGenParam''' :: Bool -> Word -> Ast.Param -> CodeGenContext Cfg
codeGenParam''' True = codeGenParamSelf
codeGenParam''' False = codeGenParamNonSelf

codeGenParam'' :: Word -> Ast.Param -> CodeGenContext Cfg
codeGenParam'' i p = codeGenParam''' (Token.content (Token.getParamNameToken (Ast.paramName p)) == "self") i p

codeGenParam :: Word -> Ast.Param -> CodeGenContext Cfg
codeGenParam paramSerialIdx' param = case Ast.paramNominalType param of
    Nothing -> codeGenParam'' paramSerialIdx' param
    Just nominalType -> codeGenParam' paramSerialIdx' (Ast.paramName param) nominalType

-- |
-- * generate an indicative variable (via location).
--
-- * cfg is just a single nop instruction ...
--
codeGenExpLambda' :: Ast.ExpLambdaContent -> CodeGenContext GeneratedExp
codeGenExpLambda' expLambda = do
    let location' = Ast.expLambdaLocation expLambda
    let actualType = ActualType.Lambda $ ActualType.LambdaContent location'
    let tmpVariable = Bitcode.TmpVariable (ActualType.toFqn actualType) location'
    let variable = Bitcode.TmpVariableCtor tmpVariable
    return $ GeneratedExp Cfg.Empty (Bitcode.VariableCtor variable) actualType

-- | whenever something goes wrong, or simply not supported
-- we can generate a non deterministic expression
nondet :: Token.VarName -> GeneratedExp
nondet varName = let
    location' = Token.getVarNameLocation varName
    rawName = Token.content (Token.getVarNameToken varName)
    actualType = ActualType.ThirdPartyImport (ActualType.ThirdPartyImportContent rawName [] Nothing Nothing)
    unresolvedRefInput = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable (ActualType.toFqn actualType) varName
    output = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable (ActualType.toFqn actualType) varName
    unresolvedRef = Bitcode.UnresolvedRef (Bitcode.UnresolvedRefContent output unresolvedRefInput location')
    instruction = Bitcode.Instruction location' unresolvedRef
    cfg = Cfg.Normal (Cfg.atom (Cfg.Node instruction))
    in GeneratedExp cfg (Bitcode.VariableCtor output) actualType

-- | for some reason, the variable needed to generate an exp
-- does not exist in the symbol table. this is (probably? surely?) an error.
--
-- EDIT: This is NOT an error - it is perfectly fine ...
--
-- best thing we can do is use some universal variable to capture all
-- the missing variables
codeGenExpVarSimpleMissing :: Token.VarName -> GeneratedExp
codeGenExpVarSimpleMissing v = let generatedExp = nondet v in generatedExp { inferredActualType = ActualType.Any }

codeGenVarSimple' :: Token.VarName -> SymbolTable -> GeneratedExp
codeGenVarSimple' v table = case SymbolTable.lookupVar v table of
    Nothing -> codeGenExpVarSimpleMissing v
    Just (v', actualType) -> GeneratedExp Cfg.Empty (Bitcode.VariableCtor v') actualType

codeGenVarSimple :: Ast.VarSimpleContent -> CodeGenState -> GeneratedExp
codeGenVarSimple v ctx = codeGenVarSimple' (Ast.varName v) (symbolTable ctx)

codeGenVarField'' :: Bitcode.Variable -> Cfg -> ActualType -> Token.FieldName -> Location -> CodeGenContext GeneratedExp
codeGenVarField'' v cfgLhs actualType fieldName loc = do
    let actualType' = ActualType.FieldedAccess actualType fieldName
    let outputFqn = ActualType.toFqn actualType'
    let output = Bitcode.TmpVariableCtor (Bitcode.TmpVariable outputFqn loc)
    let fieldReadContent = Bitcode.FieldReadContent output v fieldName
    let fieldRead = Bitcode.FieldRead fieldReadContent
    let fieldReadInstruction = Bitcode.Instruction loc fieldRead
    let cfgFieldRead = Cfg.Normal (Cfg.atom (Cfg.Node fieldReadInstruction))
    let cfg = cfgLhs `Cfg.concat` cfgFieldRead
    return $ GeneratedExp cfg (Bitcode.VariableCtor output) actualType'

codeGenVarField' :: Bitcode.Value -> Cfg -> ActualType -> Token.FieldName -> Location -> CodeGenContext GeneratedExp
codeGenVarField' (Bitcode.VariableCtor v) cfg actualType fieldName loc = codeGenVarField'' v cfg actualType fieldName loc
codeGenVarField' v cfg actualType _ _ = return (GeneratedExp cfg v actualType)

codeGenVarField :: Ast.VarFieldContent -> CodeGenContext GeneratedExp
codeGenVarField v = do
    v' <- codeGenExp (Ast.varFieldLhs v)
    codeGenVarField' (generatedValue v') (generatedCfg v') (inferredActualType v') (Ast.varFieldName v) (Ast.varFieldLocation v)

codeGenVarSubscript'' :: Bitcode.Variable -> Cfg -> ActualType -> Ast.Exp -> Location -> CodeGenContext GeneratedExp
codeGenVarSubscript'' v cfgLhs actualType idx loc = do
    idx' <- codeGenExp idx
    let idxCfg = generatedCfg idx'
    let idxValue = generatedValue idx'
    let outputFqn = ActualType.toFqn actualType
    let output = Bitcode.TmpVariableCtor (Bitcode.TmpVariable outputFqn loc)
    let subscriptReadContent = Bitcode.SubscriptReadContent output v idxValue
    let subscriptRead = Bitcode.SubscriptRead subscriptReadContent
    let subscriptReadInstruction = Bitcode.Instruction loc subscriptRead
    let cfgSubscriptRead = Cfg.Normal (Cfg.atom (Cfg.Node subscriptReadInstruction))
    let cfg = idxCfg `Cfg.concat` cfgLhs `Cfg.concat` cfgSubscriptRead
    return $ GeneratedExp cfg (Bitcode.VariableCtor output) actualType

codeGenVarSubscript' :: Bitcode.Value -> Cfg -> ActualType -> Ast.Exp -> Location -> CodeGenContext GeneratedExp
codeGenVarSubscript' (Bitcode.VariableCtor v) cfg actualType idx loc = codeGenVarSubscript'' v cfg actualType idx loc
codeGenVarSubscript' v cfg actualType _ _ = return (GeneratedExp cfg v actualType)

codeGenVarSubscript :: Ast.VarSubscriptContent -> CodeGenContext GeneratedExp
codeGenVarSubscript v = do
    v' <- codeGenExp (Ast.varSubscriptLhs v)
    codeGenVarSubscript' (generatedValue v') (generatedCfg v') (inferredActualType v') (Ast.varSubscriptLhs v) (Ast.varSubscriptLocation v)

codeGenVar :: Ast.Var -> CodeGenContext GeneratedExp
codeGenVar (Ast.VarSimple    v) = codeGenVarSimple v <$> get
codeGenVar (Ast.VarField     v) = codeGenVarField v
codeGenVar (Ast.VarSubscript v) = codeGenVarSubscript v

-- | dispatch codegen (exp) var handlers
codeGenExpVar :: Ast.ExpVarContent -> CodeGenContext GeneratedExp
codeGenExpVar (Ast.ExpVarContent v) = codeGenVar v

-- | code gen exps ( plural )
codeGenExps :: [ Ast.Exp ] -> CodeGenContext [ GeneratedExp ]
codeGenExps = mapM codeGenExp

-- | normal case currently ignores overloading
getReturnActualType'' :: Callee -> ActualType
getReturnActualType'' (GeneratedExp _ _ (ActualType.Function f)) = ActualType.returnType f
getReturnActualType'' (GeneratedExp _ _ (ActualType.ThirdPartyImport i)) = ActualType.ThirdPartyImport i
getReturnActualType'' _ = ActualType.Any

-- | separate javascript `require` calls
getReturnActualType :: Callee -> Args -> ActualType
getReturnActualType callee _ = getReturnActualType'' callee

codeGenExpCallMethod :: Bitcode.Variable -> Cfg -> [ GeneratedExp ] -> Location -> ActualType -> GeneratedExp
codeGenExpCallMethod v cfg args loc actualType = let
    output = Bitcode.TmpVariableCtor (Bitcode.TmpVariable (ActualType.toFqn actualType) loc)
    args' = Data.List.map generatedValue args
    callInstruction = Bitcode.Instruction loc (Bitcode.Call (Bitcode.CallContent output v args' loc))
    prepareCall = foldl' Cfg.concat cfg (Data.List.map generatedCfg args)
    actualCall = Cfg.Normal (Cfg.atom (Cfg.Node callInstruction))
    in GeneratedExp (prepareCall `Cfg.concat` actualCall) (Bitcode.VariableCtor output) actualType

codeGenExpCallMethod1 :: Bitcode.TmpVariable -> Token.FieldName -> ActualType.ClassInstanceContent -> Cfg -> [ GeneratedExp ] -> Location -> GeneratedExp
codeGenExpCallMethod1 v (Token.FieldName (Token.Named method _)) (ActualType.ClassInstanceContent _ c) cfg args loc = let
    actualType = ActualType.CallMethodOfClass loc method c
    modifiedFqn = ActualType.toFqn actualType
    in codeGenExpCallMethod (Bitcode.TmpVariableCtor (v { Bitcode.tmpVariableFqn = modifiedFqn })) cfg args loc actualType

codeGenExpCallMethod2 :: Bitcode.SrcVariable -> Token.FieldName -> ActualType.ClassInstanceContent -> Cfg -> [ GeneratedExp ] -> Location -> GeneratedExp
codeGenExpCallMethod2 v (Token.FieldName (Token.Named method _)) (ActualType.ClassInstanceContent _ c) cfg args loc = let
    actualType = ActualType.CallMethodOfClass loc method c
    modifiedFqn = ActualType.toFqn actualType
    in codeGenExpCallMethod (Bitcode.SrcVariableCtor (v { Bitcode.srcVariableFqn = modifiedFqn })) cfg args loc actualType

codeGenExpCallMethod3 :: Bitcode.ParamVariable -> Token.FieldName -> ActualType.ClassInstanceContent -> Cfg -> [ GeneratedExp ] -> Location -> GeneratedExp
codeGenExpCallMethod3 v (Token.FieldName (Token.Named method _)) (ActualType.ClassInstanceContent _ c) cfg args loc = let
    actualType = ActualType.CallMethodOfClass loc method c
    modifiedFqn = ActualType.toFqn actualType
    in codeGenExpCallMethod (Bitcode.ParamVariableCtor (v { Bitcode.paramVariableFqn = modifiedFqn })) cfg args loc actualType

codeGenExpCallClassFielded :: Bitcode.Variable -> Token.FieldName -> ActualType.ClassInstanceContent -> Cfg -> [ GeneratedExp ] -> Location -> GeneratedExp
codeGenExpCallClassFielded (Bitcode.TmpVariableCtor   v) = codeGenExpCallMethod1 v
codeGenExpCallClassFielded (Bitcode.SrcVariableCtor   v) = codeGenExpCallMethod2 v
codeGenExpCallClassFielded (Bitcode.ParamVariableCtor v) = codeGenExpCallMethod3 v

codeGenExpCallFirstPartyDirImportFielded :: Bitcode.Variable -> Token.FieldName -> ActualType.FirstPartyDirImportContent -> Cfg -> [ GeneratedExp ] -> Location -> GeneratedExp
codeGenExpCallFirstPartyDirImportFielded (Bitcode.TmpVariableCtor   v) = codeGenExpCallFunc1 v
codeGenExpCallFirstPartyDirImportFielded (Bitcode.SrcVariableCtor   v) = codeGenExpCallFunc2 v
codeGenExpCallFirstPartyDirImportFielded (Bitcode.ParamVariableCtor v) = codeGenExpCallFunc3 v

codeGenExpCallFunc1 :: Bitcode.TmpVariable -> Token.FieldName -> ActualType.FirstPartyDirImportContent -> Cfg -> [ GeneratedExp ] -> Location -> GeneratedExp
codeGenExpCallFunc1 v (Token.FieldName (Token.Named func _)) (ActualType.FirstPartyDirImportContent d _) cfg args loc = let
    actualType = ActualType.CallFuncFromImportedDir loc func d
    modifiedFqn = ActualType.toFqn actualType
    in codeGenExpCallMethod (Bitcode.TmpVariableCtor (v { Bitcode.tmpVariableFqn = modifiedFqn })) cfg args loc actualType

codeGenExpCallFunc2 :: Bitcode.SrcVariable -> Token.FieldName -> ActualType.FirstPartyDirImportContent -> Cfg -> [ GeneratedExp ] -> Location -> GeneratedExp
codeGenExpCallFunc2 v (Token.FieldName (Token.Named func _)) (ActualType.FirstPartyDirImportContent d _) cfg args loc = let
    actualType = ActualType.CallFuncFromImportedDir loc func d
    modifiedFqn = ActualType.toFqn actualType
    in codeGenExpCallMethod (Bitcode.SrcVariableCtor (v { Bitcode.srcVariableFqn = modifiedFqn })) cfg args loc actualType

codeGenExpCallFunc3 :: Bitcode.ParamVariable -> Token.FieldName -> ActualType.FirstPartyDirImportContent -> Cfg -> [ GeneratedExp ] -> Location -> GeneratedExp
codeGenExpCallFunc3 v (Token.FieldName (Token.Named func _)) (ActualType.FirstPartyDirImportContent d _) cfg args loc = let
    actualType = ActualType.CallFuncFromImportedDir loc func d
    modifiedFqn = ActualType.toFqn actualType
    in codeGenExpCallMethod (Bitcode.ParamVariableCtor (v { Bitcode.paramVariableFqn = modifiedFqn })) cfg args loc actualType

codeGenExpCallUntypedNamedParamFielded :: Bitcode.Variable -> Token.FieldName -> Token.ParamName -> Cfg -> [ GeneratedExp ] -> Location -> GeneratedExp
codeGenExpCallUntypedNamedParamFielded (Bitcode.TmpVariableCtor   v) = codeGenExpCallMethod1' v
codeGenExpCallUntypedNamedParamFielded (Bitcode.SrcVariableCtor   v) = codeGenExpCallMethod2' v
codeGenExpCallUntypedNamedParamFielded (Bitcode.ParamVariableCtor v) = codeGenExpCallMethod3' v

codeGenExpCallMethod1' :: Bitcode.TmpVariable -> Token.FieldName -> Token.ParamName -> Cfg -> [ GeneratedExp ] -> Location -> GeneratedExp
codeGenExpCallMethod1' v (Token.FieldName (Token.Named method _)) p cfg args loc = let
    actualType = ActualType.CallMethodOfUntypedNamedParam loc method p
    modifiedFqn = ActualType.toFqn actualType
    in codeGenExpCallMethod (Bitcode.TmpVariableCtor (v { Bitcode.tmpVariableFqn = modifiedFqn })) cfg args loc actualType

codeGenExpCallMethod2' :: Bitcode.SrcVariable -> Token.FieldName -> Token.ParamName -> Cfg -> [ GeneratedExp ] -> Location -> GeneratedExp
codeGenExpCallMethod2' v (Token.FieldName (Token.Named method _)) p cfg args loc = let
    actualType = ActualType.CallMethodOfUntypedNamedParam loc method p
    modifiedFqn = ActualType.toFqn actualType
    in codeGenExpCallMethod (Bitcode.SrcVariableCtor (v { Bitcode.srcVariableFqn = modifiedFqn })) cfg args loc actualType

codeGenExpCallMethod3' :: Bitcode.ParamVariable -> Token.FieldName -> Token.ParamName -> Cfg -> [ GeneratedExp ] -> Location -> GeneratedExp
codeGenExpCallMethod3' v (Token.FieldName (Token.Named method _)) p cfg args loc = let
    actualType = ActualType.CallMethodOfUntypedNamedParam loc method p
    modifiedFqn = ActualType.toFqn actualType
    in codeGenExpCallMethod (Bitcode.ParamVariableCtor (v { Bitcode.paramVariableFqn = modifiedFqn })) cfg args loc actualType

codeGenExpCallDefault :: ActualType -> Bitcode.Variable -> Cfg -> [ GeneratedExp ] -> Location -> GeneratedExp
codeGenExpCallDefault calleeActualType v cfg args loc = let
    actualType = getReturnActualType' calleeActualType
    output = Bitcode.TmpVariableCtor (Bitcode.TmpVariable (ActualType.toFqn actualType) loc)
    args' = Data.List.map generatedValue args
    callInstruction = Bitcode.Instruction loc (Bitcode.Call (Bitcode.CallContent output v args' loc))
    actualCall = Cfg.Normal (Cfg.atom (Cfg.Node callInstruction))
    prepareCall = foldl' Cfg.concat cfg (Data.List.map generatedCfg args)
    in GeneratedExp (prepareCall `Cfg.concat` actualCall) (Bitcode.VariableCtor output) actualType

codeGenExpCallFielded :: ActualType -> Token.FieldName -> Bitcode.Variable -> Cfg -> [ GeneratedExp ] -> Location -> GeneratedExp
codeGenExpCallFielded (ActualType.ClassInstance c) f v = codeGenExpCallClassFielded v f c
codeGenExpCallFielded (ActualType.UntypedNamedParam p) f v = codeGenExpCallUntypedNamedParamFielded v f p
codeGenExpCallFielded (ActualType.FirstPartyDirImport d) f v = codeGenExpCallFirstPartyDirImportFielded v f d
codeGenExpCallFielded t f v = codeGenExpCallDefault (ActualType.FieldedAccess t f) v

setNewFqnToTmpVariable :: Bitcode.TmpVariable -> Fqn -> Bitcode.Variable
setNewFqnToTmpVariable v newFqn = Bitcode.TmpVariableCtor (v { Bitcode.tmpVariableFqn = newFqn })

setNewFqnToSrcVariable :: Bitcode.SrcVariable -> Fqn -> Bitcode.Variable
setNewFqnToSrcVariable v newFqn = Bitcode.SrcVariableCtor (v { Bitcode.srcVariableFqn = newFqn })

setNewFqnToParamVariable :: Bitcode.ParamVariable -> Fqn -> Bitcode.Variable
setNewFqnToParamVariable v newFqn = Bitcode.ParamVariableCtor (v { Bitcode.paramVariableFqn = newFqn })

setNewFqnToVariable :: Bitcode.Variable -> Fqn -> Bitcode.Variable
setNewFqnToVariable (Bitcode.TmpVariableCtor   v) = setNewFqnToTmpVariable   v
setNewFqnToVariable (Bitcode.SrcVariableCtor   v) = setNewFqnToSrcVariable   v
setNewFqnToVariable (Bitcode.ParamVariableCtor v) = setNewFqnToParamVariable v

codeGenExpCall1stPartyFunc :: FilePath -> String -> Bitcode.Variable -> Cfg -> [ GeneratedExp ] -> Location -> GeneratedExp
codeGenExpCall1stPartyFunc f func v cfg args loc = let
    actualType = ActualType.CallFuncFromImportedFile loc func f
    newFqn = ActualType.toFqn actualType
    v' = setNewFqnToVariable v newFqn
    in codeGenExpCallDefault actualType v' cfg args loc

codeGenExpCall'' :: ActualType -> Bitcode.Variable -> Cfg -> [ GeneratedExp ] -> Location -> GeneratedExp
codeGenExpCall'' (ActualType.FieldedAccess t f) = codeGenExpCallFielded t f
codeGenExpCall'' (ActualType.FirstPartyImport (ActualType.FirstPartyImportContent f (Just i))) = codeGenExpCall1stPartyFunc f i
codeGenExpCall'' calleeActualType = codeGenExpCallDefault calleeActualType

codeGenExpCall' :: Bitcode.Value -> Cfg -> ActualType -> [ GeneratedExp ] -> Location -> GeneratedExp
codeGenExpCall' (Bitcode.VariableCtor v) cfg actualType args loc = codeGenExpCall'' actualType v cfg args loc
codeGenExpCall' _ _ _ _ loc = nullGeneratedExp loc

codeGenExpCall :: Ast.ExpCallContent -> CodeGenContext GeneratedExp
codeGenExpCall call = do
    callee <- codeGenExp (Ast.callee call)
    args <- codeGenExps (Ast.args call)
    return $ codeGenExpCall' (generatedValue callee) (generatedCfg callee) (inferredActualType callee) args (Ast.expCallLocation call)

nullBitcodeValue :: Location -> Bitcode.Value
nullBitcodeValue = Bitcode.ConstValueCtor . Bitcode.ConstNullValue . Token.ConstNull

nullGeneratedExp :: Location -> GeneratedExp
nullGeneratedExp loc = GeneratedExp Cfg.Empty (nullBitcodeValue loc) ActualType.Any

getReturnActualType' :: ActualType -> ActualType
getReturnActualType' (ActualType.Function f) = ActualType.returnType f
getReturnActualType' actualType = actualType

-- | during stmt assign, it could be the case that
-- a simple variable is also defined. if so, we need
-- to insert it into the symbol table.
createBitcodeVarIfNeeded :: Ast.VarSimpleContent -> CodeGenContext ()
createBitcodeVarIfNeeded v = do { ctx <- get; put $ let
    bitcodeVar = Bitcode.SrcVariableCtor (Bitcode.SrcVariable Fqn.NativeTypeInt (Ast.varName v))
    bitcodeVarExists = SymbolTable.varExists (Ast.varName v) (symbolTable ctx)
    symbolTable' = SymbolTable.insertVar (Ast.varName v) bitcodeVar ActualType.Any (symbolTable ctx)
    in case bitcodeVarExists of { True -> ctx; False -> ctx { symbolTable = symbolTable' } }
}

-- |
--
-- * dynamic languages often have variable declarations
-- "hide" in plain assignment syntax - this means that
-- assignment scanned according to their "ast order"
-- will insert the assigned variable to the symbol table
-- (if it hasn't been inserted before)
--
-- * this is /very/ similar to `codeGenStmtVardecInit`
--
codeGenStmtAssignToSimpleVar :: Token.VarName -> Ast.Exp -> CodeGenContext Cfg
codeGenStmtAssignToSimpleVar varName init = do
    init' <- codeGenExp init
    ctx <- get;
    let initCfg = generatedCfg init'
    let initVariable = generatedValue init'
    let actualType = inferredActualType init'
    let fqn = ActualType.toFqn actualType
    let location' = Token.getVarNameLocation varName
    let existingVar = SymbolTable.lookupVar varName (symbolTable ctx)
    let newVariable = Bitcode.SrcVariableCtor $ Bitcode.SrcVariable fqn varName
    let symbolTable' = SymbolTable.insertVar varName newVariable actualType (symbolTable ctx)
    let updateSymbolTable = ctx { symbolTable = symbolTable' }
    let dontChangeCtx = ctx
    let actualVar = case existingVar of { Nothing -> newVariable; Just (v, _) -> v }
    put $ case existingVar of { Nothing -> updateSymbolTable; _ -> dontChangeCtx }
    return $ initCfg `Cfg.concat` (createAssignCfg location' actualVar initVariable)

codeGenStmtAssignToFieldVar :: Ast.VarFieldContent -> Ast.Exp -> CodeGenContext Cfg
codeGenStmtAssignToFieldVar var exp = do
    generatedExp <- codeGenExpAssignToFieldVar var exp
    return (generatedCfg generatedExp)

codeGenStmtAssignToSubscriptVar :: Ast.VarSubscriptContent -> Ast.Exp -> CodeGenContext Cfg
codeGenStmtAssignToSubscriptVar var exp = do
    generatedExp <- codeGenExpAssignToSubscriptVar var exp
    return (generatedCfg generatedExp)

codeGenStmtAssign' :: Ast.Var -> Ast.Exp -> CodeGenContext Cfg
codeGenStmtAssign' (Ast.VarSimple    v) e = codeGenStmtAssignToSimpleVar (Ast.varName v) e
codeGenStmtAssign' (Ast.VarField     v) e = codeGenStmtAssignToFieldVar v e
codeGenStmtAssign' (Ast.VarSubscript v) e = codeGenStmtAssignToSubscriptVar v e

-- | dynamic languages often have variable declarations
-- "hide" in plain assignment syntax - this means that
-- assignment scanned according to their "ast order"
-- will insert the assigned variable to the symbol table
-- (if it hasn't been inserted before)
codeGenStmtAssign :: Ast.StmtAssignContent -> CodeGenContext Cfg
codeGenStmtAssign s = codeGenStmtAssign' (Ast.stmtAssignLhs s) (Ast.stmtAssignRhs s)

insertParamWithActualTypeToSymbolTable :: Token.ParamName -> ActualType -> Word -> CodeGenContext ()
insertParamWithActualTypeToSymbolTable name actualType i = do
    ctx <- get
    let fqn = ActualType.toFqn actualType
    let v = Bitcode.ParamVariableCtor (Bitcode.ParamVariable fqn i name)
    let symbolTable' = SymbolTable.insertParam name v actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }

insertVarWithActualTypeToSymbolTable :: Token.VarName -> ActualType -> CodeGenContext Bitcode.Variable
insertVarWithActualTypeToSymbolTable name actualType = do
    ctx <- get
    let fqn = ActualType.toFqn actualType
    let v = Bitcode.SrcVariableCtor (Bitcode.SrcVariable fqn name)
    let symbolTable' = SymbolTable.insertVar name v actualType (symbolTable ctx)
    put $ ctx { symbolTable = symbolTable' }
    return v

insertVarNoNominalTypeToSymbolTable :: Token.VarName -> CodeGenContext ()
insertVarNoNominalTypeToSymbolTable name = void (insertVarWithActualTypeToSymbolTable name ActualType.Any)

codeGenStmtVardecNoInitNoNominalType :: Token.VarName -> CodeGenContext Cfg
codeGenStmtVardecNoInitNoNominalType v = do { insertVarNoNominalTypeToSymbolTable v; return Cfg.Empty }

insertVarToSymbolTable :: Token.VarName -> Ast.Var -> CodeGenContext ()
insertVarToSymbolTable name t = void (insertVarWithActualTypeToSymbolTable name . inferredActualType =<< codeGenVar t)

codeGenStmtVardecNoInit' :: Token.VarName -> Ast.Var -> CodeGenContext Cfg
codeGenStmtVardecNoInit' v t = do { insertVarToSymbolTable v t; return Cfg.Empty }

codeGenStmtVardecNoInit :: Ast.StmtVardecContent -> CodeGenContext Cfg
codeGenStmtVardecNoInit v = case Ast.stmtVardecNominalType v of
    Nothing -> codeGenStmtVardecNoInitNoNominalType (Ast.stmtVardecName v)
    Just nominalType -> codeGenStmtVardecNoInit' (Ast.stmtVardecName v) nominalType

codeGenStmtVardecInit'' :: Token.VarName -> Cfg -> Bitcode.Value -> ActualType -> CodeGenContext Cfg
codeGenStmtVardecInit'' name cfg init actualType = do
    v <- insertVarWithActualTypeToSymbolTable name actualType
    return $ cfg `Cfg.concat` createAssignCfg (Token.getVarNameLocation name) v init

codeGenStmtVardecInit' :: Token.VarName -> GeneratedExp -> CodeGenContext Cfg
codeGenStmtVardecInit' name g = codeGenStmtVardecInit'' name (generatedCfg g) (generatedValue g) (inferredActualType g)

codeGenStmtVardecInit :: Token.VarName -> Ast.Exp -> CodeGenContext Cfg
codeGenStmtVardecInit name init = codeGenStmtVardecInit' name =<< codeGenExp init

codeGenStmtVardec :: Ast.StmtVardecContent -> CodeGenContext Cfg
codeGenStmtVardec d = case Ast.stmtVardecInitValue d of
    Nothing -> codeGenStmtVardecNoInit d
    Just init -> codeGenStmtVardecInit (Ast.stmtVardecName d) init

-- | used by codeGenStmtVardecInit''
createAssignCfg :: Location -> Bitcode.Variable -> Bitcode.Value -> Cfg
createAssignCfg loc srcVariable initValue = let
    assignContent = Bitcode.AssignContent srcVariable initValue
    assign = Bitcode.Assign assignContent
    instruction = Bitcode.Instruction loc assign
    in Cfg.Normal (Cfg.atom (Cfg.Node instruction))

codeGenExpInt :: Ast.ExpIntContent -> GeneratedExp
codeGenExpInt (Ast.ExpIntContent constInt) = let
    constValue = Bitcode.ConstValueCtor (Bitcode.ConstIntValue constInt)
    actualType = ActualType.NativeTypeConstInt (Token.constIntValue constInt)
    in GeneratedExp Cfg.Empty constValue actualType

codeGenExpStr :: Ast.ExpStrContent -> GeneratedExp
codeGenExpStr (Ast.ExpStrContent constStr) = let
    constValue = Bitcode.ConstValueCtor (Bitcode.ConstStrValue constStr)
    actualType = ActualType.NativeTypeConstStr (Token.constStrValue constStr)
    in GeneratedExp Cfg.Empty constValue actualType

codeGenExpBool :: Ast.ExpBoolContent -> GeneratedExp
codeGenExpBool (Ast.ExpBoolContent constBool) = let
    constValue = Bitcode.ConstValueCtor (Bitcode.ConstBoolValue constBool)
    actualType = ActualType.NativeTypeConstBool (Token.constBoolValue constBool)
    in GeneratedExp Cfg.Empty constValue actualType

codeGenExpNull :: Ast.ExpNullContent -> GeneratedExp
codeGenExpNull (Ast.ExpNullContent constNull) = let
    constValue = Bitcode.ConstValueCtor (Bitcode.ConstNullValue constNull)
    actualType = ActualType.NativeTypeConstNull
    in GeneratedExp Cfg.Empty constValue actualType

-- minor non interesting helper functions here

scriptToCallable :: FilePath -> Cfg -> Callable
scriptToCallable = (Callable.Script .) . Callable.ScriptContent

decFuncToCallable :: Ast.StmtFuncContent -> Cfg -> [ Callable.Annotation ] -> Callable
decFuncToCallable stmtFunc cfg annotations = let
    srcLen = numSourceStmts (Ast.stmtFuncBody stmtFunc)
    content' = Callable.FunctionContent (Ast.stmtFuncName stmtFunc) cfg annotations (Ast.stmtFuncLocation stmtFunc) srcLen
    in Callable.Function content'

fqnify :: SymbolTable -> [ Token.SuperName ] -> [ Callable.HostingClassSuper ]
fqnify = Data.List.map . fqnify'

fqnify' :: SymbolTable -> Token.SuperName -> Callable.HostingClassSuper
fqnify' symbolTable' superName = let
    actualType = SymbolTable.lookupSuperType superName symbolTable'
    in case actualType of
        Nothing -> Callable.HostingClassSuper superName Nothing
        Just actualType' -> Callable.HostingClassSuper superName (Just (toFqn actualType'))

stmtMethodToCallable :: Ast.StmtMethodContent -> SymbolTable -> Cfg -> Callable
stmtMethodToCallable stmtMethod symbolTable' cfg = let
    hostingClassName' = Ast.hostingClassName stmtMethod
    hostingClassSupers' = fqnify symbolTable' (Ast.hostingClassSupers stmtMethod)
    stmtMethodName' = Ast.stmtMethodName stmtMethod
    location' = Ast.stmtMethodLocation stmtMethod
    srcLen = numSourceStmts (Ast.stmtMethodBody stmtMethod)
    content' = Callable.MethodContent stmtMethodName' hostingClassName' hostingClassSupers' cfg location' srcLen
    in Callable.Method content'

-- | Lower a lambda AST node to a bitcode 'Callable.Lambda'.
--
-- Unlike 'decFuncToCallable' \/ 'stmtMethodToCallable', this helper does not
-- receive the AST node itself \-- only the already-lowered 'Cfg' plus the
-- lambda's source location \-- so we require the caller to pass in the
-- source-level @[ Ast.Stmt ]@ body length explicitly.
lambdaToCallable :: Cfg -> Location -> Word -> Callable
lambdaToCallable cfg location' srcLen = let
    content' = Callable.LambdaContent cfg location' srcLen
    in Callable.Lambda content'
