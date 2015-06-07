{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.General.Module
import LLVM.General.Context

import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.IntegerPredicate as IP

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map
import qualified TypeCheck as TC

import qualified Syntax as S
import Codegen
import JIT

-------------------------------------------------------------------------------
-- Contexts
-------------------------------------------------------------------------------

type VarCtx = Map.Map String AST.Name
data CodegenCtx
  = CodegenCtx {
    typeContext  :: TC.Ctx
  , varContext   :: VarCtx
  }

varType :: CodegenCtx -> S.Expr -> S.Ty
varType ctx expr = TC.typeOf (typeContext ctx) expr

emptyCtx :: CodegenCtx
emptyCtx = CodegenCtx {
    typeContext = Map.empty :: TC.Ctx
  , varContext  = Map.empty :: VarCtx
}

-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

toType :: S.Ty -> AST.Type
toType S.TInt = int
toType S.TBool = bool

toSig :: (String, S.Ty) -> [(AST.Type, AST.Name)]
toSig (name, t) = [((toType t), AST.Name name)]

genFun :: S.Expr -> ([(AST.Type, AST.Name)], [BasicBlock])
genFun (S.Fun name argname argtype rettype body) =
  (fnargs, bls)
  where
    fnargs = toSig (argname, argtype)
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      var <- alloca $ toType argtype
      store var (local (AST.Name argname))
      assign argname var
      cgen body >>= ret

codegenTop :: S.ToplevelCmd -> LLVM ()
codegenTop (S.Def var_name (S.Fun name argname argtype rettype body)) = do
  define (toType rettype) name fnargs bls
  define (toType rettype) var_name var_args var_bls
  where
    (fnargs, bls) = genFun (S.Fun name argname argtype rettype body)
    (var_args, var_bls) = genFun (S.Fun var_name argname argtype rettype pseudo_body)
    pseudo_body = (S.Apply (S.Var name) (S.Var argname))
codegenTop (S.Def var_name expr) = do
  define ty fname fnargs bls
  globalVar ty var_name (C.Int 32 0)
  where
    ty' = S.TInt -- typeOf expr
    ty = toType $ ty'
    fname = var_name ++ "_fn"
    (fnargs, bls) = genFun (S.Fun fname "_" ty' ty' expr)

codegenTop (S.Expr exp) = do
  define int "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

_fn :: S.Expr -> (S.Expr, S.Expr, (AST.Operand -> AST.Operand -> Codegen AST.Operand))
_fn (S.Times a b) = (a, b, f_mul)
_fn (S.Plus a b) = (a, b, f_add)
_fn (S.Minus a b) = (a, b, f_sub)
_fn (S.Equal a b) = (a, b, f_eq)
_fn (S.Less a b) = (a, b, f_lt)

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.Var x) = getvar x >>= load
cgen (S.Int n) = return $ cons $ C.Int 32 n
cgen (S.Bool True) = return $ cons $ C.Int 1 1
cgen (S.Bool False) = return $ cons $ C.Int 1 0
cgen (S.Apply (S.Var fn) arg) = do
  larg <- cgen arg
  call (externf int (AST.Name fn)) [larg]
cgen (S.If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  -- %entry
  ------------------
  cond <- cgen cond
  test <- f_cmp IP.NE (false) cond
  cbr test ifthen ifelse -- Branch based on the condition

  -- if.then
  ------------------
  setBlock ifthen
  trval <- cgen tr       -- Generate code for the true branch
  br ifexit              -- Branch to the merge block
  ifthen <- getBlock

  -- if.else
  ------------------
  setBlock ifelse
  flval <- cgen fl       -- Generate code for the false branch
  br ifexit              -- Branch to the merge block
  ifelse <- getBlock

  -- if.exit
  ------------------
  setBlock ifexit
  phi int [(trval, ifthen), (flval, ifelse)]

cgen binary =
  let (a, b, fn) = _fn binary
  in do
    ca <- cgen a
    cb <- cgen b
    fn ca cb
-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> S.ToplevelCmd -> CodegenCtx -> IO ((Maybe String), (Maybe String), CodegenCtx, AST.Module)
codegen mod fns ctx = do
  res <- runJIT oldast
  case res of
    Right (val, code, newast)   -> return $ (val, code, ctx, fn newast)
    Left err                    -> putStrLn err >> return (Nothing, Nothing, ctx, oldast)
  where
    modn    = codegenTop fns
    oldast  = runLLVM mod modn
    fn ast  = do
      case fns of
        S.Expr _    -> mod -- don't save main() function
        otherwise   -> ast
