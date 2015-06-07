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
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map
import qualified TypeCheck as TC

import qualified Syntax as S
import Codegen
import JIT


-------------------------------------------------------------------------------
-- Code generation
-------------------------------------------------------------------------------

toSig :: (String, S.Ty) -> [(AST.Type, AST.Name)]
toSig (name, t) = [((toType t), AST.Name name)]

genFun :: Vars -> S.Expr -> ([(AST.Type, AST.Name)], [BasicBlock])
genFun v (S.Fun name argname argtype rettype body) =
  (fnargs, bls)
  where
    fnargs = toSig (argname, argtype)
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      var <- alloca $ toType argtype
      store var (local (AST.Name argname))
      assign argname var
      cgen v body >>= ret

codegenTop :: TC.Ctx -> Vars -> S.ToplevelCmd -> LLVM ()
codegenTop _ globVars (S.Def var_name (S.Fun name argname argtype rettype body)) = do
  define (toType rettype) name fnargs bls
  define (toType rettype) var_name var_args var_bls
  where
    (fnargs, bls) = genFun globVars (S.Fun name argname argtype rettype body)
    (var_args, var_bls) = genFun globVars (S.Fun var_name argname argtype rettype pseudo_body)
    pseudo_body = (S.Apply (S.Var name) (S.Var argname))
codegenTop ctx globVars (S.Def var_name expr) = do
  globalVar ty' var_name args bls
  where
    ty' = TC.typeOf ctx expr
    fname = var_name ++ "_fn"
    (args, bls) = genFun globVars (S.Fun fname "_" S.TBool ty' expr)

codegenTop ctx globVars (S.Expr exp) = do
  define ty "main" [] blks
  where
    ty = toType $ TC.typeOf ctx exp
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen globVars exp >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

_fn :: S.Expr -> (S.Expr, S.Expr, (AST.Operand -> AST.Operand -> Codegen AST.Operand))
_fn (S.Times a b) = (a, b, f_mul)
_fn (S.Plus a b) = (a, b, f_add)
_fn (S.Minus a b) = (a, b, f_sub)
_fn (S.Equal a b) = (a, b, f_eq)
_fn (S.Less a b) = (a, b, f_lt)

cgen :: Vars -> S.Expr -> Codegen AST.Operand
cgen _ (S.Int n) = return $ cons $ C.Int 32 n
cgen _ (S.Bool True) = return $ cons $ C.Int 1 1
cgen _ (S.Bool False) = return $ cons $ C.Int 1 0
cgen globVars (S.Var x) = do
  var <- getvar globVars x
  case var of
    (Right v) -> load v
    (Left (global_name, ty)) -> cgen globVars (S.Apply (S.Var global_name) (S.Bool False))

cgen globVars (S.Apply (S.Var fn) arg) = do
  larg <- cgen globVars arg
  call (externf int (AST.Name fn)) [larg]
cgen globVars (S.If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"

  -- %entry
  ------------------
  cond <- cgen globVars cond
  test <- f_cmp IP.NE (false) cond
  cbr test ifthen ifelse    -- Branch based on the condition

  -- if.then
  ------------------
  setBlock ifthen
  trval <- cgen globVars tr -- Generate code for the true branch
  br ifexit                 -- Branch to the merge block
  ifthen <- getBlock

  -- if.else
  ------------------
  setBlock ifelse
  flval <- cgen globVars fl -- Generate code for the false branch
  br ifexit                 -- Branch to the merge block
  ifelse <- getBlock

  -- if.exit
  ------------------
  setBlock ifexit
  phi int [(trval, ifthen), (flval, ifelse)]

cgen globVars binary =
  let (a, b, fn) = _fn binary
  in do
    ca <- cgen globVars a
    cb <- cgen globVars b
    fn ca cb
-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: GeneratorState -> S.ToplevelCmd -> TC.Ctx -> IO ((Maybe String), (Maybe String), GeneratorState)
codegen mod fns ctx = do
  res <- runJIT oldast
  case res of
    Right (val, code, newast)   -> return $ (val, code, fn newast)
    Left err                    -> putStrLn err >> return (Nothing, Nothing, oldast)
  where
    modn    = codegenTop ctx (vars mod) fns
    oldast  = runLLVM mod modn
    fn ast  = do
      case fns of
        S.Expr _    -> mod -- don't save main() function
        otherwise   -> ast
