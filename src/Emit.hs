{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import Codegen
import qualified Syntax as S

toType :: S.Ty -> AST.Type
toType S.TInt = int
toType S.TBool = bool

toSig :: (String, S.Ty) -> [(AST.Type, AST.Name)]
toSig (name, t) = [((toType t), AST.Name name)]

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Fun name argname argtype rettype body) = do
  define (toType rettype) name fnargs bls
  where
    fnargs = toSig (argname, argtype)
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      var <- alloca $ toType argtype
      store var (local (AST.Name argname))
      assign argname var
      cgen body >>= ret

codegenTop exp = do
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
cgen (S.Apply fn arg) = do
  larg <- cgen arg
  lfn <- cgen fn
  call lfn [larg]
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

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM mod modn
