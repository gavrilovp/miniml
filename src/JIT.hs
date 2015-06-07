module JIT where

import Data.Int
import Data.Word
import Foreign.Ptr ( FunPtr, castFunPtr )

import Control.Monad.Except

import LLVM.General.Target
import LLVM.General.Context
import LLVM.General.CodeModel
import LLVM.General.Module as Mod
import qualified LLVM.General.AST as AST

import LLVM.General.PassManager
import LLVM.General.Transforms
import LLVM.General.Analysis

import qualified LLVM.General.ExecutionEngine as EE
import qualified Codegen as C

foreign import ccall "dynamic" haskFun :: FunPtr (IO Int32) -> (IO Int32)

run :: FunPtr a -> IO Int32
run fn = haskFun (castFunPtr fn :: FunPtr (IO Int32))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 0  -- optimization level
    model    = Nothing -- code model ( Default )
    ptrelim  = Nothing -- frame pointer elimination
    fastins  = Nothing -- fast instruction selection

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

runJIT :: C.GeneratorState -> IO (Either String ((Maybe String), (Maybe String), C.GeneratorState))
runJIT mod = do
  withContext $ \context ->
    jit context $ \executionEngine ->
      runExceptT $ withModuleFromAST context (C.modstate mod) $ \m ->
        withPassManager passes $ \pm -> do
          -- Optimization Pass
          runPassManager pm m
          optmod <- moduleAST m
          s <- moduleLLVMAssembly m
          let opt_gen_state = mod { C.modstate = optmod }

          val <- EE.withModuleInEngine executionEngine m $ \ee -> do
            mainfn <- EE.getFunction ee (AST.Name "main")
            case mainfn of
              Just fn -> do
                res <- run fn
                return $ Just res
              Nothing -> return Nothing

          -- Return the optimized module
          case val of
            Just val -> return $ (Just (show val), (Just s), opt_gen_state)
            Nothing  -> return (Nothing, (Just s), opt_gen_state)
