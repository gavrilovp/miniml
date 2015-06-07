module Main
       ( main
       ) where

import Control.Monad
import Control.Monad.State
import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import Data.Aeson.Encode.Pretty
import qualified Data.Map as M
import qualified LLVM.General.AST as AST
import qualified Data.ByteString.Lazy.Char8 as B

import Syntax
import Lexer
import Parser (toplevelCmdP)
import TypeCheck
import Emit
import Codegen

import Debug.Trace

initModule :: AST.Module
initModule = emptyModule "MiniML"

parseFile :: String -> IO ToplevelCmd
parseFile file = do
  program  <- readFile file
  putStr program
  case parse toplevelCmdP "" program of
   Left e  -> print e >> fail "parse error"
   Right r -> return r

-- shows type only
exec :: ToplevelCmd -> State Ctx String
exec (Def var e) = do
  ctx <- get
  let ty = typeOf ctx e
  put $ M.insert var ty ctx
  return $ var ++ " : " ++ show ty
exec (Expr e) = do
  ctx <- get
  let ty = typeOf ctx e
  return $ "- : " ++ show ty
    
shell :: IO ()
shell = do
  putStrLn "MiniML. Press Ctrl-C or Ctrl-D to exit."
  shell' (initModule) emptyCtx

shell' :: AST.Module -> CodegenCtx -> IO ()
shell' mod ctx = do
  putStr "MiniML> "
  hFlush stdout
  cmd <- getLine
  case parse toplevelCmdP "" cmd of
    Left e -> print e >> shell' mod ctx
    Right ast -> do
      (evaluated, bytecode, new_context, mod') <- codegen mod ast ctx'
      putStrLn "AST: " >> (B.putStrLn $ encodePretty ast) >> putStrLn (mk_str evaluated bytecode) >> shell' mod' new_context
      where
        ctx' = CodegenCtx { typeContext = local_ctx, varContext = (varContext ctx) }
        (str, local_ctx) = runState (exec ast) (typeContext ctx')
        mk_str evaluated bytecode =
          "\n" ++ llvm_str ++ "\n" ++ cmd ++ "\n" ++ value
          where
            llvm_str =
              case bytecode of
                Just code -> "LLVM bytecode:\n" ++ code
                Nothing   -> "Code generation failed"
            value =
              case evaluated of
                Just val  -> str ++ " = " ++ val
                Nothing   -> str


main :: IO ()
main = do
  shell
