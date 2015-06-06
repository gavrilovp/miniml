module Main
       ( main
       ) where

import Control.Monad
import Control.Monad.State
import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import qualified Data.Map as M
import qualified LLVM.General.AST as AST

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
  shell' (initModule) (M.empty :: Ctx)

shell' :: AST.Module -> Ctx -> IO ()
shell' mod ctx = do
  putStr "MiniML> "
  hFlush stdout
  cmd <- getLine
  case parse toplevelCmdP "" cmd of
   Left e -> print e >> shell' mod ctx
   Right ast -> traceShow ast $ putStrLn res >> shell' mod' ctx'
     where
       (str, ctx') = runState (exec ast) ctx
       res = "AST: " ++ str ++ "\nLLVM bytecode:\n" ++ bytecode
       (mod', bytecode) = (mod, "")

main :: IO ()
main = do
  ast <- parseFile "tests/let.miniml" 
  print ast
  ast <- parseFile "tests/if.miniml"
  print ast
  ast <- parseFile "tests/fun.miniml"
  print ast
  ast <- parseFile "tests/apply.miniml"
  print ast
  ast <- parseFile "tests/bool.miniml"
  print ast
  ast <- parseFile "tests/minus.miniml"
  print ast
  shell
