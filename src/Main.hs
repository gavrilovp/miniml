module Main
       ( main
       ) where

import Text.ParserCombinators.Parsec

import Syntax
import Lexer
import Parser (toplevelCmdP)

parseFile :: String -> IO ToplevelCmd
parseFile file = do
  program  <- readFile file
  case parse toplevelCmdP "" program of
   Left e  -> print e >> fail "parse error"
   Right r -> return r

main :: IO ()
main = do
  ast <- parseFile "tests/let.miniml" 
  print ast
  ast <- parseFile "tests/if.miniml"
  print ast

