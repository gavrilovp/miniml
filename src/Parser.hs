module Parser
       ( toplevelCmdParser
       ) where

import Text.ParserCombinators.Parsec

import Syntax

varParser :: Parser Expr
varParser = Var <$> many1 letter 

-- add depth restriction for left recursion
-- or chainl1
exprParser :: Parser Expr
exprParser = undefined

toplevelCmdParser :: Parser ToplevelCmd
toplevelCmdParser = undefined
