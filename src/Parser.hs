module Parser
       ( toplevelCmdP
       ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import Syntax
import Lexer

varP :: Parser Expr
varP = Var <$> identifier

intP :: Parser Expr
intP = Int <$> try integer

boolP :: Parser Expr
boolP =     Bool True <$ (try $ reserved "true")
        <|> Bool False <$ (try $ reserved "false")

termP :: Parser Expr
termP = parens exprP <|> try applyP <|> intP <|> varP  

opP :: Parser Expr
opP = buildExpressionParser aOperators termP

ifP :: Parser Expr
ifP = do
  try $ reserved "if"
  cond  <- exprP
  reserved "then"
  expr1 <- exprP
  reserved "else"
  expr2<- exprP
  return $ If cond expr1 expr2

tyP :: Parser Ty
tyP = do
  reserved "int"
  return TInt

funP :: Parser Expr
funP = do
  try $ reserved "fun"
  fn <- identifier
  let vct = do
        argName <- identifier
        colon
        argType <- tyP
        return (argName, argType)
  (argName, argType) <- parens vct
  colon
  fType <- tyP
  reserved "is"
  expr <- exprP
  return $ Fun fn argName argType fType expr  

applyP :: Parser Expr
applyP = do
  let hack = parens exprP <|> funP <|> intP <|> varP
      apP :: Parser (Expr -> Expr -> Expr)
      apP = return Apply
  chainl1 hack apP

exprP :: Parser Expr
exprP = parens exprP <|> opP <|> funP <|> ifP <|> applyP
        <|> intP <|> boolP <|> varP

defP :: Parser ToplevelCmd
defP = do
  try $ reserved "let"
  var  <- identifier
  reservedOp "="
  expr <- exprP
  return $ Def var expr

toplevelCmdP :: Parser ToplevelCmd
toplevelCmdP = spaces *> (defP <|> Expr <$> exprP) <* eof
