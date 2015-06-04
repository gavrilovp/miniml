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
intP = Int <$> integer

boolP :: Parser Expr
boolP = try (Bool True <$ reserved "true")
        <|> (Bool False <$ reserved "false")

termP :: Parser Expr
termP =     parens exprP
        <|> varP
        <|> intP

opP :: Parser Expr
opP = buildExpressionParser aOperators termP

ifP :: Parser Expr
ifP = do
  reserved "if"
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
  reserved "fun"
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
  let hack =     try (parens exprP)
             <|> try funP
             <|> try ifP
             <|> try opP
             <|> try intP
  expr1 <- hack
  expr2 <- hack
  return $ Apply expr1 expr2

exprP :: Parser Expr
exprP =     try (parens exprP)
        <|> try applyP
        <|> try funP
        <|> try ifP
        <|> try opP
        <|> try intP
        <|> try boolP

defP :: Parser ToplevelCmd
defP = do
  reserved "let"
  var  <- identifier
  reservedOp "="
  expr <- exprP
  return $ Def var expr

toplevelCmdP :: Parser ToplevelCmd
toplevelCmdP =     defP
               <|> Expr <$> exprP
