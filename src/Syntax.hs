module Syntax
       ( Name
       , Ty (..)
       , Expr (..)
       , ToplevelCmd (..)
       ) where

-- Variable names
type Name = String

-- Types
data Ty = TInt         -- integers
        | TBool        -- booleans
        | TArrow Ty Ty -- functions
        deriving (Eq)

instance Show Ty where
  show (TInt) = "int"
  show (TBool) = "bool"
  show (TArrow ty1 ty2) = show ty1 ++ " -> " ++ show ty2

-- Expressions
data Expr = Var Name
          | Int Integer
          | Bool Bool
          | Times Expr Expr
          | Plus Expr Expr
          | Minus Expr Expr
          | Equal Expr Expr
          | Less Expr Expr
          | If Expr Expr Expr
          | Fun Name Name Ty Ty Expr
          | Apply Expr Expr
          deriving Show

-- Toplevel commands
data ToplevelCmd = Expr Expr
                 | Def Name Expr
                 deriving Show
