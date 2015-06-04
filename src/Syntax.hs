module Syntax
       ( Name
       , Expr (..)
       , ToplevelCmd (..)
       ) where

-- Variable names
type Name = String

-- Types
data Ty = TInt
        | TBool
        | TArrow Ty Ty
        deriving Show

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
