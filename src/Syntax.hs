module Syntax
       ( Name
       , Expr (..)
       , ToplevelCmd
       ) where

-- Variable names
type Name = String

-- Types
data Ty = TInt
        | TBool
        | TArrow Ty Ty

-- Expressions
data Expr = Var Name
          | Int Int
          | Bool Bool
          | Times Expr Expr
          | Plus Expr Expr
          | Minus Expr Expr
          | Equal Expr Expr
          | Less Expr Expr
          | If Expr Expr Expr
          | Fun Name Name Ty Ty Expr 
          | Apply Expr Expr

-- Toplevel commands
data ToplevelCmd = Expr Expr
                 | Def Name Expr
