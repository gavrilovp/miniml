module TypeCheck
       ( check
       ) where

import Control.Exception
import Data.Typeable hiding (typeOf)
import qualified Data.Map as M
import Syntax

data TypeError = TE String
   deriving (Show, Typeable)

instance Exception TypeError

-- A context describing the types of globally defined values.
type Ctx = M.Map Name Ty

typeOf :: Ctx -> Expr -> Ty
typeOf ctx (Var v) = ctx M.! v -- replace with my exception?
typeOf _ (Int _) = TInt
typeOf _ (Bool _) = TBool
typeOf ctx (Plus e1 e2)
  | check ctx TInt e1 && check ctx TInt e2 = TInt
  | otherwise = throw (TE "wrong type")
typeOf ctx (Minus e1 e2)
  | check ctx TInt e1 && check ctx TInt e2 = TInt
  | otherwise = throw (TE "wrong type")
typeOf ctx (Equal e1 e2)
  | check ctx TInt e1 && check ctx TInt e2 = TBool
  | otherwise = throw (TE "wrong type")
typeOf ctx (Less e1 e2)
  | check ctx TInt e1 && check ctx TInt e2 = TBool
  | otherwise = throw (TE "wrong type")
typeOf ctx (If e1 e2 e3)
  | check ctx TBool e1 && check ctx ty e3 = ty
  | otherwise = throw (TE "wrong type")
  where
    ty = typeOf ctx e2
typeOf ctx (Fun fN argN argT fT e)
  | check (M.fromList [(fN, TArrow argT fT), (argN, argT)]) fT e = TArrow argT fT
  | otherwise = throw (TE "wrong type")
typeOf ctx (Apply e1 e2)
  | check ctx ty1 e2 = ty2
  | otherwise = throw (TE "wrong type")
  where
    (TArrow ty1 ty2) = typeOf ctx e1 -- ToDo: throw exc

check :: Ctx -> Ty -> Expr -> Bool
check ctx ty e =
  let ty' = typeOf ctx e
  in
   if ty /= ty'
   then
     throw (TE "wrong type")
   else
     True
