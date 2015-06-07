{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map
import qualified Syntax as S

import Control.Monad.State
import Control.Applicative

import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST

import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.IntegerPredicate as IP

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

type Vars = Map.Map String (Int, String, S.Ty)
data GeneratorState = GeneratorState {
    modstate    :: AST.Module
  , vars        :: Vars
}

newtype LLVM a = LLVM { unLLVM :: State GeneratorState a }
  deriving (Functor, Applicative, Monad, MonadState GeneratorState)

runLLVM :: GeneratorState -> LLVM a -> GeneratorState
runLLVM = flip (execState . unLLVM)

toType :: S.Ty -> AST.Type
toType S.TInt = int
toType S.TBool = bool
toType (S.TArrow _ _) = error "Can't print function prototype"

emptyState :: String -> GeneratorState
emptyState label = GeneratorState {
    modstate = (AST.defaultModule { AST.moduleName = label })
  , vars = Map.empty :: Vars
}

genGlobalName :: String -> S.Ty -> LLVM String
genGlobalName l ty = do
  vars_list <- gets vars
  let (new_vars, name) = getName_impl vars_list
  modify $ \s -> s { vars = new_vars }
  return name
  where
    getName_impl vars_list =
      case Map.lookup l vars_list of
        Nothing -> ((update 1), (new_name 1))
        Just (i, name, _) -> ((update (i + 1)), (new_name (i + 1)))
      where
        new_name i = (if i /= 1 then l ++ "_" ++ (show i) else l) ++ "_global"
        update i = Map.insert l (i, (new_name i), ty) vars_list

getGlobalVar :: String -> Vars -> (String, S.Ty)
getGlobalVar name vars =
  case Map.lookup name vars of
    Just (_, n, ty) -> (n, ty)
    Nothing         -> error $ "Variable '" ++ name ++ "' not found in scope"

addDefn :: AST.Definition -> LLVM ()
addDefn d = do
  st <- gets modstate
  let defs = AST.moduleDefinitions st
      m_state = st { AST.moduleDefinitions = defs ++ [d] }
  modify $ \s -> s { modstate = m_state }

define :: AST.Type -> String -> [(AST.Type, AST.Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = do
  addDefn $ AST.GlobalDefinition $ functionDefaults {
    name        = AST.Name label
  , parameters  = ([AST.Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

globalVar :: S.Ty -> String -> [(AST.Type, AST.Name)] -> [BasicBlock] -> LLVM ()
globalVar ty name args block = do
  real_name <- genGlobalName name ty
  define (toType ty) real_name args block

---------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

int :: AST.Type
int = AST.IntegerType 32

bool :: AST.Type
bool = AST.IntegerType 1

one = cons $ C.Int 1 1
zero = cons $ C.Int 1 0
false = zero
true = one

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm, Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix + 1) ns)

instance IsString AST.Name where
  fromString = AST.Name . fromString

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = [(String, AST.Operand)]

data CodegenState
  = CodegenState {
    currentBlock :: AST.Name                    -- Name of the active block to append to
  , blocks       :: Map.Map AST.Name BlockState -- Blocks for function
  , symtab       :: SymbolTable                 -- Function scope symbol table
  , blockCount   :: Int                         -- Count of basic blocks
  , count        :: Word                        -- Count of unnamed instructions
  , names        :: Names                       -- Name Supply
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                                -- Block index
  , stack :: [AST.Named AST.Instruction]        -- Stack of instructions
  , term  :: Maybe (AST.Named AST.Terminator)   -- Block terminator
  } deriving Show

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

sortBlocks :: [(AST.Name, BlockState)] -> [(AST.Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (AST.Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (AST.Name entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

instr :: AST.Instruction -> Codegen AST.Operand
instr ins = do
  n <- fresh
  let ref = (AST.UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = i ++ [ref AST.:= ins] } )
  return $ local ref

terminator :: AST.Named AST.Terminator -> Codegen (AST.Named AST.Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen AST.Name
entry = gets currentBlock

addBlock :: String -> Codegen AST.Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s -> s { blocks = Map.insert (AST.Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (AST.Name qname)

setBlock :: AST.Name -> Codegen AST.Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen AST.Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: String -> AST.Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ lcls }

getvar :: Vars -> String -> Codegen (Either (String, S.Ty) AST.Operand)
getvar globVars varname = do
  syms <- gets symtab
  case lookup varname syms of
    Just x  -> return $ Right x
    Nothing -> return $ Left gvar
  where
    gvar = getGlobalVar varname globVars

-------------------------------------------------------------------------------

-- References
local :: AST.Name -> AST.Operand
local = AST.LocalReference int

global :: AST.Name -> AST.Operand
global n = AST.ConstantOperand $ C.GlobalReference int n

externf :: AST.Type -> AST.Name -> AST.Operand
externf ty = AST.ConstantOperand . C.GlobalReference ty

-- Arithmetic and Constants
f_add :: AST.Operand -> AST.Operand -> Codegen AST.Operand
f_add a b = instr $ AST.Add False False a b []

f_sub :: AST.Operand -> AST.Operand -> Codegen AST.Operand
f_sub a b = instr $ AST.Sub False False a b []

f_mul :: AST.Operand -> AST.Operand -> Codegen AST.Operand
f_mul a b = instr $ AST.Mul False False a b []

f_cmp :: IP.IntegerPredicate -> AST.Operand -> AST.Operand -> Codegen AST.Operand
f_cmp cond a b = instr $ AST.ICmp cond a b []

f_lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
f_lt a b = f_cmp IP.SLT a b

f_eq :: AST.Operand -> AST.Operand -> Codegen AST.Operand
f_eq a b = f_cmp IP.EQ a b

cons :: C.Constant -> AST.Operand
cons = AST.ConstantOperand

toArgs :: [AST.Operand] -> [(AST.Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects
call :: AST.Operand -> [AST.Operand] -> Codegen AST.Operand
call fn args = instr $ AST.Call False CC.C [] (Right fn) (toArgs args) [] []

alloca :: AST.Type -> Codegen AST.Operand
alloca ty = instr $ AST.Alloca ty Nothing 0 []

store :: AST.Operand -> AST.Operand -> Codegen AST.Operand
store ptr val = instr $ AST.Store False ptr val Nothing 0 []

load :: AST.Operand -> Codegen AST.Operand
load ptr = instr $ AST.Load False ptr Nothing 0 []

-- Control Flow
br :: AST.Name -> Codegen (AST.Named AST.Terminator)
br val = terminator $ AST.Do $ AST.Br val []

cbr :: AST.Operand -> AST.Name -> AST.Name -> Codegen (AST.Named AST.Terminator)
cbr cond tr fl = terminator $ AST.Do $ AST.CondBr cond tr fl []

ret :: AST.Operand -> Codegen (AST.Named AST.Terminator)
ret val = terminator $ AST.Do $ AST.Ret (Just val) []

phi :: AST.Type -> [(AST.Operand, AST.Name)] -> Codegen AST.Operand
phi ty incoming = instr $ AST.Phi ty incoming []
