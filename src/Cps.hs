{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Cps where

import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import Data.IORef
import Data.Map as M
import Data.Text as T
import qualified Debug.Trace as Debug
import Types (Expr, Value (..))
import qualified Types as E

type Sym = Text

type Env = Map Sym Value

data Op
  = Id Sym
  | Constant Value
  deriving (Show, Eq)

data PrimSym = Add | Sub | RShift | LShift | LT | GT | EQ | HEAP | STACK | POP | RREF | RSET | DebugLog String
  deriving (Show, Eq)

data FixSym = FixF | FixS
  deriving (Show, Eq)

data Cps
  = (PrimSym, [Op], Maybe Sym) :>> [Cps]
  | (FixSym, [Bind]) :> Cps
  | AppF Op [Op]
  | AppB Op [Op]
  | DebugNop Value
  deriving (Show, Eq)

(>>:=) :: (Monad m) => (PrimSym, [Op], Maybe Sym) -> m [Cps] -> m Cps
l >>:= r = do
  k <- r
  pure $ l :>> k

(>:=) :: (Monad m) => (FixSym, [Bind]) -> m Cps -> m Cps
l >:= r = do
  k <- r
  pure $ l :> k

data Bind = Bind Sym [Sym] Cps
  deriving (Show, Eq)

readIfId :: Env -> Op -> Value
readIfId env op =
  case op of
    Id k -> case env M.!? k of
      Nothing -> error $ T.unpack $ k <> " not found in " <> E.showText env
      Just r -> r
    Constant v -> v

pushToEnv :: Maybe Sym -> Value -> Env -> Env
pushToEnv (Just k) v env = M.insert k v env
pushToEnv Nothing v env = env

unwrapOne :: [a] -> a
unwrapOne = Prelude.head

evalCps :: Env -> Cps -> E.CompilerM (Env, Value)
evalCps env ((Add, args, ret) :>> cont) = do
  acc <- liftIO $ newIORef 0
  forM_ args $ \arg -> do
    let (Num v) = readIfId env arg
    liftIO $ modifyIORef acc (+ v)
  res <- liftIO $ readIORef acc
  let newenv = pushToEnv ret (Num res) env
  evalCps newenv (unwrapOne cont)
evalCps env ((Cps.LT, [lhs, rhs], ret) :>> [then_, else_]) = do
  let (Num lhs') = readIfId env lhs
  let (Num rhs') = readIfId env rhs
  if lhs' < rhs'
    then evalCps env then_
    else evalCps env else_
evalCps env ((DebugLog msg, ops, _) :>> cont) = do
  Debug.traceM $ msg <> ": " <> show ops
  evalCps env (unwrapOne cont)
evalCps env (DebugNop val) = pure (env, val)

cps1 :: Cps
cps1 =
  (Add, [Id "x", Constant $ Num 2], Just "t")
    :>> [ (Cps.LT, [Id "t", Constant $ Num 10], Just "r")
            :>> [ (DebugLog "t", [], Nothing) :>> [DebugNop (Bool True)],
                  (DebugLog "f", [], Nothing) :>> [DebugNop (Bool False)]
                ]
        ]
