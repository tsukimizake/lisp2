{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Cps where

import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import Data.IORef
import Data.Map as M
import Data.Text as T
import qualified Data.Type.Bool as E
import Debug.Trace (traceM, traceShowM)
import qualified Debug.Trace as Debug
import Types (Expr, Value (..))
import qualified Types as E
import Control.Monad.Except (throwError)

type Sym = Text

type Env = Map Sym Fix

data Op
  = Id Sym
  | Constant Value
  deriving (Show, Eq)

data Prim
  = Add [Op] (Maybe Sym)
  | Sub [Op] (Maybe Sym)
  | RShift [Op] (Maybe Sym)
  | LShift [Op] (Maybe Sym)
  | HEAP [Op] (Maybe Sym)
  | STACK [Op] (Maybe Sym)
  | POP [Op] (Maybe Sym)
  | RREF [Op] (Maybe Sym)
  | RSET [Op] (Maybe Sym)
  | DebugLog String [Op] (Maybe Sym)
  deriving (Show, Eq)

data Branch
  = Lt [Op] (Maybe Sym)
  | Gt [Op] (Maybe Sym)
  | Eq [Op] (Maybe Sym)
  deriving (Show, Eq)

data Fix
  = FixF Sym [Sym] Cps
  | FixS Sym [Sym] Cps
  deriving (Show, Eq)

data Cps
  = Prim :>> Cps
  | Branch :|> [Cps]
  | Fix :&> Cps
  | AppF Op [Op]
  | AppB Op [Op]
  | DebugNop Value
  deriving (Show, Eq)

nop :: Op -> Cps
nop (Constant val) = DebugNop val
nop (Id _) = DebugNop (Num 0)

infixr 7 :>>

infixr 7 :|>

infixr 7 :&>

(>>:=) :: (Monad m) => Prim -> m Cps -> m Cps
l >>:= r = do
  k <- r
  pure $ l :>> k

(|>:=) :: (Monad m) => Branch -> m [Cps] -> m Cps
l |>:= r = do
  k <- r
  pure $ l :|> k

(&>:=) :: (Monad m) => Fix -> m Cps -> m Cps
l &>:= r = do
  k <- r
  pure $ l :&> k

infixr 7 >>:=

infixr 7 |>:=

infixr 7 &>:=

readIfId :: Env -> Op -> Value
readIfId env op =
  undefined

--   case op of
--     Id k -> case env M.!? k of
--       Nothing -> error $ T.unpack $ k <> " not found in " <> E.showText env
--       Just r -> r
--     Constant v -> v

pushToEnv :: Sym -> Fix -> Env -> Env
pushToEnv k v env = M.insert k v env

unwrapOne :: [a] -> a
unwrapOne = Prelude.head

evalCps :: Env -> Cps -> E.CompilerM (Env, Value)
evalCps env ((Add args ret) :>> cont) = do
  acc <- liftIO $ newIORef 0
  forM_ args $ \arg -> do
    let (Num v) = readIfId env arg
    liftIO $ modifyIORef acc (+ v)
  res <- liftIO $ readIORef acc
  newenv <- case ret of
    Just r -> do
      cont <- E.gensym
      pure $ pushToEnv r (FixS r [cont] (AppF (Id cont) [Constant (Num res)])) env
    Nothing -> pure env
  evalCps newenv cont
evalCps env (Lt [lhs, rhs] ret :|> [then_, else_]) = do
  let (Num lhs') = readIfId env lhs
  let (Num rhs') = readIfId env rhs
  if lhs' < rhs'
    then evalCps env then_
    else evalCps env else_
evalCps env (DebugLog msg ops _ :>> cont) = do
  Debug.traceM $ msg <> ": " <> show ops
  evalCps env cont
evalCps env (fix@(FixF fn args body) :&> cont) = do
  let env' = pushToEnv fn fix env
  undefined
evalCps env (FixS fn args body :&> cont) = evalCps env (FixF fn args body :&> cont) -- do the same
evalCps env (DebugNop val) = pure (env, val)

cps1env = FixS "x" ["cont"] (AppF (Id "cont") [Constant (Num 1)])

{- ORMOLU_DISABLE -}
-- (if (< (+ x 2) 10) then (debug "t") else (debug "f"))
cps1 :: Cps
cps1 =
  Add [Id "x", Constant $ Num 2] (Just "t")
    :>> Lt [Id "t", Constant $ Num 10] Nothing
      :|> [ DebugLog "t" [] Nothing :>> DebugNop (Bool True),
            DebugLog "f" [] Nothing :>> DebugNop (Bool False)
          ]

-- (define (g x)
--   (fix ((f (y) (+ x y)))
--     (+ (f (+ x 10)) 1)))
cps2 :: Cps
cps2 =
  FixF "g" ["k", "x"]
    ( FixF "f" ["c", "y"]
        ( Add [Id "x", Id "y"] (Just "t")
            :>> AppB (Id "c") [Id "t"]
        )
        :&> Add [Id "x", Constant $ Num 10] (Just "s")
        :>> FixS "d" ["t"]
          ( Add [Id "t", Constant $ Num 1] (Just "r")
              :>> AppB (Id "k") [Id "r"]
          )
        :&> AppF (Id "f") [Id "d", Id "s"]
    )
    :&> DebugNop (Num 0)

-- ; recursion!
-- (define (f x)
--   (f x))
cps3 :: Cps
cps3 = FixF "f" ["k", "x"]
  (AppF (Id "f") [Id "x"])
  :&> DebugNop (Num 0)
{- ORMOLU_ENABLE -}

{-# ANN fromExpr ("HLint: ignore Avoid lambda" :: String) #-}
fromExpr :: E.Expr -> (Op -> E.CompilerM Cps) -> E.CompilerM Cps
fromExpr (E.Constant v) c = c $ Constant v
fromExpr (E.Atom v) c = c $ Id v
fromExpr (E.OpList "+" [l, r]) c = do
  -- TODO multi args
  ret <- E.gensym
  let c' = fromExpr r . c''
      c'' = \p0 p1 -> Add [p0, p1] (Just ret) >>:= c (Id ret)
  fromExpr l c'
fromExpr (E.OpList "debug" [E.Constant val]) c = do
  -- TODO read env sitai
  traceShowM val
  DebugLog (show val) [] Nothing >>:= c (Constant val)
fromExpr (E.OpList "debug" _) c = throwError "malformed debug"
fromExpr (E.OpList "<" [lhs, rhs]) c =
  undefined
fromExpr (E.OpList "if" [cond, E.Atom "then", then_, E.Atom "else", else_]) c = do
  j <- E.gensym
  v <- E.gensym
  cv <- c (Id v)
  t' <- fromExpr then_ (\x -> pure $ AppF (Id j) [x])
  e' <- fromExpr else_ (\x -> pure $ AppF (Id j) [x])
  let c' = \x -> FixS j [v] cv :&> Eq [x, Constant $ Bool True] Nothing :|> [t', e']
  fromExpr cond (pure . c')
fromExpr (E.OpList "if" _) c = throwError "malformed if"
