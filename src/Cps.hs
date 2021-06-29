module Cps where

import Data.Text
import Gensym
import Types (Expr, Value (..))
import qualified Types as E

type Sym = Text

data Op
  = Id Sym
  | Constant Value
  deriving (Show, Eq)

data PrimSym = Add | Sub | RShift | LShift | LT | GT | EQ | HEAP | STACK | POP | RREF | RSET
  deriving (Show, Eq)

data FixSym = FixF | FixS
  deriving (Show, Eq)

data Cps
  = (PrimSym, [Op], [Sym]) :>> [Cps]
  | (FixSym, [Bind]) :> Cps
  | AppF Op [Op]
  | AppB Op [Op]
  deriving (Show, Eq)

(>>:=) :: (Monad m) => (PrimSym, [Op], [Sym]) -> m [Cps] -> m Cps
l >>:= r = do
  k <- r
  pure $ l :>> k

(>:=) :: (Monad m) => (FixSym, [Bind]) -> m Cps -> m Cps
l >:= r = do
  k <- r
  pure $ l :> k

data Bind = Bind Sym [Sym] Cps
  deriving (Show, Eq)

toCpsImpl :: Expr -> (Op -> E.CompilerM Cps) -> E.CompilerM Cps
toCpsImpl (E.Constant val) c = c (Constant val)
toCpsImpl (E.Atom sym) c = c (Id sym)
toCpsImpl (E.Prim _ _) _ = undefined
toCpsImpl (E.OpList "+" [l, r]) c = do
  let c' = \p0 ->
        toCpsImpl
          r
          ( \p1 ->
              (Add, [p0, p1], ["r"]) >>:= mapM c [Id "r"]
          )
  toCpsImpl l c'
toCpsImpl (E.List xs) c = undefined
toCpsImpl _ _ = undefined
