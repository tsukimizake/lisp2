module Cont where

import Data.Text
import Types

type Id = Text

data Op
  = Id
  | Constant Value
  deriving (Show, Eq)

data Cps
  = Prim PrimSym [Op] [Id] [Cps]
  | FixH [Bind] Cps
  | FixS [Bind] Cps
  | AppF Op [Op]
  | AppB Op [Op]
  deriving (Show, Eq)

data Bind = Bind Id [Id] Cps
  deriving (Show, Eq)

data PrimSym = Add | Sub | RShift | LShift | LT | GT | EQ | HEAP | STACK | POP | RREF | RSET
  deriving (Show, Eq)
