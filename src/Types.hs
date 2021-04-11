module Types where

import Data.Map
import Data.Text

data Value
  = Num Int
  | Str Text
  | Bool Bool
  deriving (Show, Eq)

data TypeTag = Number | Strng deriving (Show, Eq)

type Sym = Text

type Env = Map Sym Expr

data Expr
  = Constant Value
  | Atom Text
  | List [Expr]
  deriving (Show, Eq)

type Error = Text
