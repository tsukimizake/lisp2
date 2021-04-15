module Types where

import Control.Monad.Except
import Data.IORef
import Data.Map as M
import Data.Text

data Value
  = Num Int
  | Str Text
  | Bool Bool
  deriving (Show, Eq)

data TypeTag = Number | Strng deriving (Show, Eq)

type Sym = Text

type Env = IORef (Map Sym (IORef Expr))

nullEnv :: IO Env
nullEnv = newIORef M.empty

data Expr
  = Constant Value
  | Atom Text
  | List [Expr]
  deriving (Show, Eq)

type Error = Text

type IOThrowsError = ExceptT Error IO

runIOThrows :: IOThrowsError a -> IO (Either Error a)
runIOThrows = runExceptT
