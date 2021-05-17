{-# LANGUAGE NamedFieldPuns #-}

module Types where

import Control.Monad.Except
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T

data Value
  = Num Int
  | Str Text
  | Bool Bool
  deriving (Eq)

instance Show Value where
  show (Num x) = show x
  show (Str s) = T.unpack s
  show (Bool True) = "#t"
  show (Bool False) = "#f"

data TypeTag = Number | Strng deriving (Show, Eq)

type Sym = Text

type Env = IORef (Map Sym (IORef Expr))

nullEnv :: IO Env
nullEnv = newIORef M.empty

showEnv :: Env -> IO String
showEnv env = do
  es <- M.toList <$> readIORef env
  ss <- forM es $ \(k, v) -> do
    v' <- readIORef v
    pure $ k <> " : " <> showText v'
  pure . T.unpack $ T.intercalate ",  " ss

data Expr
  = Constant Value
  | Atom Text
  | List [Expr]
  | Func {params :: [Text], body :: [Expr], closure :: Env}
  | Prim {name :: Text, op :: [Expr] -> IOThrowsError Expr}
  | Case {clauses :: [(Expr -> Bool, Expr)]}

showText :: (Show a) => a -> Text
showText = T.pack . show

instance Show Expr where
  show x = case x of
    Constant x -> show x
    Atom x -> T.unpack x
    List xs -> T.unpack $ "(" <> T.intercalate " " (map showText xs) <> ")"
    Func {params, body} -> T.unpack $ "(lambda (" <> T.unwords params <> ")" <> T.intercalate " " (map showText body) <> ")"
    Prim {name} -> T.unpack name

instance Eq Expr where
  l == r = case (l, r) of
    (Atom a, Atom b) -> a == b
    (Constant a, Constant b) -> a == b
    (List a, List b) -> a == b
    input -> error $ "invalid eq comparison " ++ show input

type Error = Text

type IOThrowsError = ExceptT Error IO

runIOThrows :: IOThrowsError a -> IO (Either Error a)
runIOThrows = runExceptT

pattern OpList x xs = (List (x : xs))

pattern QuoteList xs = OpList (Atom "quote") xs

pattern N x = Constant (Num x)

pattern B x = Constant (Bool x)

pattern S x = Constant (Str x)
