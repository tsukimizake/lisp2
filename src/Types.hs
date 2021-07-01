{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types where

import Control.Monad.Except
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Gensym

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
  | Prim {name :: Text, op :: [Expr] -> CompilerM Expr}
  | Case {key :: Expr, clauses :: [Clause]}

data Clause = Clause {patStr :: Text, patFunc :: Expr -> Bool, clauseBody :: Expr}

instance Show Clause where
  show Clause {patStr, patFunc, clauseBody} = T.unpack $ "(" <> patStr <> showText clauseBody <> ")"

showText :: (Show a) => a -> Text
showText = T.pack . show

instance Show Expr where
  show x = case x of
    Constant x -> show x
    Atom x -> T.unpack x
    List xs -> T.unpack $ "(" <> T.intercalate " " (map showText xs) <> ")"
    Func {params, body} -> T.unpack $ "(lambda (" <> T.unwords params <> ")" <> T.intercalate " " (map showText body) <> ")"
    Prim {name} -> T.unpack name
    Case {clauses} -> T.unpack $ "(case " <> T.intercalate " " (map showText clauses) <> ")"

instance Eq Expr where
  l == r = case (l, r) of
    (Atom a, Atom b) -> a == b
    (Constant a, Constant b) -> a == b
    (List a, List b) -> a == b
    input -> error $ "invalid eq comparison " ++ show input

type Error = Text

newtype CompilerM a = CompilerM (GensymT (ExceptT Error IO) a)
  deriving (Functor, Applicative, Monad, MonadError Error, MonadIO)

runIOThrows :: CompilerM a -> IO (Either Error a)
runIOThrows (CompilerM x) = runExceptT $ runGensymT x

pattern OpList x xs = (List (Atom x : xs))

pattern QuoteList xs = OpList "quote" xs

pattern N x = Constant (Num x)

pattern B x = Constant (Bool x)

pattern S x = Constant (Str x)

gensym :: CompilerM Text
gensym = CompilerM Gensym.gensym

traverseExpr :: (Monad m) => (Expr -> m Expr) -> Expr -> m Expr
traverseExpr f x@(List xs) = do
  x' <- List <$> mapM (traverseExpr f) xs
  f x'
traverseExpr f x = do
  f x
