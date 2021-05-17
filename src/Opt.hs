module Opt (optimize) where

import Control.Monad
import Debug.Trace
import Types

optimize :: Expr -> IOThrowsError Expr
optimize x = do
  traverseExpr optimizeCase x

optimizeCase :: Expr -> IOThrowsError Expr
optimizeCase (List (Atom "case" : key : clauses)) = do
  clauses' <- forM clauses \clause -> do
    case clause of
      (List [List pats, body]) -> do
        pure $ Clause (showText (List pats)) (`elem` pats) body
      (List [Atom "else", body]) -> do
        pure $ Clause "else" (const True) body
      x -> error $ "broken clause " <> show x
  pure $ Case key clauses'
optimizeCase x = do
  pure x

traverseExpr :: (Monad m) => (Expr -> m Expr) -> Expr -> m Expr
traverseExpr f x@(List xs) = do
  x' <- List <$> mapM (traverseExpr f) xs
  f x'
traverseExpr f x = do
  f x
