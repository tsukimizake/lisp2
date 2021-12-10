module Opt (optimize) where

import Control.Monad
import Cps
import Debug.Trace
import Types

optimize :: Expr -> EvalM Expr
optimize x = do
  traverseExpr optimizeCase x

optimizeCase :: Expr -> EvalM Expr
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
