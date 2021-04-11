module Main where

import Control.Monad
import qualified Data.List as L
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Parser
import Types

showText :: (Show a) => a -> Text
showText = T.pack . show

evalToNum :: Env -> Expr -> Either Error Int
evalToNum env x = case eval env x of
  Right (Constant (Num v)) -> pure v
  _ -> Left $ "type error, num expected on " <> showText x

evalInfix :: Env -> (Expr -> a -> Either Error Expr) -> [Expr] -> (Env -> Expr -> Either Error a) -> Either Error Expr
evalInfix env op args innerEval = do
  if null args
    then Left "no arg"
    else do
      let (h : t) = args
      foldM
        ( \acc arg -> do
            arg' <- innerEval env arg
            op acc arg'
        )
        h
        t

evalMath :: Env -> (Int -> Int -> Int) -> [Expr] -> Either Error Expr
evalMath env op args = do
  let op' :: Expr -> Int -> Either Error Expr
      op' l r = do
        l' <- evalToNum env l
        pure . Constant . Num $ op l' r
  evalInfix env op' args evalToNum

pattern OpList x xs = (List ((Atom x) : xs))

pattern QuoteList xs = OpList "quote" xs

pattern N x = Constant (Num x)

eval :: Env -> Expr -> Either Error Expr
eval env v@(Constant _) = pure v
eval _ (QuoteList xs) = pure $ List xs
eval env (OpList x xs) = evalOpExpr env x xs

evalOpExpr :: Env -> Sym -> [Expr] -> Either Error Expr
evalOpExpr env "+" args = evalMath env (+) args
evalOpExpr env "-" args = evalMath env (-) args
evalOpExpr env "*" args = evalMath env (*) args
evalOpExpr env "/" args = evalMath env div args -- TODO devide by zero error
evalOpExpr env op args = Left $ "not implemented: " <> showText op <> " " <> showText args

main :: IO ()
main = do
  print $ eval M.empty $ OpList "+" [N 3, N 1, OpList "*" [N 19, N 2]]
