module Eval where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Types

showText :: (Show a) => a -> Text
showText = T.pack . show

typeError :: Text -> Expr -> Text
typeError expected actual = "type error, " <> expected <> " expected on " <> showText actual

coerceNum :: Expr -> Either Error Int
coerceNum (N v) = pure v
coerceNum x = Left $ typeError "num" x

coerceBool :: Expr -> Either Error Bool
coerceBool (B v) = pure v
coerceBool e = Left $ typeError "bool" e

evalInfix :: Env -> (Expr -> a -> Either Error Expr) -> [Expr] -> (Expr -> Either Error a) -> Either Error Expr
evalInfix env op args coerce = do
  if null args
    then Left "no arg"
    else do
      let (h : t) = args
      foldM
        ( \acc arg -> do
            arg' <- coerce arg
            op acc arg'
        )
        h
        t

evalMath :: Env -> (Int -> Int -> Int) -> [Expr] -> Either Error Expr
evalMath env op args = do
  let op' :: Expr -> Int -> Either Error Expr
      op' l r = do
        l' <- coerceNum l
        pure . Constant . Num $ op l' r
  evalInfix env op' args coerceNum

pattern OpList x xs = (List (x : xs))

pattern QuoteList xs = OpList (Atom "quote") xs

pattern N x = Constant (Num x)

pattern B x = Constant (Bool x)

pattern S x = Constant (Str x)

eval :: Env -> Expr -> Either Error Expr
eval env v@(Constant _) = pure v
eval _ x@(Atom _) = pure x
eval _ (QuoteList xs) = pure $ List xs
eval env (List [Atom "if", pred, t, f]) = do
  pred' <- eval env pred
  case pred' of
    B False -> eval env f
    _ -> eval env t
eval env (List (Atom "cond" : key : clauses)) =
  evalCond env key clauses
eval env (List []) = do
  pure nil
eval env (List [x]) = do
  eval env x
eval env (OpList x xs) = do
  xs' <- mapM (eval env) xs
  x' <- eval env x
  case x' of
    Atom a -> evalBuiltinOp env a xs'
    _ -> Left "TODO: non-atom function"

safe :: ([a] -> b) -> [a] -> Maybe b
safe _ [] = Nothing
safe f xs = Just $ f xs

-- predicateが初めて満たされたやつを返す
condHead :: (a -> Bool) -> [a] -> Maybe a
condHead predicate xs =
  safe head $ dropWhile (not . predicate) xs

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err Nothing = Left err
maybeToEither _ (Just x) = Right x

evalCond :: Env -> Expr -> [Expr] -> Either Error Expr
evalCond env key clauses = do
  key' <- eval env key
  case condHead (\(List (List matches : _)) -> key' `elem` matches) clauses of
    Nothing -> pure nil
    Just x -> eval env x

evalCompOp :: Env -> (Int -> Int -> Bool) -> [Expr] -> Either Error Expr
evalCompOp env op args = do
  let op' :: Expr -> Int -> Either Error Expr
      op' l r = do
        l' <- coerceNum l
        pure . B $ op l' r
  evalInfix env op' args coerceNum

evalBoolOp :: Env -> (Bool -> Bool -> Bool) -> [Expr] -> Either Error Expr
evalBoolOp env op args = do
  let op' :: Expr -> Bool -> Either Error Expr
      op' l r = do
        l' <- coerceBool l
        pure . B $ op l' r
  evalInfix env op' args coerceBool

evalBuiltinOp :: Env -> Sym -> [Expr] -> Either Error Expr
evalBuiltinOp env "+" args = evalMath env (+) args
evalBuiltinOp env "-" args = evalMath env (-) args
evalBuiltinOp env "*" args = evalMath env (*) args
evalBuiltinOp env "/" args = evalMath env div args -- TODO devide by zero error
evalBuiltinOp env "=" args = evalCompOp env (==) args
evalBuiltinOp env "/=" args = evalCompOp env (/=) args
evalBuiltinOp env "<" args = evalCompOp env (<) args
evalBuiltinOp env ">" args = evalCompOp env (>) args
evalBuiltinOp env "<=" args = evalCompOp env (<=) args
evalBuiltinOp env ">=" args = evalCompOp env (>=) args
evalBuiltinOp env "||" args = evalBoolOp env (||) args
evalBuiltinOp env "symbol?" [Atom _] = pure $ B True
evalBuiltinOp env "symbol?" _ = pure $ B False
evalBuiltinOp env "string?" [Constant (Str _)] = pure $ B True
evalBuiltinOp env "string?" _ = pure $ B False
evalBuiltinOp env "number?" [Constant (Num _)] = pure $ B True
evalBuiltinOp env "number?" _ = pure $ B False
evalBuiltinOp env "car" [List (x : _)] = pure x
evalBuiltinOp env "cdr" [List (_ : xs)] = pure $ List xs
evalBuiltinOp env "symbol-to-string" [Atom x] = pure . S $ x
evalBuiltinOp env "string-to-symbol" [Constant (Str x)] = pure $ Atom x
evalBuiltinOp env "eq?" [x, y] = pure $ B $ x == y
evalBuiltinOp env op args = Left $ "not implemented: " <> showText op <> " " <> showText args

nil :: Expr
nil = List []
