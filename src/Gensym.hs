module Gensym
  ( GensymT,
    gensym,
    runGensymT,
  )
where

import Control.Applicative
import Control.Monad (liftM)
import Control.Monad.State
import Data.Functor

type GensymT m a = StateT Int m a

gensym :: (Monad m) => GensymT m Int
gensym = do
  count <- get
  let count' = count + 1
  put count'
  pure count'

runGensymT :: (Monad m) => GensymT m a -> m a
runGensymT = flip evalStateT 0
