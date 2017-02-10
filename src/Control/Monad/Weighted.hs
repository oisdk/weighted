{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}

module Control.Monad.Weighted
  (WeightedT
  ,runWeightedT
  ,pattern WeightedT
  ,Weighted
  ,runWeighted
  ,pattern Weighted
  ,execWeightedT
  ,evalWeightedT
  ,execWeighted
  ,evalWeighted)
  where

import           Control.Applicative
import           Control.Monad.Identity
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Class
import           Data.Coerce
import           Data.Semiring

-- | A monad transformer similar to 'WriterT', except that it does not leak
-- space, and it uses the 'Semiring' class, rather than 'Monoid'.
newtype WeightedT s m a =
    WeightedT_ (StateT s m a)
    deriving (Functor,Applicative,Monad,MonadTrans)

runWeightedT
    :: Semiring s
    => WeightedT s m a -> m (a, s)
runWeightedT =
    (coerce :: (StateT s m a -> m (a, s)) -> WeightedT s m a -> m (a, s))
        (`runStateT` one)
{-# INLINE runWeightedT #-}

pattern WeightedT :: (Functor m, Semiring s) => m (a, s) -> WeightedT s m a
pattern WeightedT x <- (runWeightedT -> x) where
  WeightedT y = WeightedT_ (StateT (\s -> (fmap.fmap) (s<.>) y))

type Weighted s = WeightedT s Identity

pattern Weighted :: Semiring s => (a, s) -> Weighted s a
pattern Weighted x <- (runWeighted -> x) where
  Weighted (y,p) = WeightedT_ (StateT (\s -> Identity (y, p<.>s)))

runWeighted
    :: Semiring s
    => Weighted s a -> (a, s)
runWeighted =
    (coerce :: (WeightedT s Identity a -> Identity (a, s)) -> (WeightedT s Identity a -> (a, s)))
        runWeightedT
{-# INLINE runWeighted #-}

instance (Semiring s, Monad m) => MonadWriter (Mul s) (WeightedT s m) where
  writer (x, Mul s) = WeightedT (pure (x, s))
  {-# INLINE writer #-}
  listen (WeightedT_ s) = WeightedT_ ((,) <$> s <*> gets Mul)
  {-# INLINE listen #-}
  pass (WeightedT_ s) = WeightedT_ (passS s) where
    passS = (=<<) (uncurry (<$) . fmap (modify . coerce))
  {-# INLINE pass #-}

evalWeightedT :: (Monad m, Semiring s) => WeightedT s m a -> m a
evalWeightedT =
    (coerce :: (StateT s m a -> m a) -> WeightedT s m a -> m a)
        (`evalStateT` one)
{-# INLINE evalWeightedT #-}

execWeightedT :: (Monad m, Semiring s) => WeightedT s m a -> m s
execWeightedT =
    (coerce :: (StateT s m a -> m s) -> WeightedT s m a -> m s)
        (`execStateT` one)
{-# INLINE execWeightedT #-}

evalWeighted :: Semiring s => Weighted s a -> a
evalWeighted =
    (coerce :: (State s a -> a) -> Weighted s a -> a)
        (`evalState` one)
{-# INLINE evalWeighted #-}

execWeighted :: Semiring s => Weighted s a -> s
execWeighted =
    (coerce :: (State s a -> s) -> Weighted s a -> s)
        (`execState` one)
{-# INLINE execWeighted #-}

instance (Alternative m, Monad m, Semiring s) => Alternative (WeightedT s m) where
  empty = WeightedT empty
  WeightedT x <|> WeightedT y = WeightedT (x <|> y)
  _ <|> _ = undefined

