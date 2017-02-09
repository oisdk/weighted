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
  ,pattern Weighted)
  where

import           Control.Monad.Identity
import           Control.Monad.State.Strict
import           Control.Monad.Writer
import           Data.Coerce
import           Data.Semiring

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
