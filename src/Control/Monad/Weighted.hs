{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

-- | This module provides monad transformer similar to
-- 'Control.Monad.Writer.Strict.WriterT', implemented using 'StateT', making it
-- tail recursive. (The traditional writer always leaks space: see
-- <https://mail.haskell.org/pipermail/libraries/2013-March/019528.html here>
-- for more information).
--
-- <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#pattern-synonyms Pattern Synonyms>
-- are used to provide the same interface as
-- 'Control.Monad.Writer.Strict.WriterT'. Unfortunately, current GHC warns
-- whenever these patterns are used that there are unmatched patterns: the
-- <https://ghc.haskell.org/trac/ghc/ticket/8779 COMPLETE> pragma should solve
-- this problem in future version of GHC.
--
-- A pattern synonym is also provided for a non-transformer version of writer.
-- Again, this is just 'StateT' underneath, but its interface looks as if it was
-- defined like so:
--
-- > newtype Writer w a = Writer { runWriter :: (a, w) }
--
-- The other difference between this monad and
-- 'Control.Monad.Writer.Strict.WriterT' is that it relies on '<.>' from
-- 'Semiring', rather than 'mappend' from 'Monoid'.
module Control.Monad.Weighted
  (
   -- * Transformer
   WeightedT
  ,runWeightedT
  ,pattern WeightedT
  ,execWeightedT
  ,evalWeightedT
  ,
   -- * Plain
   Weighted
  ,runWeighted
  ,pattern Weighted
  ,execWeighted
  ,evalWeighted)
  where

import           Control.Applicative

import           Control.Monad.Fail
import           Control.Monad.Identity

import           Control.Monad.Reader.Class
import           Control.Monad.Weighted.Class
import           Control.Monad.Writer.Class
import           Control.Monad.Cont.Class
import           Control.Monad.Error.Class

import           Control.Monad.State.Strict

import           Data.Coerce

import           Data.Functor.Classes

import           Data.Monoid
import           Data.Semiring

-- | A monad transformer similar to 'Control.Monad.Writer.Strict.WriterT', except
-- that it does not leak space. It is implemented using a state monad, so that
-- `mappend` is tail recursive. See
-- <https://mail.haskell.org/pipermail/libraries/2013-March/019528.html this>
-- email to the Haskell libraries committee for more information.
--
-- It also uses '<.>' from 'Semiring', rather than 'mappend' from 'Monoid' when
-- combining computations.
--
-- Wherever possible, coercions are used to eliminate any overhead from the
-- newtype wrapper.
newtype WeightedT s m a =
    WeightedT_ (StateT s m a)
    deriving (Functor,Applicative,Monad,MonadTrans,MonadCont,MonadError e,MonadReader r,MonadFix,MonadFail,MonadIO,Alternative,MonadPlus,MonadWriter w)

-- | Run a weighted computation in the underlying monad.
runWeightedT
    :: Semiring s
    => WeightedT s m a -> m (a, s)
runWeightedT =
    (coerce :: (StateT s m a -> m (a, s)) -> WeightedT s m a -> m (a, s))
        (`runStateT` one)

{-# INLINE runWeightedT #-}

{-# ANN module "HLint: ignore Use second" #-}

-- | This pattern gives the newtype wrapper around 'StateT' the same interface
-- as 'Control.Monad.Writer.Strict.WriterT'. Unfortunately, GHC currently warns
-- that a function is incomplete wherever this pattern is used. This issue
-- should be solved in a future version of GHC, when the
-- <https://ghc.haskell.org/trac/ghc/ticket/8779 COMPLETE> pragma is
-- implemented.
pattern WeightedT :: (Functor m, Semiring s) =>
        m (a, s) -> WeightedT s m a

pattern WeightedT x <- (runWeightedT -> x)
  where WeightedT y
          = WeightedT_ (StateT (\ s -> fmap (\ (x, p) -> (x, s <.> p)) y))

-- | A type synonym for the plain (non-transformer) version of 'Weighted'. This
-- can be used as if it were defined as:
--
-- > newtype Weighted w a = Weighted { runWeighted :: (a, w) }
type Weighted s = WeightedT s Identity

-- | This pattern gives the newtype wrapper around 'StateT' the same interface
-- as as if it was defined like so:
--
-- > newtype Weighted w a = Weighted { runWeighted :: (a, w) }
--
-- Unfortunately GHC warns that a function is incomplete wherever this pattern
-- is used. This issue should be solved in a future version of GHC, when the
-- <https://ghc.haskell.org/trac/ghc/ticket/8779 COMPLETE> pragma is
-- implemented.
--
-- >>> execWeighted $ traverse (\x -> Weighted ((), x)) [1..5]
-- 120
pattern Weighted :: Semiring s => (a, s) -> Weighted s a

pattern Weighted x <- (runWeighted -> x)
  where Weighted (y, p)
          = WeightedT_ (StateT (\ s -> Identity (y, (<.>) p s)))

-- | Run a weighted computation.
--
-- >>> runWeighted $ traverse (\x -> Weighted (show x, x)) [1..5]
-- (["1","2","3","4","5"],120)
runWeighted
    :: Semiring s
    => Weighted s a -> (a, s)
runWeighted =
    (coerce :: (WeightedT s Identity a -> Identity (a, s)) -> (WeightedT s Identity a -> (a, s)))
        runWeightedT

{-# INLINE runWeighted #-}

instance MonadState s m =>
         MonadState s (WeightedT w m) where
    get = lift get
    put = lift . put
    state = lift . state

-- | Run a weighted computation in the underlying monad, and return its result.
evalWeightedT
    :: (Monad m, Semiring s)
    => WeightedT s m a -> m a
evalWeightedT =
    (coerce :: (StateT s m a -> m a) -> WeightedT s m a -> m a)
        (`evalStateT` one)

{-# INLINE evalWeightedT #-}

-- | Run a weighted computation in the underlying monad, and collect its weight.
execWeightedT
    :: (Monad m, Semiring s)
    => WeightedT s m a -> m s
execWeightedT =
    (coerce :: (StateT s m a -> m s) -> WeightedT s m a -> m s)
        (`execStateT` one)

{-# INLINE execWeightedT #-}

-- | Run a weighted computation, and return its result.
evalWeighted
    :: Semiring s
    => Weighted s a -> a
evalWeighted =
    (coerce :: (State s a -> a) -> Weighted s a -> a) (`evalState` one)

{-# INLINE evalWeighted #-}

-- | Run a weighted computation, and collect its weight.
execWeighted
    :: Semiring s
    => Weighted s a -> s
execWeighted =
    (coerce :: (State s a -> s) -> Weighted s a -> s) (`execState` one)

{-# INLINE execWeighted #-}

instance (Foldable m, Semiring w) =>
         Foldable (WeightedT w m) where
    foldMap f =
        foldMap
            (\(x,_) ->
                  f x) .
        runWeightedT

first_
    :: Applicative f
    => (a -> f b) -> (a, c) -> f (b, c)
first_ f (x,y) = flip (,) y <$> f x

instance (Traversable m, Semiring w) =>
         Traversable (WeightedT w m) where
    traverse f x = WeightedT <$> (traverse . first_) f (runWeightedT x)

instance (Eq1 m, Eq w, Semiring w) =>
         Eq1 (WeightedT w m) where
    liftEq eq x y =
        liftEq
            (\(xx,xy) (yx,yy) ->
                  eq xx yx && xy == yy)
            (runWeightedT x)
            (runWeightedT y)

instance (Ord1 m, Ord w, Semiring w) =>
         Ord1 (WeightedT w m) where
    liftCompare cmp x y =
        liftCompare
            (\(xx,xy) (yx,yy) ->
                  cmp xx yx <> compare xy yy)
            (runWeightedT x)
            (runWeightedT y)

instance (Read w, Read1 m, Semiring w, Functor m) =>
         Read1 (WeightedT w m) where
    liftReadsPrec rp rl =
        readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "WeightedT" WeightedT
      where
        rp' = liftReadsPrec2 rp rl readsPrec readList
        rl' = liftReadList2 rp rl readsPrec readList

instance (Show w, Show1 m, Semiring w) =>
         Show1 (WeightedT w m) where
    liftShowsPrec sp sl d m =
        showsUnaryWith (liftShowsPrec sp' sl') "WeightedT" d (runWeightedT m)
      where
        sp' = liftShowsPrec2 sp sl showsPrec showList
        sl' = liftShowList2 sp sl showsPrec showList

instance (Eq w, Eq1 m, Eq a, Semiring w) =>
         Eq (WeightedT w m a) where
    (==) = eq1

instance (Ord w, Ord1 m, Ord a, Semiring w) =>
         Ord (WeightedT w m a) where
    compare = compare1

instance (Read w, Read1 m, Read a, Semiring w, Functor m) =>
         Read (WeightedT w m a) where
    readsPrec = readsPrec1

instance (Show w, Show1 m, Show a, Semiring w) =>
         Show (WeightedT w m a) where
    showsPrec = showsPrec1

instance (Semiring w, Monad m) =>
         MonadWeighted w (WeightedT w m) where
    weighted (x,s) = WeightedT (pure (x, s))
    {-# INLINE weighted #-}
    weigh (WeightedT_ s) = WeightedT_ ((,) <$> s <*> get)
    {-# INLINE weigh #-}
    scale (WeightedT_ s) = WeightedT_ (scaleS s)
      where
        scaleS = (=<<) (uncurry (<$) . fmap modify)
    {-# INLINE scale #-}
