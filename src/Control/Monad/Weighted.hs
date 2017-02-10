{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE UndecidableInstances       #-}
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
  ,evalWeighted
  ,MonadWeighted(..)
  ,FilterT
  ,pattern FilterT
  ,runFilterT)
  where


import           Control.Applicative
import           Control.Monad.Identity
import           Control.Monad.State.Strict

import           Control.Monad.Cont.Class
import           Control.Monad.Error.Class
import           Control.Monad.Fail
import           Control.Monad.Reader.Class
import           Control.Monad.Writer.Class

import qualified Control.Monad.Trans.Identity as Identity
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.State.Strict as StateStrict
import qualified Control.Monad.Trans.State.Lazy as StateLazy
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Trans.Reader as Reader

import           Data.Coerce
import           Data.Functor.Classes

import           Data.Monoid
import           Data.Semiring

import           Control.Arrow (first)

catchZero :: (DetectableZero s, Alternative m) => (s -> m a) -> s -> m a
catchZero f s | isZero s = empty
              | otherwise = f s

-- | A monad transformer similar to 'WriterT', except that it does not leak
-- space, and it uses the 'Semiring' class, rather than 'Monoid'.
newtype WeightedT s m a =
    WeightedT_ (StateT s m a)
    deriving (Functor,Applicative,Monad,MonadTrans,MonadCont,MonadError e
             ,MonadReader r,MonadFix,MonadFail,MonadIO,Alternative,MonadPlus
             ,MonadWriter w)

-- | Discards results which are zero
newtype FilterT s m a =
    FilterT_ { unFilterT :: StateT s m a }
    deriving (MonadTrans,MonadCont,MonadError e
             ,MonadReader r,MonadFix,MonadFail,MonadIO,Alternative,MonadPlus
             ,MonadWriter w)

instance (Alternative m, DetectableZero s) => Functor (FilterT s m) where
    fmap f (FilterT_ (StateT st)) =
        (FilterT_ . StateT . catchZero) ((fmap . first) f . st)
    {-# INLINE fmap #-}

instance (Alternative m, Monad m, DetectableZero s) =>
         Applicative (FilterT s m) where
    pure x = (FilterT_ . StateT) (catchZero (pure . (,) x))
    {-# INLINE pure #-}
    FilterT_ (StateT fs) <*> FilterT_ (StateT xs) =
        FilterT_ . StateT . catchZero $
        \s -> do
            (f,s') <- fs s
            (x,s'') <- catchZero xs s'
            catchZero
                (\s''' ->
                      pure (f x, s'''))
                s''
    {-# INLINE (<*>) #-}

instance (Alternative m, Monad m, DetectableZero s) =>
         Monad (FilterT s m) where
    FilterT_ (StateT st) >>= f =
        FilterT_ . StateT . catchZero $
        \s -> do
            (x,s') <- st s
            (y,s'') <- catchZero (runStateT (unFilterT (f x))) s'
            catchZero
                (\s''' ->
                      pure (y, s'''))
                s''

runWeightedT
    :: Semiring s
    => WeightedT s m a -> m (a, s)
runWeightedT =
    (coerce :: (StateT s m a -> m (a, s)) -> WeightedT s m a -> m (a, s))
        (`runStateT` one)
{-# INLINE runWeightedT #-}

runFilterT
    :: Semiring s
    => FilterT s m a -> m (a, s)
runFilterT =
    (coerce :: (StateT s m a -> m (a, s)) -> FilterT s m a -> m (a, s))
        (`runStateT` one)

pattern FilterT :: (Alternative m, DetectableZero s) => m (a, s) -> FilterT s m a
pattern FilterT x <- (runFilterT -> x)
  where FilterT y = FilterT_ . StateT . catchZero $ \s -> (fmap.fmap) (s<.>) y

pattern WeightedT :: (Functor m, Semiring s) =>
        m (a, s) -> WeightedT s m a

pattern WeightedT x <- (runWeightedT -> x)
  where WeightedT y
          = WeightedT_ (StateT (\ s -> (fmap . fmap) (s<.>) y))

type Weighted s = WeightedT s Identity

pattern Weighted :: Semiring s => (a, s) -> Weighted s a

pattern Weighted x <- (runWeighted -> x)
  where Weighted (y, p)
          = WeightedT_ (StateT (\ s -> Identity (y, (<.>) p s)))

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

evalWeightedT
    :: (Monad m, Semiring s)
    => WeightedT s m a -> m a
evalWeightedT =
    (coerce :: (StateT s m a -> m a) -> WeightedT s m a -> m a)
        (`evalStateT` one)

{-# INLINE evalWeightedT #-}

execWeightedT
    :: (Monad m, Semiring s)
    => WeightedT s m a -> m s
execWeightedT =
    (coerce :: (StateT s m a -> m s) -> WeightedT s m a -> m s)
        (`execStateT` one)

{-# INLINE execWeightedT #-}

evalWeighted
    :: Semiring s
    => Weighted s a -> a
evalWeighted =
    (coerce :: (State s a -> a) -> Weighted s a -> a) (`evalState` one)

{-# INLINE evalWeighted #-}

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
        readsData $ readsUnaryWith (liftReadsPrec rp' rl') "WeightedT" WeightedT
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

class (Semiring w, Monad m) => MonadWeighted w m | m -> w where
    {-# MINIMAL (weighted | weight), weigh, scale #-}
    -- | @'weighted' (a,w)@ embeds a simple weighted action.
    weighted :: (a,w) -> m a
    weighted ~(a, w) = do
      weight w
      return a

    -- | @'weight' w@ is an action that produces the output @w@.
    weight   :: w -> m ()
    weight w = weighted ((),w)

    -- | @'weigh' m@ is an action that executes the action @m@ and adds
    -- its output to the value of the computation.
    weigh :: m a -> m (a, w)
    -- | @'scale' m@ is an action that executes the action @m@, which
    -- returns a value and a function, and returns the value, applying
    -- the function to the output.
    scale   :: m (a, w -> w) -> m a

instance (Semiring w, Monad m) => MonadWeighted w (WeightedT w m) where
    weighted (x,s) = WeightedT (pure (x, s))
    {-# INLINE weighted #-}
    weigh (WeightedT_ s) = WeightedT_ ((,) <$> s <*> get)
    {-# INLINE weigh #-}
    scale (WeightedT_ s) = WeightedT_ (scaleS s)
      where
        scaleS = (=<<) (uncurry (<$) . fmap modify)
    {-# INLINE scale #-}

instance MonadWeighted w m => MonadWeighted w (Except.ExceptT e m) where
    weighted = lift . weighted
    weight   = lift . weight
    weigh    = Except.liftListen weigh
    scale    = Except.liftPass scale

instance MonadWeighted w m => MonadWeighted w (Identity.IdentityT m) where
    weighted = lift . weighted
    weight   = lift . weight
    weigh    = Identity.mapIdentityT weigh
    scale    = Identity.mapIdentityT scale

instance MonadWeighted w m => MonadWeighted w (StateStrict.StateT s m) where
    weighted = lift . weighted
    weight   = lift . weight
    weigh    = StateStrict.liftListen weigh
    scale    = StateStrict.liftPass scale

instance MonadWeighted w m => MonadWeighted w (StateLazy.StateT s m) where
    weighted = lift . weighted
    weight   = lift . weight
    weigh    = StateLazy.liftListen weigh
    scale    = StateLazy.liftPass scale

instance MonadWeighted w m => MonadWeighted w (Maybe.MaybeT m) where
    weighted = lift . weighted
    weight   = lift . weight
    weigh    = Maybe.liftListen weigh
    scale    = Maybe.liftPass scale

instance MonadWeighted w m => MonadWeighted w (Reader.ReaderT r m) where
    weighted = lift . weighted
    weight   = lift . weight
    weigh    = Reader.mapReaderT weigh
    scale    = Reader.mapReaderT scale

