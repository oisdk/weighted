{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}

module Control.Monad.Weighted.Filter
  (catchZero
  ,FilterT
  ,pattern FilterT
  ,runFilterT
  ,evalFilterT
  ,execFilterT)
  where

import           Data.Semiring

import           Data.Functor.Classes

import           Control.Applicative

import           Control.Monad.State.Strict

import           Control.Monad.Cont.Class
import           Control.Monad.Error.Class
import           Control.Monad.Fail
import           Control.Monad.Reader.Class
import           Control.Monad.Weighted.Class
import           Control.Monad.Writer.Class

import           Control.Arrow                (first)
import           Data.Coerce
import           Data.Monoid

catchZero :: (DetectableZero s, Alternative m) => (s -> m a) -> s -> m a
catchZero f s | isZero s = empty
              | otherwise = f s

remZeroes :: (DetectableZero s, Alternative m, Monad m) => m (a, s) -> m (a, s)
remZeroes xs = xs >>=  (\(x,p) -> if isZero p then empty else pure (x,p))

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
    {-# INLINE (>>=) #-}

runFilterT
    :: (DetectableZero s, Alternative m, Monad m)
    => FilterT s m a -> m (a, s)
runFilterT = remZeroes .
    (coerce :: (StateT s m a -> m (a, s)) -> FilterT s m a -> m (a, s))
        (`runStateT` one)

{-# INLINE runFilterT #-}

evalFilterT
    :: (Monad m, Semiring s)
    => FilterT s m a -> m a
evalFilterT =
    (coerce :: (StateT s m a -> m a) -> FilterT s m a -> m a)
        (`evalStateT` one)

{-# INLINE evalFilterT #-}

execFilterT
    :: (Monad m, Semiring s)
    => FilterT s m a -> m s
execFilterT =
    (coerce :: (StateT s m a -> m s) -> FilterT s m a -> m s)
        (`execStateT` one)

{-# INLINE execFilterT #-}

pattern FilterT :: (Alternative m, DetectableZero s, Monad m) => m (a, s) -> FilterT s m a
pattern FilterT x <- (runFilterT -> x)
  where FilterT y = FilterT_ . StateT . catchZero $ \s -> (fmap.fmap) (s<.>) y

instance (DetectableZero w, Monad m, Alternative m) => MonadWeighted w (FilterT w m) where
    weighted (x,s) = FilterT (pure (x, s))
    {-# INLINE weighted #-}
    weigh (FilterT_ s) = FilterT_ ((,) <$> s <*> get)
    {-# INLINE weigh #-}
    scale (FilterT_ s) = FilterT_ (scaleS s)
      where
        scaleS = (=<<) (uncurry (<$) . fmap modify)
    {-# INLINE scale #-}

instance (Foldable m, DetectableZero w, Alternative m, Monad m) =>
         Foldable (FilterT w m) where
    foldMap f =
        foldMap
            (\(x,p) ->
                  if isZero p
                      then mempty
                      else f x) .
        runFilterT

first_
    :: Applicative f
    => (a -> f b) -> (a, c) -> f (b, c)
first_ f (x,y) = flip (,) y <$> f x

instance (Traversable m, DetectableZero w, Alternative m, Monad m) =>
         Traversable (FilterT w m) where
    traverse f x = FilterT <$> (traverse . first_) f (runFilterT x)

instance (Eq1 m, Eq w, DetectableZero w, Monad m, Alternative m) =>
         Eq1 (FilterT w m) where
    liftEq eq x y =
        liftEq
            (\(xx,xy) (yx,yy) ->
                  eq xx yx && xy == yy)
            (runFilterT x)
            (runFilterT y)

instance (Ord1 m, Ord w, DetectableZero w, Monad m, Alternative m) =>
         Ord1 (FilterT w m) where
    liftCompare cmp x y =
        liftCompare
            (\(xx,xy) (yx,yy) ->
                  cmp xx yx <> compare xy yy)
            (runFilterT x)
            (runFilterT y)

instance (Read w, Read1 m, DetectableZero w, Alternative m, Monad m) =>
         Read1 (FilterT w m) where
    liftReadsPrec rp rl =
        readsData $ readsUnaryWith (liftReadsPrec rp' rl') "FilterT" FilterT
      where
        rp' = liftReadsPrec2 rp rl readsPrec readList
        rl' = liftReadList2 rp rl readsPrec readList

instance (Show w, Show1 m, DetectableZero w, Monad m, Alternative m) =>
         Show1 (FilterT w m) where
    liftShowsPrec sp sl d m =
        showsUnaryWith (liftShowsPrec sp' sl') "FilterT" d (runFilterT m)
      where
        sp' = liftShowsPrec2 sp sl showsPrec showList
        sl' = liftShowList2 sp sl showsPrec showList

instance (Eq w, Eq1 m, Eq a, DetectableZero w, Monad m, Alternative m) =>
         Eq (FilterT w m a) where
    (==) = eq1

instance (Ord w, Ord1 m, Ord a, DetectableZero w, Monad m, Alternative m) =>
         Ord (FilterT w m a) where
    compare = compare1

instance (Read w, Read1 m, Read a, DetectableZero w, Alternative m, Monad m) =>
         Read (FilterT w m a) where
    readsPrec = readsPrec1

instance (Show w, Show1 m, Show a, DetectableZero w, Alternative m, Monad m) =>
         Show (FilterT w m a) where
    showsPrec = showsPrec1

instance (Alternative m, MonadState s m, DetectableZero w) =>
         MonadState s (FilterT w m) where
    get = lift get
    put = lift . put
    state = lift . state
