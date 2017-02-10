{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}

module Control.Monad.Weighted.Class where

import qualified Control.Monad.Trans.Identity as Identity
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.State.Strict as StateStrict
import qualified Control.Monad.Trans.State.Lazy as StateLazy
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Trans.Reader as Reader

import Control.Monad.Trans (lift)

import Data.Semiring

import Data.Coerce

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

collect :: (Foldable m, MonadWeighted w m) => m a -> w
collect = getAdd  #. foldMap (Add #. snd) . weigh

infixr 9 #.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(#.) _ = coerce
