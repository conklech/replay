{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Replay where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Monoid
import Data.Foldable
import Data.Functor.Identity
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Data

{- Instances for the following types may be possible.
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.State.Class
import Control.Monad.Error.Class
import Control.Monad.Cont.Class
import Data.Functor.Bind -}

{- I don't think Traversable is possible. See below.
import Data.Traversable -}


data RF x f a b = Pure a | LiftR (x -> b) (f x)
  deriving (
#if __GLASGOW_HASKELL__ >= 707
           Typeable
#endif
           )

instance Functor f => Functor (RF x f a) where
  fmap _ (Pure a) = Pure a
  fmap f (LiftR g bs) = LiftR (f . g) bs

instance Foldable f => Foldable (RF x f a) where
  foldMap f (LiftR g as) = foldMap (f . g) as
  foldMap _ _            = mempty
  {-# INLINE foldMap #-}

{-
instance Traversable f => Traversable (RF x f a) where
  traverse _ (Pure a)  = pure (Pure a)
  traverse f (LiftR g as) = _ <$> traverse (f . g) as
  {-# INLINE traverse #-}
-}

instance Functor f => Bifunctor (RF x f) where
  bimap f _ (Pure a)     = Pure (f a)
  bimap _ g (LiftR k as) = LiftR (g . k) as
  {-# INLINE bimap #-}

instance Foldable f => Bifoldable (RF x f) where
  bifoldMap f _ (Pure a)     = f a
  bifoldMap _ g (LiftR k as) = foldMap (g . k) as
  {-# INLINE bifoldMap #-}

newtype ReplayT x m n a = ReplayT { runReplayT :: n (RF x m a (ReplayT x m n a)) }

instance (Functor f, Monad m) => Functor (ReplayT x f m) where
  fmap f (ReplayT m) = ReplayT (liftM f' m) where
    f' (Pure a)  = Pure (f a)
    f' (LiftR g bs) = LiftR (fmap f . g) bs
  {-# INLINE fmap #-}

instance (Functor f, Monad m) => Applicative (ReplayT x f m) where
  pure a = ReplayT (return (Pure a))
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}
  
instance (Functor f, Monad m) => Monad (ReplayT x f m) where
  return a = ReplayT (return (Pure a))
  {-# INLINE return #-}
  ReplayT m >>= f = ReplayT $ m >>= \v -> case v of
    Pure a -> runReplayT (f a)
    LiftR g w -> return (LiftR (fmap (>>= f) g) w )

instance MonadTrans (ReplayT x f) where
  lift = ReplayT . liftM Pure
  {-# INLINE lift #-}

instance (Functor f, MonadIO m) => MonadIO (ReplayT x f m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

-- | Replay an effect log and then perform at most one layer of computation in 'm'.
-- 
-- In this example, the replayable monad is '(->) String', i.e. the reader monad.
--
-- >>> let m = (record (id :: String -> String) >>= lift . putStrLn)
-- >>> :t m
-- m :: ReplayT String ((->) String) IO ()
-- >> 
-- >>> let layer0 = replay [] m
-- >>> :t layer0
-- layer0 :: IO (Either () (String -> [String]))
-- >>> Right effect1 <- layer0
-- >>> :t effect1
-- effect0 :: (String -> [String])
-- >>> let log0 = effect0 "First input"
-- >>> log0
-- ["Input 1"]
-- >>> let layer1 = replay log0 m
-- >>> r <- layer1
-- Input 1
-- >>> :force r
-- r = Left ()
replay :: (Functor m, Functor n, Monad m, Monad n) 
       => [x] 
       -> ReplayT x m n a 
       -> n (Either a (m [x]))
replay l (ReplayT n) = do
  val <- n
  case val of
    Pure x -> return $ Left x
    LiftR g x -> case l of
      (v : vs) -> (fmap . fmap . fmap) (v :) $ replay vs (g v)
      [] -> return . Right $ (: []) <$> x

-- | Replay an effect log, and then tear down the rest of the monad using iteration.
--
-- This function is analogous to 'iterT' from 'Control.Monad.Trans.Free'. When an empty
-- log is provided, it should be equivalent to 'iterT'.
replayAndContinue :: (Functor f, Monad m) => (f (m a) -> m a) -> [x] -> ReplayT x f m a -> m a
replayAndContinue f l (ReplayT m) = do
    val <- m
    case val of
      LiftR g _ -> 
        case l of
          (v : vs) -> replayAndContinue f vs (g v)
          [] -> go l val
      _ -> go l val
  where
    go l v = 
      case fmap (replayAndContinue f l) v of
        Pure x -> return x
        LiftR g x -> f (fmap g x)

-- | Create a recorded effect.
record :: (Monad m, Functor f) => f x -> ReplayT x f m x
record f = ReplayT . return $ LiftR return f
