{-# LANGUAGE DeriveDataTypeable #-}
module Replay where

import qualified Data.Aeson.Types as Aeson
import           Data.Aeson (FromJSON, ToJSON)

import Control.Applicative
import Control.Monad (liftM, MonadPlus(..), ap, join)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
-- import Control.Monad.Reader.Class
-- import Control.Monad.Writer.Class
-- import Control.Monad.State.Class
-- import Control.Monad.Error.Class
-- import Control.Monad.Cont.Class
-- import Data.Functor.Bind hiding (join)
import Data.Monoid
import Data.Foldable
import Data.Functor.Identity
-- import Data.Traversable
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Data

import Control.Monad.Trans.Reader (Reader, runReader, ask)

data RF x f a b = Pure a | LiftR (x -> b) (f x)
  deriving (Typeable)

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

newtype RT x m n a = RT { runRT :: n (RF x m a (RT x m n a)) }

instance (Functor f, Monad m) => Functor (RT x f m) where
  fmap f (RT m) = RT (liftM f' m) where
    f' (Pure a)  = Pure (f a)
    f' (LiftR g bs) = LiftR (fmap f . g) bs
  {-# INLINE fmap #-}

instance (Functor f, Monad m) => Applicative (RT x f m) where
  pure a = RT (return (Pure a))
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}
  
instance (Functor f, Monad m) => Monad (RT x f m) where
  return a = RT (return (Pure a))
  {-# INLINE return #-}
  RT m >>= f = RT $ m >>= \v -> case v of
    Pure a -> runRT (f a)
    LiftR g w -> return (LiftR (fmap (>>= f) g) w )

instance MonadTrans (RT x f) where
  lift = RT . liftM Pure
  {-# INLINE lift #-}

instance (Functor f, MonadIO m) => MonadIO (RT x f m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

stepRT :: (Functor m, Functor n, Monad m, Monad n) 
       => [x] 
       -> RT x m n a 
       -> n (Either a (m [x]))
stepRT l (RT n) = do
  val <- n
  case val of
    Pure x -> return $ Left x
    LiftR g x -> case l of
      (v : vs) -> (fmap . fmap . fmap) (v :) $ stepRT vs (g v)
      [] -> return . Right $ (: []) <$> x
  
iterT :: (Functor f, Monad m) => (f (m a) -> m a) -> [x] -> RT x f m a -> m a
iterT f l (RT m) = do
    val <- m
    case val of
      LiftR g _ -> 
        case l of
          (v : vs) -> iterT f vs (g v)
          [] -> go l val
      _ -> go l val
  where
    go l v = 
      case fmap (iterT f l) v of
        Pure x -> return x
        LiftR g x -> f (fmap g x)

liftR :: (Monad m, Functor f) => f x -> RT x f m x
liftR f = RT . return $ LiftR return f

liftR' :: (Monad m, Functor f, FromJSON x, ToJSON x) => f x -> RT Aeson.Value f m (Maybe x)
liftR' f = fmap (Aeson.parseMaybe Aeson.parseJSON) . liftR $ (fmap Aeson.toJSON f)

test :: Monad m => RT Aeson.Value (Reader Bool) m Bool
test = do
  Just i <- liftR' ask
  if i then do
         Just i' <- liftR' ask
         return $ not i'
       else return True
