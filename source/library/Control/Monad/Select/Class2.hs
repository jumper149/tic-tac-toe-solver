{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Select.Class2 where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control
import Control.Monad.Trans.Elevator
import Control.Monad.Trans.Select
import Data.Kind

type MonadSelect :: Type -> (Type -> Type) -> Constraint
class (Monad m) => MonadSelect r m | m -> r where
  select' :: ((a -> m r) -> m a) -> m a

instance Monad m => MonadSelect r (SelectT r m) where
  select' f = SelectT $ \k -> runSelectT (f (lift . k)) k

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via
  Elevator t1 (t2 (m :: Type -> Type))
  instance
  {-# OVERLAPPABLE #-}
    ( MonadSelect r (t2 m)
    , MonadTransControl t1
    ) =>
    MonadSelect r (ComposeT t1 t2 m)

instance (MonadSelect r m, MonadTransControl t) => MonadSelect r (Elevator t m) where
  select' f = (restoreT . pure =<<) $ liftWith $ \runT ->
    select' $ \k -> runT $ f $ \a -> lift . k =<< runT (pure a)

-- | Set by 'SelectT'.
deriving via
  SelectT r (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) =>
    MonadSelect r (ComposeT (SelectT r) t2 m)

-- instance (MonadTransControlIdentity t, MonadSelect r m) => MonadSelect r (Elevator t m) where
--  select' f = liftWithIdentity $ \runT -> select' $ \k -> runT $ f (lift . k)
