-- Haskell StateT monad 
module StateMonad where

data StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Monad m) => Monad (StateT s m) where
  return a = StateT $ \ s -> return (a, s)
  m >>= k  = StateT $ \ s -> do
    ~(a, s') <- runStateT m s
    runStateT (k a) s'

instance Monad m => Functor (StateT s m) where
  fmap = liftM

class MonadTrans t where
  lift :: (Monad m) => m a -> t m a

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> do
              a <- m
              return (a, s)

get :: (Monad m) => StateT s m s
get = StateT $ \s -> return (s, s)

gets :: (Monad m) => (s -> a) -> StateT s m a
gets f = StateT $ \s -> return (f s, s)

put :: (Monad m) => s -> StateT s m ()
put x = StateT $ \_ -> return ((), x)

modify :: (Monad m) => (s -> s) -> StateT s m ()
modify f = get >>= \s -> put (f s)

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)
