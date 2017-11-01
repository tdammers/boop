{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE TypeOperators #-}
{-#LANGUAGE FunctionalDependencies #-}

module Control.OOP.MonadVar
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Proxy

-- | Generalized read-only access to mutable variables
class Monad m => MonadConstVar v m | m -> v where
  readVar :: v a -> m a

-- | Generalized mutable variables
class (Monad m, MonadConstVar v m) => MonadVar v m | m -> v where
  newVar :: a -> m (v a)
  writeVar :: a -> v a -> m ()

  modifyVar :: (a -> a) -> v a -> m ()
  swapVar :: a -> v a -> m a
  modifySwapVar :: (a -> a) -> v a -> m a

  transaction :: Proxy v -> m a -> m a

  modifyVar f var =
    modifySwapVar f var >> return ()

  swapVar val' =
    modifySwapVar (const val')

  modifySwapVar f var = do
    val <- readVar var
    writeVar (f val) var
    return val

instance MonadIO m => MonadConstVar IORef m where
  readVar = liftIO . readIORef
  
instance MonadIO m => MonadVar IORef m where
  newVar = liftIO . newIORef
  writeVar v = liftIO . flip writeIORef v
  transaction _ = id

