{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Ether.Monad.TLS
       ( TlsT
       , runConnection
       ) where

import Prelude hiding (mapM, mapM_)

import qualified Control.Monad.Network.Class as T (MonadConnection(..))
import qualified Control.Monad.Ether.Network.Class as E (MonadConnection(..))

import Control.Monad.State.Strict (StateT)

import qualified Network.Monad.TLS as T

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Catch

import Control.Monad.Trans.Ether.Tagged
import qualified Network.TLS as TLS
import Control.Monad.Trans.Class

newtype TlsT tag l a = TlsT { runTlsT :: TaggedTrans tag T.TlsT l a }
                       deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadTrans)

instance (T.MonadConnection l, MonadMask l, MonadIO l) => E.MonadConnection tag (TlsT tag l) where
  send _ = TlsT . pack . T.send
  recv _ = TlsT . pack . T.recv
  reconnect _ = TlsT $ pack T.reconnect

runConnection :: (T.MonadConnection l, MonadMask l, MonadIO l) => proxy tag -> TLS.ClientParams l -> TlsT tag l a -> l a
runConnection tag params conn = T.runConnection params (unpack $ runTlsT conn)
