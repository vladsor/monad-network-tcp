{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Network.Ether.Monad
       ( TcpT
       , Tcp
       , runConnection
       ) where

import Prelude hiding (mapM, mapM_)
import qualified Network.Monad as T

import qualified Control.Monad.Network.Class as T (MonadConnection(..))
import qualified Control.Monad.Ether.Network.Class as E (MonadConnection(..))

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Catch

import qualified Network.Socket as NS hiding (recv, send)

import Control.Monad.Trans.Ether.Tagged
import Data.Functor.Identity (Identity(..))

type TcpT tag m = TaggedTrans tag T.TcpT m
type Tcp tag = TcpT tag Identity

instance (MonadIO m, MonadThrow m) => E.MonadConnection tag (TcpT tag m) where
  send _ = pack . T.send
  recv _ = pack . T.recv
  reconnect _ = pack T.reconnect

runConnection :: (MonadIO m, MonadThrow m) => proxy tag -> NS.AddrInfo -> TcpT tag m a -> m a
runConnection _ addr conn = T.runConnection addr (unpack conn)
