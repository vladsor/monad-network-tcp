{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Network.Monad.TCP
       ( Tcp
       , TcpT
       , runConnection
       , runConnectionC
       , TcpException(..)
       ) where

import           Prelude                     hiding (mapM, mapM_)
import           Control.Monad.Catch
import           Control.Monad.Trans.Resource (MonadResource)
import           Control.Monad.Network.Class (Connection(..))
import           Control.Monad.State.Strict  (StateT)
import qualified Control.Monad.State.Strict  as ST
import           Control.Monad.Trans
import           Network.Socket              (AddrInfo, close)
import           Control.Monad.Morph              (MFunctor)
import Data.Conduit (ConduitT, transPipe)
import Data.Conduit.Lift (runStateC)

import Network.Monad.TCP.Internal
import qualified Network.Monad.TCP.Internal as Internal

import Control.Monad.Extra (whenJust)

newtype TcpT m a = TcpT { connStateT :: StateT ConnState m a }
    deriving ( Functor, Applicative, Monad, MonadFail, MonadIO, MonadThrow
             , MonadCatch, MonadMask, MFunctor, MonadTrans, MonadResource)

type Tcp = TcpT IO

instance (MonadIO m, MonadThrow m) => Connection (TcpT m) where
  send = TcpT . Internal.send
  recv = TcpT . Internal.recv
  reconnect = TcpT Internal.reconnect

runConnection :: (MonadIO m, MonadThrow m) => AddrInfo -> TcpT m a -> m a
runConnection addr conn = do
    st <- mkConnState addr
    (v, ConnState s _) <- ST.runStateT (connStateT conn) st
    whenJust s $ rethrow TcpExceptionDisconnect . close
    return v

runConnectionC :: (MonadIO m, MonadThrow m) => AddrInfo -> ConduitT i o (TcpT m) a -> ConduitT i o m a
runConnectionC addr conn = do
  st <- mkConnState addr
  (v, ConnState s _) <- runStateC st $ transPipe connStateT conn
  whenJust s $ rethrow TcpExceptionDisconnect . close
  return v
