{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Network.Monad.TCP
       ( Tcp
       , TcpT
       , runConnection
       , TcpException(..)
       ) where

import           Prelude                     hiding (mapM, mapM_)
import           Control.Monad.Catch
import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Trans.Resource (MonadResource)
import           Control.Monad.Network.Class (Connection(..))
import           Control.Monad.State.Strict  (StateT)
import qualified Control.Monad.State.Strict  as ST
import           Control.Monad.Trans
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Fail
import           Data.Typeable
import           GHC.Generics                (Generic)
import           Network.Socket              (AddrInfo, close)
import           Control.Monad.Morph              (MFunctor)

import Network.Monad.TCP.Internal
import qualified Network.Monad.TCP.Internal as Internal

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
    rethrow TcpExceptionDisconnect $ close s
    return v
