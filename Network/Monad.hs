{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Monad
       ( Tcp
       , TcpT
       , runConnection
       ) where

import Prelude hiding (mapM, mapM_)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Catch

import Control.Monad.Network.Class (MonadConnection(..))

import Control.Monad.State.Strict (StateT)
import qualified Control.Monad.State.Strict as ST

import qualified Network.Socket as NS hiding (recv, send)
import qualified Network.Socket.ByteString as NS
import qualified Data.ByteString as S
import GHC.Generics (Generic)

import Control.Monad.Trans
import Data.Typeable

data ConnState = ConnState { ncSocket :: NS.Socket, ncAddr :: NS.AddrInfo }

newtype TcpT m a = TcpT { connStateT :: StateT ConnState m a }
                        deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

type Tcp = TcpT IO

instance MonadTrans TcpT where
  lift = TcpT . lift

data TcpException = TcpExceptionConnect String
                  | TCpExceptionDisconnect String
                  | TcpExceptionSend String
                  | TcpExceptionReceive String
                  deriving (Show, Typeable)

instance Exception TcpException

rethrow thr io = do
  r <- liftIO $ do
    mask $ \unm -> unm (liftM Right io) `catch` (\(e :: SomeException) -> return (Left (show e)))
  case r of
    Right r' -> return r'
    Left exc -> throwM (thr exc)

mkConnState :: (MonadIO m, MonadThrow m) => NS.AddrInfo -> m ConnState
mkConnState addr = do
  s <- rethrow TcpExceptionConnect $ NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
  rethrow TcpExceptionConnect $ NS.connect s (NS.addrAddress addr)
  return $ ConnState s addr

instance (MonadIO m, MonadThrow m) => MonadConnection (TcpT m) where
  send bs = TcpT $ do
    ConnState s _ <- ST.get
    lift $ rethrow TcpExceptionSend (NS.send s bs)
    return ()
  recv len = TcpT $ do
    ConnState s _ <- ST.get
    r <- lift $ rethrow TcpExceptionReceive (NS.recv s len)
    return r
  reconnect = TcpT $ do
    ConnState s addr <- ST.get
    lift $ rethrow TCpExceptionDisconnect $ NS.close s
    ST.StateT $ const $ liftM ((),) $ mkConnState addr

runConnection :: (MonadIO m, MonadThrow m) => NS.AddrInfo -> TcpT m a -> m a
runConnection addr conn = do
    st <- mkConnState addr
    (v, ConnState s _) <- ST.runStateT (connStateT conn) st
    rethrow TCpExceptionDisconnect $ NS.close s
    return v
