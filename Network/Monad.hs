{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

-- #define DEBUG 1

module Network.Monad
       ( Tcp
       , TcpT
       , runConnection
       ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Morph         (MFunctor)
import           Control.Monad.Network.Class (MonadConnection (..))
import           Control.Monad.State.Strict  (StateT)
import qualified Control.Monad.State.Strict  as ST
import           Control.Monad.Trans
import           Control.Monad.Trans.Class   (lift)
import qualified Data.ByteString             as S
import           Data.Typeable
import           GHC.Generics                (Generic)
import qualified Network.Socket              as NS hiding (recv, send)
import qualified Network.Socket.ByteString   as NS
import           Prelude                     hiding (mapM, mapM_)

data ConnState = ConnState { ncSocket :: NS.Socket, ncAddr :: NS.AddrInfo }

newtype TcpT m a = TcpT { connStateT :: StateT ConnState m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MFunctor)

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
  r <- liftIO $ mask $ \unm -> unm (Right <$> io) `catch` (\(e :: SomeException) -> return (Left (show e)))
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
#ifdef DEBUG
    liftIO $ print $ "sending:" ++ (show $ S.length bs)
#endif
    lift $ rethrow TcpExceptionSend (NS.send s bs)
    return ()
  recv len = TcpT $ do
    ConnState s _ <- ST.get
#ifdef DEBUG
    liftIO $ print $ "receiving:" ++ (show len)
#endif
    lift $ rethrow TcpExceptionReceive (NS.recv s len)
  reconnect = TcpT $ do
    ConnState s addr <- ST.get
    lift $ rethrow TCpExceptionDisconnect $ NS.close s
    ST.StateT $ const $ ((),) <$> mkConnState addr

runConnection :: (MonadIO m, MonadThrow m) => NS.AddrInfo -> TcpT m a -> m a
runConnection addr conn = do
    st <- mkConnState addr
    (v, ConnState s _) <- ST.runStateT (connStateT conn) st
    rethrow TCpExceptionDisconnect $ NS.close s
    return v
