{-# LANGUAGE CPP                        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Monad.TCP.Internal
       ( ConnState(..)
       , TcpException(..)
       , mkConnState
       , send
       , recv
       , reconnect
       , rethrow
       ) where

-- #define DEBUG 1

import Control.Monad (when)
import           Control.Monad.State.Strict  (StateT)
import qualified Control.Monad.State.Strict  as ST
import Network.Socket              (Socket, AddrInfo(..), socket, connect, close)
import qualified Network.Socket.ByteString   as NS
import           Control.Monad.Catch
import Data.Typeable
import qualified Data.ByteString as S
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Trans.Class   (lift)

data ConnState = ConnState { ncSocket :: Socket, ncAddr :: AddrInfo }

data TcpException = TcpExceptionConnect String
                  | TcpExceptionDisconnect String
                  | TcpExceptionSend String
                  | TcpExceptionReceive String
                  deriving (Show, Typeable)

instance Exception TcpException

rethrow :: (MonadThrow m, MonadIO m) => (String -> TcpException) -> IO a -> m a
rethrow thr io = do
  r <- liftIO $ mask $ \unm -> unm (Right <$> io) `catch` (\(e :: SomeException) -> return (Left (show e)))
  case r of
    Right r' -> return r'
    Left exc -> throwM (thr exc)

mkConnState :: (MonadIO m, MonadThrow m) => AddrInfo -> m ConnState
mkConnState addr@AddrInfo{..} = do
  s <- rethrow TcpExceptionConnect $ socket addrFamily addrSocketType addrProtocol
  rethrow TcpExceptionConnect $ connect s addrAddress
  return $ ConnState s addr

send :: (MonadIO m, MonadThrow m, Monad m) => S.ByteString -> StateT ConnState m ()
send bs = do
    ConnState s _ <- ST.get
#ifdef DEBUG
    liftIO $ print $ "sending:" ++ (show $ S.length bs)
#endif
    len <- lift $ rethrow TcpExceptionSend (NS.send s bs)
    when (len /= S.length bs) $ throwM (TcpExceptionSend "")

recv :: (MonadIO m, MonadThrow m, Monad m) => Int -> StateT ConnState m S.ByteString
recv len = do
    ConnState s _ <- ST.get
#ifdef DEBUG
    liftIO $ print $ "receiving:" ++ (show len)
#endif
    lift $ rethrow TcpExceptionReceive (NS.recv s len)

reconnect :: (MonadIO m, MonadThrow m, Monad m) => StateT ConnState m ()
reconnect = do
    ConnState s addr <- ST.get
    lift $ rethrow TcpExceptionDisconnect $ close s
    ST.StateT $ const $ ((),) <$> mkConnState addr
