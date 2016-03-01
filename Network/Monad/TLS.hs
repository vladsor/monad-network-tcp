{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Monad.TLS
       ( TlsT
       , runConnection
       ) where

import Prelude hiding (mapM, mapM_)

import Data.Monoid ((<>))

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Monad.Catch

import Control.Monad.Network.Class (MonadConnection(..))

import Control.Monad.State.Strict (StateT)
import qualified Control.Monad.State.Strict as ST

import qualified Network.TLS as TLS
import qualified Network.TLS.Backend as TLS
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Data.Typeable (Typeable)

data ConnState l = TLSConnState { tlsCtx :: TLS.Context l, tlsBuffer :: S.ByteString }
newtype TlsT l a = TlsT { connState :: StateT (ConnState l) l a }
                        deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance MonadTrans TlsT where
  lift = TlsT . lift

data TlsException = TlsExceptionSend String
                  | TlsExceptionReceive String
                  | TlsExceptionBye String
                  | TlsExceptionLowerReconnect String
                  | TlsExceptionHandshake String
                  | TlsExceptionCreateContext String
                  deriving (Show, Typeable)

instance Exception TlsException

rethrow thr io = do
  r <- lift (liftM Right io) `catch` (\(e :: SomeException) -> return $ Left $ show e)
  case r of
    Right r' -> return r'
    Left e -> throwM (thr e)

tlsSend :: (MonadConnection l, MonadIO l, MonadCatch l) => S.ByteString -> TlsT l ()
tlsSend bs = TlsT $ do
    TLSConnState ctx _ <- ST.get
    rethrow TlsExceptionSend $ TLS.sendData ctx $ L.fromStrict bs
    return ()

tlsRecv :: (MonadConnection l, MonadIO l, MonadMask l) => Int -> TlsT l S.ByteString
tlsRecv len = TlsT $ do
    TLSConnState ctx buf <- ST.get
    (res, buf') <- if S.length buf >= len
      then return (S.take len buf, S.drop len buf)
      else rethrow TlsExceptionReceive $ recv' ctx buf

    ST.put $ TLSConnState ctx buf'
    return $ res
    where
      recv' ctx acc | S.length acc > len = return (S.take len acc, S.drop len acc)
      recv' ctx acc = do
        bs <- TLS.recvData ctx
        if S.null bs then return (acc, S.empty)
        else recv' ctx (acc <> bs)

tlsRecconnect :: (MonadConnection l, MonadIO l, MonadMask l) => TlsT l ()
tlsRecconnect = TlsT $ do
    TLSConnState ctx _ <- ST.get
    rethrow TlsExceptionBye $ TLS.bye ctx
    rethrow TlsExceptionLowerReconnect reconnect
    rethrow TlsExceptionHandshake $ TLS.handshake ctx

instance (MonadConnection l, MonadIO l, MonadMask l) => MonadConnection (TlsT l) where
  send = tlsSend
  recv = tlsRecv
  reconnect = tlsRecconnect

data Backend = Backend
instance (MonadConnection l, MonadIO l) => TLS.HasBackend Backend l where
    initializeBackend _ = return ()
    getBackend _ = TLS.Backend (return ()) (return ()) send (recvAll S.empty)
      where
        recvAll acc len | len == 0 = return acc
        recvAll acc len = do
                            b <- recv len
                            if S.length b > 0 then recvAll (acc <> b) (len - S.length b)
                            else return acc

runConnection :: (MonadConnection l, MonadIO l, MonadMask l) => TLS.ClientParams l -> TlsT l a -> l a
runConnection params conn = do
    ctx <- rethrow' TlsExceptionCreateContext $ TLS.contextNew Backend params
    rethrow' TlsExceptionHandshake $ TLS.handshake ctx
    v <- ST.evalStateT (connState conn) (TLSConnState ctx S.empty)
    rethrow' TlsExceptionBye $ TLS.bye ctx
    return v
    where
      rethrow' thr io =
        io `catch` (\(e :: SomeException) -> throwM $ thr $ show e)
