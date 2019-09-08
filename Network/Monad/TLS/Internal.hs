{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}

module Network.Monad.TLS.Internal
       ( ConnState(..)
       , TlsException(..)
       , send
       , recv
       , reconnect
       , rethrow
       , Backend(..)
       ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import qualified Control.Monad.Network.Class as Class
import           Control.Monad.Trans
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as ST
import           Crypto.Random
import qualified Data.ByteString                  as S
import qualified Data.ByteString.Lazy             as L
import           Data.Monoid                      ((<>))
import           Data.Typeable                    (Typeable)
import qualified Network.TLS                      as TLS
import qualified Network.TLS.Backend              as TLS
import           Prelude                          hiding (mapM, mapM_)
import           Control.Monad.Morph              (MFunctor)
import           Control.Monad.Fail

data ConnState s = TLSConnState { tlsCtx :: TLS.Context s, tlsBuffer :: S.ByteString }

data TlsException = TlsExceptionSend String
                  | TlsExceptionReceive String
                  | TlsExceptionBye String
                  | TlsExceptionLowerReconnect String
                  | TlsExceptionHandshake String
                  | TlsExceptionCreateContext String
                  deriving (Show, Typeable)

instance Exception TlsException

rethrow :: (Monad m, MonadCatch m) => (String -> TlsException) -> m a -> m a
rethrow thr io = do
    r <- (Right <$> io) `catch` (\(e :: SomeException) -> return $ Left $ show e)
    case r of
      Right r' -> return r'
      Left e   -> throwM (thr e)

send :: (Monad s, Class.Connection s, MonadIO s, MonadCatch s, MonadThrow s) => S.ByteString -> StateT (ConnState s) s ()
send bs = do
    TLSConnState ctx _ <- ST.get
#ifdef DEBUG
    liftIO $ print $ "tls send:" ++ (show $ S.length bs)
#endif
    rethrow TlsExceptionSend $ lift $ TLS.sendData ctx $ L.fromStrict bs

recv :: (Monad s, Class.Connection s, MonadIO s, MonadMask s, MonadCatch s) => Int -> StateT (ConnState s) s S.ByteString
recv len = do
    TLSConnState ctx buf <- ST.get
#ifdef DEBUG
    liftIO $ print $ "tls recv:" ++ (show len)
#endif
    (res, buf') <- if S.length buf >= len
      then return (S.take len buf, S.drop len buf)
      else rethrow TlsExceptionReceive $ lift $ recv' ctx buf

    ST.put $ TLSConnState ctx buf'
    return res
    where
      recv' ctx acc | S.length acc > len = return (S.take len acc, S.drop len acc)
      recv' ctx acc = do
          bs <- TLS.recvData ctx
          if S.null bs then return (acc, S.empty)
                       else recv' ctx (acc <> bs)

reconnect :: (Monad s, Class.Connection s, MonadIO s, MonadMask s, MonadCatch s) => StateT (ConnState s) s ()
reconnect = do
    TLSConnState ctx _ <- ST.get
    rethrow TlsExceptionBye $ lift $ TLS.bye ctx
    rethrow TlsExceptionLowerReconnect $ lift Class.reconnect
    rethrow TlsExceptionHandshake $ lift $ TLS.handshake ctx

data Backend = Backend
instance (Class.Connection l, MonadIO l) => TLS.HasBackend Backend l where
    initializeBackend _ = return ()
    getBackend _ = TLS.Backend (return ()) (return ()) Class.send (recvAll S.empty)
      where
        recvAll acc len | len == 0 = return acc
        recvAll acc len = do
                            b <- Class.recv len
                            if S.length b > 0 then recvAll (acc <> b) (len - S.length b)
                            else return acc
