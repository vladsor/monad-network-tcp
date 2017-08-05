{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- #define DEBUG 1

module Network.Monad.TLS
       ( TlsT
       , runConnection
       ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Network.Class      (MonadConnection (..))
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
import           Control.Monad.Morph         (MFunctor)

data ConnState s = TLSConnState { tlsCtx :: TLS.Context s, tlsBuffer :: S.ByteString }
newtype TlsT s l a = TlsT { connState :: StateT (ConnState s) l a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadTrans, MFunctor)

data TlsException = TlsExceptionSend String
                  | TlsExceptionReceive String
                  | TlsExceptionBye String
                  | TlsExceptionLowerReconnect String
                  | TlsExceptionHandshake String
                  | TlsExceptionCreateContext String
                  deriving (Show, Typeable)

instance Exception TlsException

rethrow thr io = do
    r <- lift (Right <$> io) `catch` (\(e :: SomeException) -> return $ Left $ show e)
    case r of
      Right r' -> return r'
      Left e   -> throwM (thr e)

tlsSend :: (Monad (t s), MonadConnection s, MonadIO s, MonadCatch (t s), MonadTrans t, MonadThrow s) => S.ByteString -> TlsT s (t s) ()
tlsSend bs = TlsT $ do
    TLSConnState ctx _ <- ST.get
#ifdef DEBUG
    liftIO $ print $ "tls send:" ++ (show $ S.length bs)
#endif
    rethrow TlsExceptionSend $ lift $ TLS.sendData ctx $ L.fromStrict bs
--    return ()

tlsRecv :: (Monad (t s), MonadConnection s, MonadIO s, MonadMask s, MonadCatch (t s), MonadTrans t) => Int -> TlsT s (t s) S.ByteString
tlsRecv len = TlsT $ do
    TLSConnState ctx buf <- ST.get
#ifdef DEBUG
    liftIO $ print $ "tls recv:" ++ (show len)
#endif
    (res, buf') <- if S.length buf >= len
      then return (S.take len buf, S.drop len buf)
      else rethrow TlsExceptionReceive $ recv' ctx buf

    ST.put $ TLSConnState ctx buf'
    return $ res
    where
      recv' ctx acc | S.length acc > len = return (S.take len acc, S.drop len acc)
      recv' ctx acc = do
          bs <- lift $ TLS.recvData ctx
          if S.null bs then return (acc, S.empty)
                       else recv' ctx (acc <> bs)

tlsRecconnect :: (Monad (t s), MonadConnection s, MonadIO s, MonadMask s, MonadCatch (t s), MonadTrans t) => TlsT s (t s) ()
tlsRecconnect = TlsT $ do
    TLSConnState ctx _ <- ST.get
    rethrow TlsExceptionBye $ lift $ TLS.bye ctx
    rethrow TlsExceptionLowerReconnect $ lift reconnect
    rethrow TlsExceptionHandshake $ lift $ TLS.handshake ctx

instance (Monad (t s), MonadConnection s, MonadIO s, MonadMask s, MonadCatch (t s), MonadTrans t) => MonadConnection (TlsT s (t s)) where
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

runConnection :: (Monad (t s), MonadConnection s, MonadIO s, MonadRandom s, MonadMask s, MonadTrans t, MonadCatch (t s)) => TLS.ClientParams s -> TlsT s (t s) a -> (t s) a
runConnection params conn = do
#ifdef DEBUG
    liftIO $ print "Create TLS context"
#endif
    ctx <- rethrow' TlsExceptionCreateContext $ lift $ TLS.contextNew Backend params
#ifdef DEBUG
    liftIO $ print "Handshake"
#endif
    rethrow' TlsExceptionHandshake $ lift $ TLS.handshake ctx
    v <- ST.evalStateT (connState conn) (TLSConnState ctx S.empty)
    rethrow' TlsExceptionBye $ lift $ TLS.bye ctx
    return v
    where
      rethrow' thr io =
        io `catch` (\(e :: SomeException) -> throwM $ thr $ show e)
