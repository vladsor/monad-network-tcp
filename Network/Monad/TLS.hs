{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Monad.TLS
       ( TLSConn
       ) where

import Prelude hiding (mapM, mapM_)

import Data.Monoid ((<>))

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class
import Control.Monad.Catch

import Control.Monad.Network.Class

import Control.Monad.State.Strict (StateT)
import qualified Control.Monad.State.Strict as ST

import qualified Network.TLS as TLS
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S

import Crypto.Random
import qualified Crypto.Random.AESCtr as RNG

data TLSConnState l = TLSConnState { tlsCtx :: TLS.Context l, tlsBuffer :: S.ByteString }
newtype TLSConn l a = TLSConn { connState :: StateT (TLSConnState l) l a }
                      deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

tlsSend :: (MonadConnection l, MonadMask l, MonadIO l) => S.ByteString -> TLSConn l ()
tlsSend bs = TLSConn $ do
    TLSConnState ctx _ <- ST.get
    lift $ TLS.sendData ctx $ L.fromStrict bs
    return ()

tlsRecv :: (MonadConnection l, MonadMask l, MonadIO l) => Int -> TLSConn l S.ByteString
tlsRecv len = TLSConn $ do
    TLSConnState ctx buf <- ST.get
    (res, buf') <- lift $ if S.length buf >= len
      then return (S.take len buf, S.drop len buf)
      else recv' ctx buf

    ST.put $ TLSConnState ctx buf'
    return $ res
    where
      recv' ctx acc | S.length acc > len = return (S.take len acc, S.drop len acc)
      recv' ctx acc = do
        bs <- TLS.recvData ctx
        if S.null bs then return (acc, S.empty)
        else recv' ctx (acc <> bs)

tlsRecconnect :: (MonadConnection l, MonadMask l, MonadIO l) => TLSConn l ()
tlsRecconnect = TLSConn $ do
    TLSConnState ctx _ <- ST.get
    lift $ do
      TLS.bye ctx
      reconnect
      TLS.handshake ctx

instance (MonadConnection l, Functor l, MonadMask l, MonadIO l) => MonadConnection (TLSConn l) where
  send = tlsSend
  recv = tlsRecv
  reconnect = tlsRecconnect

instance (MonadConnection l, RunMonadConnection l a b, Functor l, MonadMask l, MonadIO l) => RunMonadConnection (TLSConn l) (TLS.ClientParams l, RNG.AESRNG) l where
  runConnection (params, rng) conn = do
    let recvAll acc len | len == 0 = return acc
        recvAll acc len = do
                            b <- recv len
                            if S.length b > 0 then recvAll (acc <> b) (len - S.length b)
                            else return acc

    let cBackend :: TLS.Backend l = TLS.Backend (return ()) (return ()) send (recvAll S.empty)

    ctx <- TLS.contextNew cBackend params rng
    TLS.handshake ctx
    v <- ST.evalStateT (connState conn) (TLSConnState ctx S.empty)
    TLS.bye ctx
    return v
