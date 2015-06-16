{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Monad
       ( NetworkConn
       ) where

import Prelude hiding (mapM, mapM_)

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class
import Control.Monad.Catch

import Control.Monad.Network.Class

import Control.Monad.State.Strict (StateT)
import qualified Control.Monad.State.Strict as ST

import qualified Network.Socket as NS hiding (recv, send)
import qualified Network.Socket.ByteString as NS
import qualified Data.ByteString as S

data NetworkConnState = NetworkConnState { ncSocket :: NS.Socket, ncAddr :: NS.AddrInfo }
newtype NetworkConn a = NetworkConn { connState :: StateT NetworkConnState IO a }
                        deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

mkNetworkConn :: NS.AddrInfo -> IO NetworkConnState
mkNetworkConn addr = do
  s <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
  NS.connect s (NS.addrAddress addr)
  return $ NetworkConnState s addr

nsend :: S.ByteString -> NetworkConn ()
nsend bs = NetworkConn $ do
    NetworkConnState s _ <- ST.get
    lift $ NS.send s bs
    return ()

nrecv :: Int -> NetworkConn S.ByteString
nrecv len = NetworkConn $ do
    NetworkConnState s _ <- ST.get
    r <- lift $ NS.recv s len
    return r

nreconnect :: NetworkConn ()
nreconnect = NetworkConn $ do
    NetworkConnState s addr <- ST.get
    lift $ NS.close s
    ST.StateT $ \_ -> liftM ((),) $ mkNetworkConn addr

instance MonadConnection NetworkConn where
  send = nsend
  recv = nrecv
  reconnect = nreconnect

instance RunMonadConnection NetworkConn NS.AddrInfo IO where
  runConnection addr conn = do
    st <- mkNetworkConn addr
    (v, NetworkConnState s' _) <- ST.runStateT (connState conn) st
    NS.close s'
    return v

