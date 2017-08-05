{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Network.Ether.Monad.TLS
       ( TlsT
       , runConnection
       ) where

import           Prelude                           hiding (mapM, mapM_)

import qualified Control.Monad.Ether.Network.Class as E (MonadConnection (..))
import qualified Control.Monad.Network.Class       as T (MonadConnection (..))

import           Control.Monad.State.Strict        (StateT)

import qualified Network.Monad.TLS                 as T

import           Control.Monad.Catch
import           Control.Monad.IO.Class            (MonadIO (..))

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Ether.Tagged
import           Crypto.Random
import qualified Network.TLS                       as TLS

newtype TlsT tag s l a = TlsT { runTlsT :: TaggedTrans tag (T.TlsT s) l a }
                       deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadTrans)

instance (Monad (t s), T.MonadConnection s, MonadIO s, MonadMask s, MonadCatch (t s), MonadTrans t) => E.MonadConnection tag (TlsT tag s (t s)) where
    send _ = TlsT . pack . T.send
    recv _ = TlsT . pack . T.recv
    reconnect _ = TlsT $ pack T.reconnect

runConnection :: (Monad (t s), T.MonadConnection s, MonadIO s, MonadRandom s, MonadMask s, MonadTrans t, MonadCatch (t s))
              => proxy tag -> TLS.ClientParams s -> TlsT tag s (t s) a -> t s a
runConnection tag params conn = T.runConnection params (unpack $ runTlsT conn)
