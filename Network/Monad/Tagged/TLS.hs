{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Network.Monad.Tagged.TLS
       ( TlsT
       , runConnection
       ) where

import           Prelude                           hiding (mapM, mapM_)

import           Control.Monad.Catch
import           Control.Monad.IO.Class            (MonadIO)

import qualified Control.Monad.Network.Class as Class (Connection (..))
import qualified Control.Monad.Network.Class.Tagged as Tagged (Connection (..))
import           Control.Monad.Trans.Class
import           Control.Monad.Fail
import           Data.Functor.Trans.Tagged
import qualified Network.Monad.TLS                 as T
import qualified Network.TLS                       as TLS
import           Crypto.Random

type TlsT tag l = TaggedT tag (T.TlsT l)

instance
    ( Class.Connection s
    , Monad s
    , MonadIO s
    , MonadMask s
    ) => Tagged.Connection tag (TlsT tag s)
  where
    send = tagT . Class.send
    recv = tagT . Class.recv
    reconnect = tagT Class.reconnect

runConnection :: (Monad s, Class.Connection s, MonadIO s, MonadRandom s, MonadMask s, MonadFail s)
              => TLS.ClientParams s
              -> TlsT tag s a
              -> s a
runConnection p = T.runConnection p . untagT
