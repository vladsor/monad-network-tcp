{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Network.Monad.TLS
       ( TlsT(..)
       , runConnection
       , TlsException(..)
       ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Resource (MonadResource)
import           Control.Monad.Network.Class      (Connection(..))
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
import           Control.Monad.Fail
import           Data.Kind

import qualified Network.Monad.TLS.Internal as Internal
import Network.Monad.TLS.Internal (TlsException(..), rethrow)

newtype TlsT l a = TlsT { connState :: StateT (Internal.ConnState l) l a }
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadResource)

instance MonadTrans TlsT where
  lift = TlsT . lift

instance (Monad s, Connection s, MonadIO s, MonadMask s) => Connection (TlsT s) where
  send = TlsT . Internal.send
  recv = TlsT . Internal.recv
  reconnect = TlsT Internal.reconnect

runConnection :: (Monad s, Connection s, MonadIO s, MonadRandom s, MonadMask s, MonadFail s)
              => TLS.ClientParams s
              -> TlsT s a
              -> s a
runConnection params conn = do
    ctx <- rethrow TlsExceptionCreateContext $ TLS.contextNew Internal.Backend params
    rethrow TlsExceptionHandshake $ TLS.handshake ctx
    v <- ST.evalStateT (connState conn) (Internal.TLSConnState ctx S.empty)
    rethrow TlsExceptionBye $ TLS.bye ctx
    return v
