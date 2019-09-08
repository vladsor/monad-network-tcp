module Network.Monad.Tagged.TCP
       ( TcpT
       , Tcp
       , runConnection
       ) where

import           Prelude                           hiding (mapM, mapM_)
import qualified Network.Monad.TCP                 as T
import           Control.Monad.Catch
import           Control.Monad.IO.Class            (MonadIO)
import Network.Socket                    (AddrInfo)
import           Data.Functor.Identity             (Identity)
import           Data.Functor.Trans.Tagged

type Tcp tag = TcpT tag Identity
type TcpT tag m = TaggedT tag (T.TcpT m)

runConnection :: (MonadIO m, MonadThrow m) => AddrInfo -> TcpT tag m a -> m a
runConnection a = T.runConnection a . untagT
