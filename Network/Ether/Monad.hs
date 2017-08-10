{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeInType              #-}
{-# LANGUAGE TypeSynonymInstances    #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}


module Network.Ether.Monad
       ( TcpT
       , Tcp
       , runConnection
       ) where

import qualified Network.Monad                     as T
import           Prelude                           hiding (mapM, mapM_)

import qualified Control.Monad.Ether.Network.Class as E (MonadConnection (..))
import qualified Control.Monad.Network.Class       as T (MonadConnection (..))

import           Control.Monad.Catch
import           Control.Monad.IO.Class            (MonadIO (..))

import qualified Network.Socket                    as NS hiding (recv, send)

-- import           Control.Monad.Ether.Network.Class (CONN, Handle (..), TAGGED)
import           Data.Coerce
import           Data.Functor.Identity             (Identity (..))
import           Data.Kind
-- import           Data.Tagged
import           Ether.TaggedTrans
import           GHC.Exts                          (Constraint)

instance Handle CONN T.TcpT where
  handling r = r

type Tcp tag = TcpT tag Identity
type TcpT tag = TaggedTrans (TAGGED CONN tag) T.TcpT

data TAGGED e t

type K_Monad = Type -> Type

type K_Trans = K_Monad -> K_Monad

type family
  HandleSuper
    (eff :: keff)
    (trans :: K_Trans)
  :: Constraint

type family
  HandleConstraint
    (eff :: keff)
    (trans :: K_Trans) (m :: K_Monad)
  :: Constraint

class
  HandleSuper eff trans =>
    Handle eff (trans :: K_Trans)
  where
    handling :: (Monad m, MonadIO m, MonadThrow m) => (HandleConstraint eff trans m => r) -> r

data CONN

type instance HandleSuper      CONN trans   = ()
type instance HandleConstraint CONN trans m =
  T.MonadConnection (trans m)

instance
    ( Handle CONN trans
    , Monad m, MonadIO m, MonadThrow m, Monad (trans m)
    ) => E.MonadConnection tag (TaggedTrans (TAGGED CONN tag) trans m)
  where
      send =
          handling @CONN @trans @m $
          coerce (T.send @(trans m))
      recv =
          handling @CONN @trans @m $
          coerce (T.recv @(trans m))
      reconnect =
          handling @CONN @trans @m $
          coerce (T.reconnect @(trans m))

runConnection :: forall tag m a. (MonadIO m, MonadThrow m) => NS.AddrInfo -> TcpT tag m a -> m a
runConnection = coerce (T.runConnection @m @a)
