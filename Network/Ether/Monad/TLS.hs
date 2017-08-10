{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}


module Network.Ether.Monad.TLS
       ( TlsT
       , runConnection
       ) where

import           Prelude                           hiding (mapM, mapM_)

import           Control.Monad.Catch
-- import           Control.Monad.Ether.Network.Class (CONN, Handle (..), TAGGED)
import qualified Control.Monad.Ether.Network.Class as E (MonadConnection (..))
import           Control.Monad.IO.Class            (MonadIO (..))
import qualified Control.Monad.Network.Class       as T (MonadConnection (..))
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Identity
import           Crypto.Random
import           Data.Coerce
import           Data.Functor.Identity
import           Ether.TaggedTrans
import qualified Network.Monad.TLS                 as T
import qualified Network.TLS                       as TLS

import           Data.Coerce
import           Data.Functor.Identity             (Identity (..))
import           Data.Kind
-- import           Data.Tagged
import           Ether.TaggedTrans
import           GHC.Exts                          (Constraint)

{-
data ConnState s = TLSConnState { tlsCtx :: TLS.Context s, tlsBuffer :: S.ByteString }
newtype TlsT s l a = TlsT { connState :: StateT (ConnState s) l a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadTrans, MFunctor)
-}


instance Handle CONN T.TlsT where
  handling r = r


data TAGGED e t

type K_Monad = Type -> Type

type K_Trans = K_Monad -> K_Monad

type K_Trans2 = K_Monad -> K_Monad -> K_Monad

type family
  HandleSuper
    (eff :: keff)
    (trans :: K_Trans2)
  :: Constraint

type family
  HandleConstraint
    (eff :: keff)
    (trans :: K_Trans2)
    (s :: K_Monad)
    (t :: K_Trans)
  :: Constraint

{-
instance (Monad (t s), MonadConnection s, MonadIO s, MonadMask s, MonadCatch (t s), MonadTrans t) => MonadConnection (TlsT s (t s)) where
-}

class
  HandleSuper eff trans =>
    Handle eff (trans :: K_Trans2)
  where
      handling :: (Monad (t s), T.MonadConnection s, MonadIO s, MonadMask s, MonadCatch (t s), MonadTrans t)
               => (HandleConstraint eff trans s t => r)
               -> r

data CONN

type instance HandleSuper      CONN trans   = ()
type instance HandleConstraint CONN trans s t =
    T.MonadConnection (trans s (t s))

-- type Tls tag s = TlsT tag IdentityT Identity
type TlsT tag s = TaggedTrans (TAGGED CONN tag) (T.TlsT s)

instance
    ( Handle CONN trans
      , Monad (t s), T.MonadConnection s, MonadIO s, MonadMask s, MonadCatch (t s), MonadTrans t
--      , Monad m, MonadIO m, MonadThrow m,
      , Monad (trans s (t s))
    ) => E.MonadConnection tag (TaggedTrans (TAGGED CONN tag) (trans s) (t s))
  where
      send =
          handling @CONN @trans @t @s $
              coerce (T.send @(trans s (t s)))
      recv =
          handling @CONN @trans @t @s $
              coerce (T.recv @(trans s (t s)))
      reconnect =
          handling @CONN @trans @t @s $
              coerce (T.reconnect @(trans s (t s)))


{-
runConnection :: (Monad (t s), MonadConnection s, MonadIO s, MonadRandom s, MonadMask s, MonadTrans t, MonadCatch (t s))
              => TLS.ClientParams s
              -> TlsT s (t s) a
              -> (t s) a
-}
runConnection :: forall tag t s a. (Monad (t s), T.MonadConnection s, MonadIO s, MonadRandom s, MonadMask s, MonadTrans t, MonadCatch (t s))
              => TLS.ClientParams s
              -> TlsT tag s (t s) a
              -> (t s) a
runConnection = coerce (T.runConnection @t @s @a)

