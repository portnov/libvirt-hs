{-# LANGUAGE ForeignFunctionInterface, StandaloneDeriving, DeriveDataTypeable #-}

{# context lib="virt" prefix="vir" #}

#include <libvirt/libvirt.h>

-- | Internal types definitions and low-level functions.
-- This module is not supposed to be imported by client code.
module System.LibVirt.Internal
  (Connection (..), Domain (..), Network (..),
   Stream (..), StoragePool (..), StorageVol (..)
  ) where

import Data.Generics
import Foreign
import Foreign.C.Types
import Foreign.C.String

{# pointer *virConnectPtr as Connection newtype #}

deriving instance Eq Connection
deriving instance Data Connection
deriving instance Typeable Connection

instance Show Connection where
  show (Connection ptr) = "<Connection: " ++ show ptr ++ ">"

{# pointer *virDomainPtr as Domain newtype #}

deriving instance Eq Domain
deriving instance Data Domain
deriving instance Typeable Domain

instance Show Domain where
  show (Domain ptr) = "<Domain: " ++ show ptr ++ ">"

{# pointer *virNetworkPtr as Network newtype #}

deriving instance Eq Network
deriving instance Data Network
deriving instance Typeable Network

instance Show Network where
  show (Network ptr) = "<Network: " ++ show ptr ++ ">"

{# pointer *virStreamPtr as Stream newtype #}

deriving instance Eq Stream
deriving instance Data Stream
deriving instance Typeable Stream

instance Show Stream where
  show (Stream ptr) = "<Stream: " ++ show ptr ++ ">"

{# pointer *virStoragePoolPtr as StoragePool newtype #}

deriving instance Eq StoragePool
deriving instance Data StoragePool
deriving instance Typeable StoragePool

instance Show StoragePool where
  show (StoragePool ptr) = "<StoragePool: " ++ show ptr ++ ">"

{# pointer *virStorageVolPtr as StorageVol newtype #}

deriving instance Eq StorageVol
deriving instance Data StorageVol
deriving instance Typeable StorageVol

instance Show StorageVol where
  show (StorageVol ptr) = "<StorageVol: " ++ show ptr ++ ">"
