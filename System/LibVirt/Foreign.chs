{-# LANGUAGE ForeignFunctionInterface, StandaloneDeriving, DeriveDataTypeable, EmptyDataDecls #-}

{# context lib="virt" prefix="vir" #}

#include <libvirt/libvirt.h>

module System.LibVirt.Foreign
  (-- * Types
   Connection, Domain, Network,
   DomainID,
   DomainInfo (..),
   DomainState (..),
   Stream,
   StreamFlags (..),
   DomainCreateFlags (..),
   DomainXMLFlags (..),
   SecurityLabel (..),
   SecurityModel (..),
   NodeInfo (..),
   StoragePoolInfo (..),
   StoragePoolState (..),
   StoragePoolBuildFlags (..),
   StoragePoolDeleteFlags (..),
   StorageVolInfo (..),
   StorageVolType (..),
   StorageVolDeleteFlags (..),
   StorageVolWipeAlgorithm (..),
   StorageXMLFlags (..),
   StorageVolCreateFlags (..),
   StorageVolResizeFlags (..),
   SchedParameterType (..),
   ConnectCredential (..),
   DomainEventID (..),
   DomainEventType (..),
   DomainEventDefinedDetailType (..),
   DomainEventUndefinedDetailType (..),
   DomainEventStartedDetailType (..),
   DomainEventSuspendedDetailType (..),
   DomainEventResumedDetailType (..),
   DomainEventStoppedDetailType (..),
   DomainEventShutdownDetailType (..),
   FreeCallback,
   ConnectDomainEventGenericCallback,
   ConnectDomainEventCallback,

   -- * Connection management functions
   initialize,
   openConnection, closeConnection,
   connectSetKeepAlive,
   connectGetCapabilities,

   -- * Domains management functions
   runningDomainsCount, definedDomainsCount,
   runningDomainsIDs, definedDomainsNames,
   lookupDomainID, lookupDomainName,
   getDomainInfo, getDomainXML,
   defineDomainXML, undefineDomain,
   getDomainID, getDomainName,

   -- * Domains control
   createDomain, createDomainXML,
   destroyDomain,
   shutdownDomain, rebootDomain,
   suspendDomain, resumeDomain,
   saveDomain, restoreDomain,
   refDomain, freeDomain,

   -- * Networks management
   getNetworkConnection,
   runningNetworksCount, definedNetworksCount,
   runningNetworksNames, definedNetworksNames,
   lookupNetworkName, lookupNetworkUUID,
   createNetworkXML, defineNetworkXML,
   undefineNetwork, destroyNetwork,
   createNetwork,
   refNetwork, freeNetwork,
   getNetworkName,
   getNetworkXML,

   -- * Storage Pool management
   runningStoragePoolsCount, definedStoragePoolsCount,
   runningStoragePoolsNames, definedStoragePoolsNames,
   lookupStoragePoolName, lookupStoragePoolUUID,
   lookupStoragePoolVolume, createStoragePoolXML,
   defineStoragePoolXML, undefineStoragePool,
   buildStoragePool, createStoragePool,
   destroyStoragePool, deleteStoragePool,
   refStoragePool, freeStoragePool, refreshStoragePool,
   getStoragePoolName, getStoragePoolInfo, getStoragePoolXML,
   getStoragePoolAutostart, setStoragePoolAutostart,
   storagePoolIsActive, storagePoolIsPersistent,

   -- * Storage Volume management
   storagePoolVolsCount, storagePoolVolsNames,
   getStorageVolConnection,
   lookupStorageVolName, lookupStorageVolKey, lookupStorageVolPath,
   getStorageVolName, getStorageVolKey, getStorageVolInfo,
   getStorageVolXML, getStorageVolPath,
   createStorageVolXML, createStorageVolXMLFrom,
   downloadStorageVol, uploadStorageVol,
   deleteStorageVol, wipeStorageVol, wipeStorageVolWith,
   refStorageVol, freeStorageVol,
   resizeStorageVol,

   -- * callback management
   eventRegisterDefaultImpl,
   eventRunDefaultImpl,
   connectDomainEventRegister,
   connectDomainEventRegisterAny,
   connectDomainEventDeregisterAny,
   mkConnectDomainEventGenericCallback,
   mkConnectDomainEventCallback,
   mkFreeCallback

  ) where

import Data.Bits
import Data.Generics ()
import Data.Functor
import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.Posix.IO
import System.Posix.Types
import System.Posix.Files

{# import System.LibVirt.Internal #}
{# import System.LibVirt.Errors #}

cIntConv = fromIntegral

cuchar2state :: CUChar -> DomainState
cuchar2state c = toEnum (fromIntegral c)

flags2int :: (Enum f, Num a) => [f] -> a
flags2int list = fromIntegral $ foldr (.|.) 0 (map fromEnum list)

flag2int :: (Enum f, Num a) => f -> a
flag2int = fromIntegral . fromEnum

data DomainInfo = DomainInfo {
  diState :: DomainState,
  diMaxMem :: Integer,
  diMemory :: Integer,
  diNrVirtCPU :: Int,
  diCPUTime :: Integer }
  deriving (Eq, Show)

{# pointer *virDomainInfo as DomainInfoPtr -> DomainInfo #}

{# enum DomainState {underscoreToCase} deriving (Eq, Show) #}
{# enum DomainCreateFlags {underscoreToCase} deriving (Eq, Show) #}
{# enum DomainXMLFlags {underscoreToCase} deriving (Eq, Show) #}
{# enum DomainEventID {underscoreToCase} deriving (Eq, Show) #}
{# enum DomainEventType {underscoreToCase} deriving (Eq, Show) #}
{# enum DomainEventDefinedDetailType {underscoreToCase} deriving (Eq, Show) #}
{# enum DomainEventUndefinedDetailType {underscoreToCase} deriving (Eq, Show) #}
{# enum DomainEventStartedDetailType {underscoreToCase} deriving (Eq, Show) #}
{# enum DomainEventSuspendedDetailType {underscoreToCase} deriving (Eq, Show) #}
{# enum DomainEventResumedDetailType {underscoreToCase} deriving (Eq, Show) #}
{# enum DomainEventStoppedDetailType {underscoreToCase} deriving (Eq, Show) #}
{# enum DomainEventShutdownDetailType {underscoreToCase} deriving (Eq, Show) #}


data NetworkXMLFlags = NetworkXML
  deriving (Eq, Show, Enum)

{# enum StreamFlags {underscoreToCase} deriving (Eq, Show) #}

{# fun virStreamNew as newStream
    { connectionToPtr `Connection',
      flags2int       `[StreamFlags]' } -> `Stream' ptrToStream* #}

{# fun virStreamFinish as finishStream
    { streamToPtr `Stream' } -> `Int' exceptionOnMinusOne* #}

{# fun virStreamFree as freeStream
    { streamToPtr `Stream' } -> `Int' exceptionOnMinusOne* #}

data SecurityLabel = SecurityLabel {
  slLabel :: String,
  slEnforcing :: Int }
  deriving (Eq, Show)

{# pointer *virSecurityLabelPtr as SecurityLabelPtr -> SecurityLabel #}

data SecurityModel = SecurityModel {
  smModel :: String,
  smDOI :: String }
  deriving (Eq, Show)

{# pointer *virSecurityModelPtr as SecurityModelPtr -> SecurityModel #}

data NodeInfo = NodeInfo {
  niModel :: String,
  niMemory :: CULong,
  niCPUs :: CUInt,
  niMHz :: CUInt,
  niNodes :: CUInt,
  niSockets :: CUInt,
  niCores :: CUInt,
  niThreads :: CUInt }
  deriving (Eq, Show)

{# pointer *virNodeInfoPtr as NodeInfoPtr -> NodeInfo #}

data StoragePoolInfo = StoragePoolInfo {
  spiState :: StoragePoolState,
  spiCapacity :: CULLong,
  spiAllocation :: CULLong,
  spiAvailable :: CULLong }
  deriving (Eq, Show)

{# pointer *virStoragePoolInfoPtr as StoragePoolInfoPtr -> StoragePoolInfo #}

{# enum StoragePoolState {underscoreToCase} deriving (Eq, Show) #}
{# enum StoragePoolBuildFlags {underscoreToCase} deriving (Eq, Show) #}
{# enum StoragePoolDeleteFlags {underscoreToCase} deriving (Eq, Show) #}

data StorageVolInfo = StorageVolInfo {
  sviType :: StorageVolType,
  sviCapacity :: CULLong,
  sviAllocation :: CULLong }
  deriving (Eq, Show)

{# pointer *virStorageVolInfoPtr as StorageVolInfoPtr -> StorageVolInfo #}

{# enum StorageVolType {underscoreToCase} deriving (Eq, Show) #}
{# enum StorageVolDeleteFlags {underscoreToCase} deriving (Eq, Show) #}
{# enum StorageVolWipeAlgorithm {underscoreToCase} deriving (Eq, Show) #}
{# enum StorageXMLFlags {underscoreToCase} deriving (Eq, Show) #}
{# enum StorageVolCreateFlags {underscoreToCase} deriving (Eq, Show) #}
{# enum StorageVolResizeFlags {underscoreToCase} deriving (Eq, Show) #}

{# enum SchedParameterType {underscoreToCase} deriving (Eq, Show) #}

data ConnectCredential = ConnectCredential {
  ccType :: Int,
  ccPrompt :: String,
  ccChallenge :: String,
  ccDefresult :: String,
  ccResult :: String,
  ccResultLen :: Integer }
  deriving (Eq, Show)

{# pointer *virConnectCredentialPtr as ConnectCredentialPtr -> ConnectCredential #}

{# fun virInitialize as initialize { } -> `Int' exceptionOnMinusOne* #}

foreign import ccall "&" virConnectAuthPtrDefault :: Ptr (Ptr ())

openConnection :: String -> IO Connection
openConnection uri =
  withCString uri $ \str -> do
    authPtr <- peek virConnectAuthPtrDefault
    connPtr <- {# call virConnectOpenAuth #} str authPtr 0
    ptrToConnection connPtr

{# fun virConnectClose as closeConnection
    { connectionToPtr `Connection' } -> `Int' exceptionOnMinusOne* #}

{# fun virConnectNumOfDomains as runningDomainsCount
    { connectionToPtr `Connection' } -> `Int' exceptionOnMinusOne* #}

type DomainID = CInt

runningDomainsIDs :: Connection -> IO [DomainID]
runningDomainsIDs conn = do
  cn <- {# call virConnectNumOfDomains #} (connectionToPtr conn)
  let n = fromIntegral cn
  r <- allocaArray n $ \arr -> do
          r <- {# call virConnectListDomains #}
                  (connectionToPtr conn) arr cn
          peekArray n arr
  return r

definedDomainsNames :: Connection -> IO [String]
definedDomainsNames conn = do
  cn <- {# call virConnectNumOfDefinedDomains #} (connectionToPtr conn)
  let n = fromIntegral cn
  allocaArray n $ \nptr -> do
    {# call virConnectListDefinedDomains #}
        (connectionToPtr conn) nptr cn
    mapM peekCString =<< peekArray n nptr

{# fun virDomainLookupByID as lookupDomainID
    { connectionToPtr `Connection',
      id              `DomainID'    } -> `Domain' ptrToDomain* #}

{# fun virDomainGetID as getDomainID
    { domainToPtr `Domain' } -> `DomainID' cIntConv #}

{# fun virDomainGetName as getDomainName
    { domainToPtr `Domain' } -> `String' #}

{# fun virDomainLookupByName as lookupDomainName
    { connectionToPtr `Connection',
                      `String'      } -> `Domain' ptrToDomain* #}

{# fun virConnectNumOfDefinedDomains as definedDomainsCount
    { connectionToPtr `Connection' } -> `Int' exceptionOnMinusOne* #}

getDomainInfo :: Domain -> IO DomainInfo
getDomainInfo dptr = do
  allocaBytes {# sizeof virDomainInfo #} $ \iptr -> do
         i <- {# call virDomainGetInfo #} (domainToPtr dptr) iptr
         state   <- {# get DomainInfo->state #}     iptr
         maxmem  <- {# get DomainInfo->maxMem #}    iptr
         memory  <- {# get DomainInfo->memory #}    iptr
         ncpus   <- {# get DomainInfo->nrVirtCpu #} iptr
         cputime <- {# get DomainInfo->cpuTime #}   iptr
         return $ DomainInfo {
                    diState     = cuchar2state state,
                    diMaxMem    = fromIntegral maxmem,
                    diMemory    = fromIntegral memory,
                    diNrVirtCPU = fromIntegral ncpus,
                    diCPUTime   = fromIntegral cputime }

{# fun virDomainDefineXML as defineDomainXML
    { connectionToPtr `Connection',
                      `String'      } -> `Domain' ptrToDomain* #}

{# fun virDomainUndefine as undefineDomain
    { domainToPtr `Domain' } -> `Int' exceptionOnMinusOne* #}

{# fun virDomainCreate as createDomain
    { domainToPtr `Domain' } -> `Int' exceptionOnMinusOne* #}

{# fun virDomainCreateXML as createDomainXML
    { connectionToPtr `Connection',
                      `String',
      flags2int       `[DomainCreateFlags]' } -> `Domain' ptrToDomain* #}

{# fun virDomainGetXMLDesc as getDomainXML
    { domainToPtr `Domain',
      flags2int   `[DomainXMLFlags]' } -> `String' #}

{# fun virDomainShutdown as shutdownDomain
    { domainToPtr `Domain' } -> `Int' exceptionOnMinusOne* #}

{# fun virDomainReboot as rebootDomain
    { domainToPtr `Domain',
      id          `CUInt'  } -> `Int' exceptionOnMinusOne* #}

{# fun virDomainDestroy as destroyDomain
    { domainToPtr `Domain' } -> `Int' exceptionOnMinusOne* #}

{# fun virDomainRef as refDomain
    { domainToPtr `Domain' } -> `Int' exceptionOnMinusOne* #}

{# fun virDomainFree as freeDomain
    { domainToPtr `Domain' } -> `Int' exceptionOnMinusOne* #}

{# fun virDomainSuspend as suspendDomain
    { domainToPtr `Domain' } -> `Int' exceptionOnMinusOne* #}

{# fun virDomainResume as resumeDomain
    { domainToPtr `Domain' } -> `Int' exceptionOnMinusOne* #}

{# fun virDomainSave as saveDomain
    { domainToPtr `Domain',
                  `String' } -> `Int' exceptionOnMinusOne* #}

{# fun virDomainRestore as restoreDomain
    { connectionToPtr `Connection',
                      `String'      } -> `Int' exceptionOnMinusOne* #}

{# fun virNetworkGetConnect as getNetworkConnection
    { networkToPtr `Network' } -> `Connection' ptrToConnection* #}

{# fun virConnectNumOfNetworks as runningNetworksCount
    { connectionToPtr `Connection' } -> `Int' exceptionOnMinusOne* #}

runningNetworksNames :: Connection -> IO [String]
runningNetworksNames conn = do
  cn <- {# call virConnectNumOfNetworks #} (connectionToPtr conn)
  let n = fromIntegral cn
  allocaArray n $ \nptr -> do
    {# call virConnectListNetworks #} (connectionToPtr conn) nptr cn
    mapM peekCString =<< peekArray n nptr

{# fun virConnectNumOfDefinedNetworks as definedNetworksCount
    { connectionToPtr `Connection' } -> `Int' exceptionOnMinusOne* #}

definedNetworksNames :: Connection -> IO [String]
definedNetworksNames conn = do
  cn <- {# call virConnectNumOfDefinedNetworks #} (connectionToPtr conn)
  let n = fromIntegral cn
  allocaArray n $ \nptr -> do
    {# call virConnectListDefinedNetworks #} (connectionToPtr conn) nptr cn
    mapM peekCString =<< peekArray n nptr

{# fun virNetworkLookupByName as lookupNetworkName
    { connectionToPtr `Connection', `String' } -> `Network' ptrToNetwork* #}

withCUString :: String -> (Ptr CUChar -> IO a) -> IO a
withCUString str fn = withCString str (fn . castPtr)

{# fun virNetworkLookupByUUID as lookupNetworkUUID
    { connectionToPtr `Connection', withCUString* `String' } -> `Network' ptrToNetwork* #}

{# fun virNetworkCreateXML as createNetworkXML
    { connectionToPtr `Connection', `String' } -> `Network' ptrToNetwork* #}

{# fun virNetworkDefineXML as defineNetworkXML
    { connectionToPtr `Connection', `String' } -> `Network' ptrToNetwork* #}

{# fun virNetworkUndefine as undefineNetwork
    { networkToPtr `Network' } -> `Int' exceptionOnMinusOne* #}

{# fun virNetworkCreate as createNetwork
    { networkToPtr `Network' } -> `Int' exceptionOnMinusOne* #}

{# fun virNetworkDestroy as destroyNetwork
    { networkToPtr `Network' } -> `Int' exceptionOnMinusOne* #}

{# fun virNetworkRef as refNetwork
    { networkToPtr `Network' } -> `Int' exceptionOnMinusOne* #}

{# fun virNetworkFree as freeNetwork
    { networkToPtr `Network' } -> `Int' exceptionOnMinusOne* #}

{# fun virNetworkGetName as getNetworkName
    { networkToPtr `Network' } -> `String' #}

{# fun virNetworkGetXMLDesc as getNetworkXML
    { networkToPtr `Network', 
      flags2int    `[NetworkXMLFlags]' } -> `String' #}

{# fun virConnectDomainEventRegisterAny as connectDomainEventRegisterAny
    `(Storable a)' =>
    { connectionToPtr `Connection',
      domainToPtr `Domain',
      flag2int `DomainEventID',
      castFunPtr `FunPtr (ConnectDomainEventCallback a)',
      castPtr `Ptr a',
      id `FunPtr FreeCallback'
      } -> `Int' exceptionOnMinusOne* #}

{# fun virConnectDomainEventDeregisterAny as connectDomainEventDeregisterAny
    { connectionToPtr `Connection',
      fromIntegral `Int'
    } -> `Int' exceptionOnMinusOne* #}

{# fun virEventRegisterDefaultImpl as eventRegisterDefaultImpl
     { } -> `Int'exceptionOnMinusOne* #}

{# fun virConnectSetKeepAlive as connectSetKeepAlive
      { connectionToPtr `Connection',
        id     `CInt',
        id     `CUInt'
      } -> `Int' exceptionOnMinusOne* #}

{# fun virConnectDomainEventRegister as connectDomainEventRegister
      { connectionToPtr `Connection',
        castFunPtr `FunPtr (ConnectDomainEventCallback a)',
        castPtr `Ptr a',
        id `FunPtr FreeCallback'
      } -> `Int' exceptionOnMinusOne* #}

{# fun virEventRunDefaultImpl as eventRunDefaultImpl
      {} -> `Int' exceptionOnMinusOne* #}

-- | Provides capabilities of the hypervisor / driver.
{# fun virConnectGetCapabilities as connectGetCapabilities
      { connectionToPtr `Connection' } -> `String' #}

{# fun virConnectNumOfStoragePools as runningStoragePoolsCount
    { connectionToPtr `Connection' } -> `Int' exceptionOnMinusOne* #}

runningStoragePoolsNames :: Connection -> IO [String]
runningStoragePoolsNames conn = do
  cn <- {# call virConnectNumOfStoragePools #} (connectionToPtr conn)
  let n = fromIntegral cn
  allocaArray n $ \nptr -> do
    {# call virConnectListStoragePools #} (connectionToPtr conn) nptr cn
    mapM peekCString =<< peekArray n nptr

{# fun virConnectNumOfDefinedStoragePools as definedStoragePoolsCount
    { connectionToPtr `Connection' } -> `Int' exceptionOnMinusOne* #}

definedStoragePoolsNames :: Connection -> IO [String]
definedStoragePoolsNames conn = do
  cn <- {# call virConnectNumOfDefinedStoragePools #} (connectionToPtr conn)
  let n = fromIntegral cn
  allocaArray n $ \nptr -> do
    {# call virConnectListDefinedStoragePools #} (connectionToPtr conn) nptr cn
    mapM peekCString =<< peekArray n nptr

{# fun virStoragePoolLookupByName as lookupStoragePoolName
    { connectionToPtr `Connection',
                      `String' } -> `StoragePool' ptrToStoragePool* #}

{# fun virStoragePoolLookupByUUID as lookupStoragePoolUUID
    { connectionToPtr `Connection',
      withCUString*   `String' } -> `StoragePool' ptrToStoragePool* #}

{# fun virStoragePoolLookupByVolume as lookupStoragePoolVolume
    { storageVolToPtr `StorageVol' } -> `StoragePool' ptrToStoragePool* #}

{# fun virStoragePoolCreateXML as createStoragePoolXML
    { connectionToPtr `Connection',
                      `String',
      id              `CUInt' } -> `StoragePool' ptrToStoragePool* #}

{# fun virStoragePoolDefineXML as defineStoragePoolXML
    { connectionToPtr `Connection',
                      `String',
      id              `CUInt' } -> `StoragePool' ptrToStoragePool* #}

{# fun virStoragePoolBuild as buildStoragePool
    { storagePoolToPtr `StoragePool',
      flags2int        `[StoragePoolBuildFlags]' } -> `Int' exceptionOnMinusOne* #}

{# fun virStoragePoolUndefine as undefineStoragePool
    { storagePoolToPtr `StoragePool' } -> `Int' exceptionOnMinusOne* #}

{# fun virStoragePoolCreate as createStoragePool
    { storagePoolToPtr `StoragePool',
      id               `CUInt' } -> `Int' exceptionOnMinusOne* #}

{# fun virStoragePoolDestroy as destroyStoragePool
    { storagePoolToPtr `StoragePool' } -> `Int' exceptionOnMinusOne* #}

{# fun virStoragePoolDelete as deleteStoragePool
    { storagePoolToPtr `StoragePool',
      flags2int        `[StoragePoolDeleteFlags]' } -> `Int' exceptionOnMinusOne* #}

{# fun virStoragePoolRef as refStoragePool
    { storagePoolToPtr `StoragePool' } -> `Int' exceptionOnMinusOne* #}

{# fun virStoragePoolFree as freeStoragePool
    { storagePoolToPtr `StoragePool' } -> `Int' exceptionOnMinusOne* #}

{# fun virStoragePoolRefresh as refreshStoragePool
    { storagePoolToPtr `StoragePool',
      id               `CUInt' } -> `Int' exceptionOnMinusOne* #}

{# fun virStoragePoolGetName as getStoragePoolName
    { storagePoolToPtr `StoragePool' } -> `String' #}

getStoragePoolInfo :: StoragePool -> IO StoragePoolInfo
getStoragePoolInfo ptr = do
  allocaBytes {# sizeof virStoragePoolInfo #} $ \iptr -> do
         i <- {# call virStoragePoolGetInfo #} (storagePoolToPtr ptr) iptr
         state      <- {# get StoragePoolInfo->state      #} iptr
         capacity   <- {# get StoragePoolInfo->capacity   #} iptr
         allocation <- {# get StoragePoolInfo->allocation #} iptr
         available  <- {# get StoragePoolInfo->available  #} iptr
         return $ StoragePoolInfo {
                    spiState      = toEnum (fromIntegral state),
                    spiCapacity   = fromIntegral capacity,
                    spiAllocation = fromIntegral allocation,
                    spiAvailable  = fromIntegral available }

{# fun virStoragePoolGetXMLDesc as getStoragePoolXML
    { storagePoolToPtr `StoragePool',
      flags2int        `[StorageXMLFlags]' } -> `String' #}

getStoragePoolAutostart :: StoragePool -> IO Bool
getStoragePoolAutostart ptr = do
  alloca $ \aptr -> do
    {# call virStoragePoolGetAutostart #} (storagePoolToPtr ptr) aptr >>=
      exceptionOnMinusOne
    toBool <$> peek aptr

{# fun virStoragePoolSetAutostart as setStoragePoolAutostart
    { storagePoolToPtr `StoragePool',
      fromBool         `Bool' } -> `Int' exceptionOnMinusOne* #}

{# fun virStoragePoolIsActive as storagePoolIsActive
    { storagePoolToPtr `StoragePool' } -> `Bool' boolExceptionOnMinusOne* #}

{# fun virStoragePoolIsPersistent as storagePoolIsPersistent
    { storagePoolToPtr `StoragePool' } -> `Bool' boolExceptionOnMinusOne* #}

{# fun virStoragePoolNumOfVolumes as storagePoolVolsCount
    { storagePoolToPtr `StoragePool' } -> `Int' exceptionOnMinusOne* #}

storagePoolVolsNames :: StoragePool -> IO [String]
storagePoolVolsNames pool = do
  cn <- {# call virStoragePoolNumOfVolumes #} (storagePoolToPtr pool)
  let n = fromIntegral cn
  allocaArray n $ \nptr -> do
    {# call virStoragePoolListVolumes #} (storagePoolToPtr pool) nptr cn
    mapM peekCString =<< peekArray n nptr

{# fun virStorageVolGetConnect as getStorageVolConnection
    { storageVolToPtr `StorageVol' } -> `Connection' ptrToConnection* #}

{# fun virStorageVolLookupByName as lookupStorageVolName
    { storagePoolToPtr `StoragePool', `String' } -> `StorageVol' ptrToStorageVol* #}

{# fun virStorageVolLookupByKey as lookupStorageVolKey
    { connectionToPtr `Connection', `String' } -> `StorageVol' ptrToStorageVol* #}

{# fun virStorageVolLookupByPath as lookupStorageVolPath
    { connectionToPtr `Connection', `String' } -> `StorageVol' ptrToStorageVol* #}

{# fun virStorageVolGetName as getStorageVolName
    { storageVolToPtr `StorageVol' } -> `String' #}

getStorageVolInfo :: StorageVol -> IO StorageVolInfo
getStorageVolInfo ptr = do
  allocaBytes {# sizeof virStorageVolInfo #} $ \iptr -> do
         i <- {# call virStorageVolGetInfo #} (storageVolToPtr ptr) iptr
         type'      <- {# get StorageVolInfo->type       #} iptr
         capacity   <- {# get StorageVolInfo->capacity   #} iptr
         allocation <- {# get StorageVolInfo->allocation #} iptr
         return $ StorageVolInfo {
                    sviType       = toEnum (fromIntegral type'),
                    sviCapacity   = fromIntegral capacity,
                    sviAllocation = fromIntegral allocation }

{# fun virStorageVolGetXMLDesc as getStorageVolXML
    { storageVolToPtr `StorageVol',
      id              `CUInt' } -> `String' #}

{# fun virStorageVolGetPath as getStorageVolPath
    { storageVolToPtr `StorageVol' } -> `String' #}

{# fun virStorageVolGetKey as getStorageVolKey
    { storageVolToPtr `StorageVol' } -> `String' #}

{# fun virStorageVolResize as resizeStorageVol
    { storageVolToPtr `StorageVol',
      id              `CULLong',
      id              `CUInt' } -> `Int' exceptionOnMinusOne* #}

{# fun virStorageVolCreateXML as createStorageVolXML
    { storagePoolToPtr `StoragePool',
                       `String',
      flags2int        `[StorageVolCreateFlags]' } -> `StorageVol' ptrToStorageVol* #}

{# fun virStorageVolCreateXMLFrom as createStorageVolXMLFrom
    { storagePoolToPtr `StoragePool',
                       `String',
      storageVolToPtr  `StorageVol',
      flags2int        `[StorageVolCreateFlags]' } -> `StorageVol' ptrToStorageVol* #}

downloadStorageVol :: FilePath -> StorageVol -> IO ()
downloadStorageVol path vol = downloadStorageVolPartial path vol 0 0

downloadStorageVolPartial :: FilePath -> StorageVol -> CULLong -> CULLong -> IO ()
downloadStorageVolPartial path vol off len = do
  sink <- mkSink $ \st buf nbytes opaque -> do
    fd <- peek (castPtr opaque)
    fromIntegral <$> fdWriteBuf fd (castPtr buf) (fromIntegral nbytes)
  conn <- getStorageVolConnection vol
  st <- newStream conn []
  {# call virStorageVolDownload #}
    (storageVolToPtr vol) (streamToPtr st) off len 0 >>= exceptionOnMinusOne
  fd <- createFile path stdFileMode
  alloca $ \fdptr -> do
    poke fdptr fd
    {# call virStreamRecvAll #}
      (streamToPtr st) sink (castPtr fdptr) >>= exceptionOnMinusOne
    closeFd fd
    finishStream st
    freeStream st
    freeHaskellFunPtr sink

type SinkCallback a = Ptr () -> Ptr CChar -> CULong -> Ptr () -> IO CInt

foreign import ccall "wrapper"
  mkSink :: SinkCallback a -> IO (FunPtr (SinkCallback a))

uploadStorageVol :: FilePath -> StorageVol -> IO ()
uploadStorageVol path vol = uploadStorageVolPartial path vol 0 0

uploadStorageVolPartial :: FilePath -> StorageVol -> CULLong -> CULLong -> IO ()
uploadStorageVolPartial path vol off len = do
  source <- mkSource $ \st buf nbytes opaque -> do
    fd <- peek (castPtr opaque)
    fromIntegral <$> fdReadBuf fd (castPtr buf) (fromIntegral nbytes)
  conn <- getStorageVolConnection vol
  st <- newStream conn []
  {# call virStorageVolUpload #}
    (storageVolToPtr vol) (streamToPtr st) off len 0 >>= exceptionOnMinusOne
  fd <- openFd path ReadOnly Nothing defaultFileFlags
  alloca $ \fdptr -> do
    poke fdptr fd
    {# call virStreamSendAll #}
      (streamToPtr st) source (castPtr fdptr) >>= exceptionOnMinusOne
    closeFd fd
    finishStream st
    freeStream st
    freeHaskellFunPtr source

type SourceCallback a = Ptr () -> Ptr CChar -> CULong -> Ptr () -> IO CInt

foreign import ccall "wrapper"
  mkSource :: SourceCallback a -> IO (FunPtr (SourceCallback a))

{# fun virStorageVolDelete as deleteStorageVol
    { storageVolToPtr `StorageVol',
      id               `CUInt' } -> `Int' exceptionOnMinusOne* #}

{# fun virStorageVolWipe as wipeStorageVol
    { storageVolToPtr `StorageVol',
      id              `CUInt' } -> `Int' exceptionOnMinusOne* #}

{# fun virStorageVolWipePattern as wipeStorageVolWith
    { storageVolToPtr `StorageVol',
                      `StorageVolWipeAlgorithm',
      id              `CUInt' } -> `Int' exceptionOnMinusOne* #}

{# fun virStorageVolRef as refStorageVol
    { storageVolToPtr `StorageVol' } -> `Int' exceptionOnMinusOne* #}

{# fun virStorageVolFree as freeStorageVol
    { storageVolToPtr `StorageVol' } -> `Int' exceptionOnMinusOne* #}

type ConnectDomainEventGenericCallback a = Connection -> Domain -> Ptr a -> IO ()
type FreeCallback = Ptr () -> IO ()


type ConnectDomainEventCallback a = Connection -> Domain -> Int -> Int -> Ptr a -> IO ()

foreign import ccall "wrapper"
    mkConnectDomainEventCallback :: ConnectDomainEventCallback a
                                 -> IO (FunPtr (ConnectDomainEventCallback a))

foreign import ccall "wrapper"
  mkConnectDomainEventGenericCallback :: ConnectDomainEventGenericCallback a
                                      -> IO (FunPtr (ConnectDomainEventGenericCallback a))

foreign import ccall "wrapper"
  mkFreeCallback :: FreeCallback -> IO (FunPtr FreeCallback)

