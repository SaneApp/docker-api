{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, MultiParamTypeClasses, FlexibleContexts, Rank2Types, FunctionalDependencies, FlexibleInstances #-}
module Docker.Types where
import qualified API as API
import Control.Applicative
import Control.Lens
import Control.Lens.TH
import Data.ByteString.Char8 (pack)
import Data.Char
import Data.Default
import Data.Maybe
import Control.Monad.Trans
import Control.Lens.TH
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text, unpack)
import Data.Time
import Network.HTTP.Conduit
import Prelude hiding (all)
import URI.TH
import URI.Types

runDocker uri = API.runAPIClient uri (\r -> r { responseTimeout = Nothing }) . fromDockerM

type ContainerId = Text
type ImageName = Text

val :: (ToTemplateValue l SingleElement) => String -> Getter s (Maybe l) -> s -> Maybe (String, TemplateValue SingleElement)
val str l x = maybe Nothing (\v -> Just (str, v)) mv
  where mv = fmap toTemplateValue (x ^. l)

mkAssoc x = Associative . catMaybes . map ($ x)

get :: FromJSON a => String -> DockerM a
get = DockerM . fmap responseBody . API.get . pack
post str = DockerM . fmap responseBody . API.post (pack str)
delete str = DockerM . fmap responseBody . API.delete (pack str)

newtype DockerM a = DockerM { fromDockerM :: API.APIClient a }
  deriving (Monad, MonadIO, Functor, Applicative)

data ListContainerOptions = ListContainerOptions
  { _lcoAll :: Maybe Bool
  , _lcoLimit :: Maybe Int
  , _lcoSince :: Maybe ContainerId
  , _lcoBefore :: Maybe ContainerId
  , _lcoSize :: Maybe Bool
  } deriving (Show)

data ContainerPort = ContainerPort
  { _cpPrivatePort :: Int
  , _cpPublicPort :: Int
  , _cpType :: Text
  } deriving (Show)

data ContainerSummary = ContainerSummary
  { _csId :: Text
  , _csImage :: Text
  , _csCommand :: Text
  , _csCreated :: Int
  , _csStatus :: Text
  , _csPorts :: Maybe [ContainerPort]
  , _csSizeRw :: Int
  , _csSizeRootFs :: Int
  } deriving (Show)

{-
data ContainerConfig = ContainerConfig
  { _ccreqHostname :: Text
  , _ccreqUser :: Text
  , _ccreqMemory :: Int
  , _ccreqMemorySwap :: Int
  , _ccreqAttachStdin :: Bool
  , _ccreqAttachStdout :: Bool
  , _ccreqAttachStderr :: Bool
  , _ccreqPortSpecs :: Maybe ____
  , _ccreqPrivileged :: Bool
  , _ccreqTty :: Bool
  , _ccreqOpenStdin :: Bool
  , _ccreqStdinOnce :: Bool
  , _ccreqEnv :: Maybe ____
  , _ccreqCmd :: [Text]
  , _ccreqDns :: Maybe ____
  , _ccreqImage :: Text
  , _ccreqVolumes :: ____
  , _ccreqVolumesFrom :: Text
  , _ccreqWorkingDir :: Text
  }
-}

data CreatedContainer = CreatedContainer
  { _ccrespId :: Text
  , _ccrespWarning :: Text
  } deriving (Show)

data ContainerState = ContainerState
  { _csRunning :: Bool
  , _csPid :: Int
  , _csExitCode :: Int
  , _csStartedAt :: UTCTime
  , _csGhost :: Bool
  } deriving (Show)

{-
data NetworkSettings = NetworkSettings
  { _nsIpAddress :: Text
  , _nsIpPrefixLen :: Int
  , _nsGateway :: Text
  , _nsBridge :: Text
  , _nsPortMapping :: Maybe ____
  }

data ContainerInfo = ContainerInfo
  { _ciId :: Text
  , _ciCreated :: UTCTime
  , _ciPath :: Text
  , _ciArgs :: Text
  , _ciConfig :: ContainerConfig
  , _ciState :: ContainerState
  , _ciImage :: Text
  , _ciNetworkSettings :: NetworkSettings
  , _ciSysInitPath :: Text
  , _ciResolvConfPath :: Text
  , _ciVolumes :: ____
  }
-}

data RunningProcesses = RunningProcesses
  { _rpTitles :: [Text]
  , _rpProcesses :: [[Text]]
  } deriving (Show)

data FileSystemChange = FileSystemChange
  { _fscPath :: Text
  , _fscKind :: Int
  } deriving (Show)

data StartSettings = StartSettings
  { _ssBinds :: [Text]
  , _ssLxcConf :: [(Text, Text)]
  } deriving (Show)

data WaitResult = WaitResult
  { _wrStatusCode :: Int
  } deriving (Show)

data CopyFile = CopyFile
  { _cfResource :: Text
  } deriving (Show)

data ImageInfo = ImageInfo
  { _iiRepository :: Text
  , _iiTag :: Text
  , _iiId :: Text
  , _iiCreated :: UTCTime
  , _iiSize :: Int
  , _iiVirtualSize :: Int
  } deriving (Show)

data ListImagesOptions = ListImageOptions
  { _lioAll :: Maybe Bool
  } deriving (Show)

data CreateImageOptions = CreateImageOptions
  { _cioFromImage :: Maybe Text
  , _cioFromSrc :: Maybe Text
  , _cioRepo :: Maybe Text
  , _cioTag :: Maybe Text
  , _cioRegistry :: Maybe Text
  } deriving (Show)

data StopContainerOptions = StopContainerOptions
  { _stopcoT :: Maybe Int
  } deriving (Show)

data RestartContainerOptions = RestartContainerOptions
  { _rcoT :: Maybe Int
  } deriving (Show)

data SystemInfo = SystemInfo
  { _siDebug :: Bool
  , _siContainers :: Int
  , _siImages :: Int
  , _siNFd :: Int
  , _siNGoroutines :: Int
  , _siMemoryLimit :: Bool
  , _siIPv4Forwarding :: Bool
  , _siLXCVersion :: Text
  , _siNEventsListener :: Int
  , _siKernelVersion :: Text
  , _siIndexServerAddress :: Text
  } deriving (Show)

data VersionInfo = VersionInfo
  { _viVersion :: Text
  , _viGitCommit :: Text
  , _viGoVersion :: Text
  } deriving (Show)

data HistoryInfo = HistoryInfo
  { _hiId :: Text
  , _hiCreated :: UTCTime
  , _hiCreatedBy :: Text
  } deriving (Show)

data StatusCodeResult = StatusCodeResult
  { _scrStatusCode :: Int
  } deriving (Show)

makeFields ''StatusCodeResult
makeFields ''ContainerPort
makeFields ''ContainerSummary
makeFields ''CreatedContainer
makeFields ''ContainerState
makeFields ''RunningProcesses
makeFields ''FileSystemChange
makeFields ''StartSettings
makeFields ''WaitResult
makeFields ''CopyFile
makeFields ''ImageInfo
makeFields ''SystemInfo
makeFields ''VersionInfo
makeFields ''HistoryInfo
makeFields ''ListContainerOptions
makeFields ''ListImagesOptions
makeFields ''CreateImageOptions
makeFields ''StopContainerOptions
makeFields ''RestartContainerOptions

fmap concat $ mapM (deriveJSON defaultOptions { fieldLabelModifier = dropWhile (not . isUpper) })
  [ ''StatusCodeResult
  , ''ContainerPort
  , ''ContainerSummary
  , ''CreatedContainer
  , ''ContainerState
  , ''RunningProcesses
  , ''FileSystemChange
  , ''StartSettings
  , ''WaitResult
  , ''CopyFile
  , ''ImageInfo
  , ''SystemInfo
  , ''VersionInfo
  , ''HistoryInfo
  ]

all' :: (Functor f, HasAll c e) => (e -> f e) -> c -> f c
all' = all

instance ToTemplateValue Bool SingleElement where
  toTemplateValue True = Single "true"
  toTemplateValue False = Single "false"

instance Default ListContainerOptions where
  def = ListContainerOptions def def def def def

instance Default ListImagesOptions where
  def = ListImageOptions def

instance Default StopContainerOptions where
  def = StopContainerOptions def

instance Default RestartContainerOptions where
  def = RestartContainerOptions def

instance ToTemplateValue ListContainerOptions AssociativeListElement where
  toTemplateValue x = mkAssoc x
    [ val "all" all
    , val "limit" limit
    , val "since" since
    , val "before" before
    , val "size" size
    ]

instance ToTemplateValue CreateImageOptions AssociativeListElement where
  toTemplateValue x = mkAssoc x
    [ val "fromImage" fromImage
    , val "fromSrc" fromSrc
    , val "repo" repo
    , val "tag" tag
    , val "registry" registry
    ]

instance ToTemplateValue StopContainerOptions AssociativeListElement where

instance ToTemplateValue RestartContainerOptions AssociativeListElement where

instance ToTemplateValue ListImagesOptions AssociativeListElement where

instance ToTemplateValue Text SingleElement where
  toTemplateValue = Single . unpack
