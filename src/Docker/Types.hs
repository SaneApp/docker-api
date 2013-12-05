{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
module Docker.Types where
import Control.Applicative
import Control.Lens
import Control.Lens.TH
import Data.ByteString.Char8 (pack)
import Data.Char
import Data.Default
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Lens.TH
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text, unpack)
import Data.Time
import Data.Time.Clock.POSIX
import Network.HTTP.Conduit
import qualified Network.HTTP.API as API
import Prelude hiding (all)
import URI.TH
import URI.Types

runDocker :: String -> DockerM a -> IO (Either API.APIError a)
runDocker uri = API.runAPIClient uri (\r -> r { responseTimeout = Nothing }) . fromDockerM

type ContainerId = Text
type ImageName = Text

val :: (ToTemplateValue l SingleElement) => String -> Getter s (Maybe l) -> s -> Maybe (String, TemplateValue SingleElement)
val str l x = maybe Nothing (\v -> Just (str, v)) mv
  where mv = fmap toTemplateValue (x ^. l)

val' :: (ToTemplateValue l SingleElement) => String -> Getter s l -> s -> Maybe (String, TemplateValue SingleElement)
val' str l x = Just (str, toTemplateValue (x ^. l))


mkAssoc x = Associative . catMaybes . map ($ x)

get :: FromJSON a => String -> DockerM a
get = DockerM . fmap responseBody . API.get . pack
post str = DockerM . fmap responseBody . API.post (pack str)

delete :: FromJSON a => String -> DockerM a
delete str = DockerM $ API.APIClient $ do
  (API.ClientSettings req man middleware) <- ask
  let r = middleware $ req { path = pack str, method = "DELETE" }
  resp <- lift $ lift $ httpLbs r man
  fmap responseBody $ API.fromAPIClient $ API.jsonize resp

post' :: FromJSON a => String -> DockerM a
post' str = DockerM $ API.APIClient $ do
  (API.ClientSettings req man middleware) <- ask
  let r = middleware $ req { path = pack str, method = "POST" }
  resp <- lift $ lift $ httpLbs r man
  fmap responseBody $ API.fromAPIClient $ API.jsonize resp

newtype DockerM a = DockerM { fromDockerM :: API.APIClient a }
  deriving (Monad, MonadIO, Functor, Applicative)

data NewContainer = NewContainer
  { _ncHostname :: Text
  , _ncUser :: Text
  , _ncMemory :: Int
  , _ncMemorySwap :: Int
  , _ncAttachStdin :: Bool
  , _ncAttachStdout :: Bool
  , _ncAttachStderr :: Bool
  , _ncPortSpecs :: Maybe Object
  , _ncPrivileged :: Bool
  , _ncTty :: Bool
  , _ncOpenStdin :: Bool
  , _ncStdinOnce :: Bool
  , _ncEnv :: Maybe Object
  , _ncCmd :: [Text]
  , _ncDns :: Maybe Object
  , _ncImage :: Text
  , _ncVolumes :: Object
  , _ncVolumesFrom :: Text
  , _ncWorkingDir :: Text
  } deriving (Show)

data CreatedContainerResponse = CreatedContainerResponse
  { _ccrId :: Text
  , _ccrWarnings :: [Text]
  } deriving (Show)

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

data ContainerConfig = ContainerConfig
  { _ccreqHostname :: Text
  , _ccreqDomainName :: Text
  , _ccreqUser :: Text
  , _ccreqMemory :: Int
  , _ccreqMemorySwap :: Int
  , _ccreqAttachStdin :: Bool
  , _ccreqAttachStdout :: Bool
  , _ccreqAttachStderr :: Bool
  , _ccreqPortSpecs :: Maybe Object
  , _ccreqPrivileged :: Bool
  , _ccreqTty :: Bool
  , _ccreqOpenStdin :: Bool
  , _ccreqStdinOnce :: Bool
  , _ccreqEnv :: Maybe Object
  , _ccreqCmd :: [Text]
  , _ccreqDns :: Maybe Object
  , _ccreqImage :: Text
  , _ccreqVolumes :: Object
  , _ccreqVolumesFrom :: Text
  , _ccreqWorkingDir :: Text
  } deriving (Show)

data CreatedContainer = CreatedContainer
  { _ccrespId :: Text
  , _ccrespWarning :: Text
  } deriving (Show)

data ContainerState = ContainerState
  { _csRunning :: Bool
  , _csPid :: Int
  , _csExitCode :: Int
  , _csStartedAt :: UTCTime
  , _csFinishedAt :: Maybe UTCTime
  , _csGhost :: Bool
  } deriving (Show)

data NetworkSettings = NetworkSettings
  { _nsIPAddress :: Text
  , _nsIPPrefixLen :: Int
  , _nsGateway :: Text
  , _nsBridge :: Text
  , _nsPortMapping :: Maybe Object
  , _nsPorts :: Maybe Object
  } deriving (Show)

data ContainerInfo = ContainerInfo
  { _ciID :: Text
  , _ciCreated :: UTCTime
  , _ciPath :: Text
  , _ciArgs :: [Text]
  -- , _ciConfig :: ContainerConfig
  , _ciState :: ContainerState
  , _ciImage :: Text
  , _ciNetworkSettings :: NetworkSettings
  , _ciSysInitPath :: Text
  , _ciResolvConfPath :: Text
  , _ciVolumes :: Maybe Object
  } deriving (Show)

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
  { _iiRepository :: Maybe Text
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

data Event = Event
  { _eStatus :: Text
  , _eId :: Text
  , _eFrom :: Text
  , _eTime :: Int
  } deriving (Show)

data CommitOptions = CommitOptions
  { _coContainer :: Text
  , _coRepo :: Text
  , _coMessage :: Maybe Text
  , _coTag :: Maybe Text
  , _coAuthor :: Maybe Text
  , _coRun :: Maybe Text
  } deriving (Show)

data CommittedImage = CommittedImage
  { _committedId :: Text
  } deriving (Show)

data PushOptions = PushOptions
  { _pRegistry :: Maybe Text
  } deriving (Show)

data TagOptions = TagOptions
  { _toRepo :: Text
  , _toForce :: Maybe Bool
  } deriving (Show)

data SearchOptions = SearchOptions
  { _soTerm :: Text
  } deriving (Show)

data SearchResult = SearchResult
  { _srDescription :: Text
  , _srIsOfficial :: Bool
  , _srIsTrusted :: Bool
  , _srName  :: Text
  , _srStarCount :: Int
  } deriving (Show)

data AttachOptions = AttachOptions
  { _aoLogs :: Bool
  , _aoStream :: Bool
  , _aoStdIn :: Bool
  , _aoStdOut :: Bool
  , _aoStdErr :: Bool
  } deriving (Show)

data InsertOptions = InsertOptions
  { _ioPath :: Text
  , _ioUrl :: Text
  } deriving (Show)

data BuildOptions = BuildOptions
  { _boRepoAndTag :: Text
  , _boSuppressVerboseOutput :: Maybe Bool
  , _boNoCache :: Maybe Bool
  } deriving (Show)

data RemoveContainerOptions = RemoveContainerOptions
  { _rcRemoveVolumes :: Maybe Bool
  } deriving (Show)

data ListRunningProcessOptions = ListRunningProcessOptions
  { _lrpPsArgs :: Maybe Text
  } deriving (Show)

data StatusUpdate
  = StatusUpdate
    { _icStatus :: Text
    , _icProgress :: Maybe Text
    }
  | ErrorStatus
    { _icError :: Text
    }
  deriving (Show)

data DeletionInfo
  = Untagged Text
  | Deleted Text
  deriving (Show)

data AuthInfo = AuthInfo
  { _aiUsername :: Text
  , _aiPassword :: Text
  , _aiEmail :: Text
  , _aiServerAddress :: Maybe Text
  } deriving (Show)

makeFields ''NewContainer
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
makeFields ''Event
makeFields ''CommittedImage
makeFields ''PushOptions
makeFields ''TagOptions
makeFields ''SearchOptions
makeFields ''AttachOptions
makeFields ''InsertOptions
makeFields ''RemoveContainerOptions
makeFields ''ListRunningProcessOptions
makeFields ''ContainerConfig
makeFields ''NetworkSettings
makeFields ''ContainerInfo
makeFields ''CreatedContainerResponse
makeFields ''AuthInfo
makeFields ''CommitOptions
makeFields ''BuildOptions

fmap concat $ mapM (deriveJSON defaultOptions { fieldLabelModifier = dropWhile (not . isUpper) })
  [ ''NewContainer
  , ''StatusCodeResult
  , ''ContainerPort
  , ''ContainerSummary
  , ''CreatedContainer
  , ''ContainerState
  , ''RunningProcesses
  , ''FileSystemChange
  , ''StartSettings
  , ''WaitResult
  , ''CopyFile
  , ''SystemInfo
  , ''VersionInfo
  , ''HistoryInfo
  , ''Event
  , ''CommittedImage
  , ''ContainerConfig
  , ''NetworkSettings
  , ''ContainerInfo
  , ''CreatedContainerResponse
  , ''DeletionInfo
  , ''AuthInfo
  ]

fmap concat $ mapM (deriveJSON defaultOptions { fieldLabelModifier = API.snakeCase . dropWhile (not . isUpper) })
  [ ''ImageInfo
  , ''SearchResult
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
  toTemplateValue x = mkAssoc x [ val "t" t ]

instance ToTemplateValue RestartContainerOptions AssociativeListElement where
  toTemplateValue x = mkAssoc x [ val "t" t ]

instance ToTemplateValue ListImagesOptions AssociativeListElement where
  toTemplateValue x = mkAssoc x [ val "all" all ]

instance ToTemplateValue ListRunningProcessOptions AssociativeListElement where
  toTemplateValue x = mkAssoc x
    [ val "ps_args" psArgs
    ]

instance ToTemplateValue RemoveContainerOptions AssociativeListElement where
  toTemplateValue x = mkAssoc x
    [ val "v" removeVolumes
    ]

instance ToTemplateValue TagOptions AssociativeListElement where
  toTemplateValue x = mkAssoc x
    [ val' "repo" repo
    , val "force" force
    ]

instance ToTemplateValue SearchOptions AssociativeListElement where
  toTemplateValue x = mkAssoc x
    [ val' "term" term
    ]

instance ToTemplateValue BuildOptions AssociativeListElement where
  toTemplateValue x = mkAssoc x
    [ val' "t" repoAndTag
    , val "q" suppressVerboseOutput
    , val "nocache" noCache
    ]

instance ToTemplateValue CommitOptions AssociativeListElement where
  toTemplateValue x = mkAssoc x
    [ val' "container" container
    , val' "repo" repo
    , val "tag" tag
    , val "m" message
    , val "author" author
    , val "run" run
    ]

instance ToTemplateValue UTCTime SingleElement where
  toTemplateValue = Single . show . (\x -> (round x) :: Int) . utcTimeToPOSIXSeconds

instance ToTemplateValue Text SingleElement where
  toTemplateValue = Single . unpack
