{-# LANGUAGE QuasiQuotes #-}
module Docker where
import API (APIClient(..), ClientSettings(..), jsonize)
import Control.Monad.Reader
import Control.Monad.Trans
import URI.TH
import URI.Types
import Data.Default
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy.Char8 (ByteString)
import Docker.Types hiding (path)
import Network.HTTP.Conduit

type Docker p r = (p -> p) -> DockerM r

listContainers :: Docker ListContainerOptions [ContainerSummary]
listContainers f = get [uri| /containers/json{?opts} |]
  where opts = f def

{-
createContainer :: NewContainer -> Docker CreatedContainerResponse
createContainer = post [uri| /containers/create |]

inspectContainer :: ContainerId -> Docker ContainerInfo
inspectContainer cid = get [uri| /containers/{cid}/json |]

listRunningContainerProcesses :: ContainerId -> PsArgs -> Docker RunningProcesses
listRunningContainerProcesses cid ps_args = get [uri| /containers/{cid}/top{?ps_args} |]
-}

-- Custom client code since docker returns invalid json if no changes exist (docker case 2234)
inspectFilesystemChanges :: ContainerId -> DockerM [FileSystemChange]
inspectFilesystemChanges cid = DockerM $ APIClient $ do
  (ClientSettings req man middleware) <- ask
  let r = middleware $ req { path = pack [uri| /containers/{cid}/changes |] }
  resp <- lift $ lift $ httpLbs r man
  if (responseBody resp == ("null" :: ByteString))
    then return []
    else fmap responseBody $ fromAPIClient $ jsonize resp
{-
exportContainer :: ContainerId -> Docker OutputStream
exportContainer cid = get [uri| /containers/{cid}/export |]
-}
startContainer :: ContainerId -> StartSettings -> DockerM ()
startContainer cid = post [uri| /containers/{cid}/start |]

stopContainer :: ContainerId -> Docker StopContainerOptions ()
stopContainer cid f = post [uri| /containers/{cid}/stop{?opts} |] ()
  where opts = f def

restartContainer :: ContainerId -> Docker RestartContainerOptions ()
restartContainer cid f = post [uri| /containers/{cid}/restart{?opts} |] ()
  where opts = f def

killContainer :: ContainerId -> DockerM ()
killContainer cid = post [uri| /containers/{cid}/kill |] ()
{-
attachContainer :: ContainerId -> Docker AttachOptions OutputStream
attachContainer cid = 
-}
awaitContainerExit :: ContainerId -> DockerM StatusCodeResult
awaitContainerExit cid = post [uri| /containers/{cid}/wait |] ()
{-
removeContainer :: ContainerId -> Docker RemoveOptions ()
removeContainer cid f = delete [uri| /containers/{cid}{?opts} |]
  where opts = f def

copyFile :: ContainerId -> FilePath -> DockerM OutputStream
copyFile cid = post [uri| /containers/{cid}/copy |]
-}
listImages :: Docker ListImagesOptions [ImageInfo]
listImages f = get [uri| /images/json{?opts} |]
  where opts = f def

createImage :: Docker CreateImageOptions CreationStatus
createImage = post [uri| /images/create{?opts} |]
{-
insertFile :: ImageName -> InsertOptions -> Docker InsertionStatus
insertFile = post [uri| /images/{name}/insert{?opts} |]
-}
inspectImage :: ImageName -> DockerM ImageInfo
inspectImage name = get [uri| /images/{name}/json |]

getImageHistory :: ImageName -> DockerM [HistoryInfo]
getImageHistory name = get [uri| /images/{name}/history |]
{-
pushImage :: ImageName -> Docker PushOptions ()
pushImage = post [uri| /images/{name}/push{?opts} |]

tagImage :: ImageName -> TagOptions -> Docker ()
tagImage = post [uri| /images/{name}/tag{?opts} |]

removeImage :: ImageName -> Docker [DeletionInfo]
removeImage = delete [uri| /images/{name} |]

searchImages :: SearchOptions -> Docker [SearchResult]
searchImages = get [uri| /images/search{?opts} |]

buildImage :: InputStream -> BuildOptions -> Docker OutputStream
buildImage = post [uri| /build |]

checkAuthConfiguration :: Docker AuthInfo
checkAuthConfiguration = post [uri| /auth |]
-}
getSystemInformation :: DockerM SystemInfo
getSystemInformation = get [uri| /info |]

getDockerVersionInformation :: DockerM VersionInfo
getDockerVersionInformation = get [uri| /version |]
{-
commitImageChanges :: CommitOptions -> Docker CommittedImage
commitImageChanges = post [uri| /commit{?opts} |]

getEvents :: UTCTime -> Docker [Event]
getEvents t = get [uri| /events{?since} |]
  where since = 

getEventStream :: Docker (Stream Event)
getEventStream = get [uri| /events |]

-}