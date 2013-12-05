{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Docker where
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Aeson
import Data.Conduit
import URI.TH
import URI.Types
import Data.Aeson (Object)
import Data.Default
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Time.Clock
import Docker.Types hiding (path)
import Network.HTTP.Conduit
import Network.HTTP.API hiding (get, put, post, delete)

listContainers :: ListContainerOptions -> DockerM [ContainerSummary]
listContainers opts = get [uri| /containers/json{?opts} |]

createContainer :: NewContainer -> DockerM CreatedContainerResponse
createContainer = post [uri| /containers/create |]

inspectContainer :: ContainerId -> DockerM ContainerInfo
inspectContainer cid = get [uri| /containers/{cid}/json |]

listRunningContainerProcesses :: ContainerId -> ListRunningProcessOptions -> DockerM RunningProcesses
listRunningContainerProcesses cid ps_args = get [uri| /containers/{cid}/top{?ps_args} |]

-- Custom client code since docker returns invalid json if no changes exist (docker case 2234)
inspectFilesystemChanges :: ContainerId -> DockerM [FileSystemChange]
inspectFilesystemChanges cid = DockerM $ APIClient $ do
  (ClientSettings req man middleware) <- ask
  let r = middleware $ req { path = pack [uri| /containers/{cid}/changes |] }
  resp <- lift $ lift $ httpLbs r man
  if (responseBody resp == ("null" :: ByteString))
    then return []
    else fmap responseBody $ fromAPIClient $ jsonize resp

exportContainer :: ContainerId -> DockerM ByteString
exportContainer cid = get [uri| /containers/{cid}/export |]

startContainer :: ContainerId -> StartSettings -> DockerM ()
startContainer cid = post [uri| /containers/{cid}/start |]

stopContainer :: ContainerId -> StopContainerOptions -> DockerM ()
stopContainer cid opts = post [uri| /containers/{cid}/stop{?opts} |] ()

restartContainer :: ContainerId -> RestartContainerOptions -> DockerM ()
restartContainer cid opts = post [uri| /containers/{cid}/restart{?opts} |] ()

killContainer :: ContainerId -> DockerM ()
killContainer cid = post [uri| /containers/{cid}/kill |] ()

attachContainer :: ContainerId -> AttachOptions -> DockerM ByteString
attachContainer cid = error "foo"

awaitContainerExit :: ContainerId -> DockerM StatusCodeResult
awaitContainerExit cid = post [uri| /containers/{cid}/wait |] ()

removeContainer :: ContainerId -> RemoveContainerOptions -> DockerM ()
removeContainer cid opts = delete [uri| /containers/{cid}{?opts} |]

copyFile :: ContainerId -> FilePath -> DockerM ByteString
copyFile cid = post [uri| /containers/{cid}/copy |]

listImages :: ListImagesOptions -> DockerM [ImageInfo]
listImages opts = get [uri| /images/json{?opts} |]

-- createImage :: CreateImageOptions -> DockerM (Source (ResourceT IO) StatusUpdate)
-- createImage opts = post' [uri| /images/create{?opts} |]

-- insertFile :: ImageName -> InsertOptions -> DockerM (Source (ResourceT IO) StatusUpdate)
-- insertFile name opts = post' [uri| /images/{name}/insert{?opts} |]

inspectImage :: ImageName -> DockerM ImageInfo
inspectImage name = get [uri| /images/{name}/json |]

getImageHistory :: ImageName -> DockerM [HistoryInfo]
getImageHistory name = get [uri| /images/{name}/history |]

-- pushImage :: ImageName -> PushOptions -> DockerM (Source (ResourceT IO) StatusUpdate)
-- pushImage name opts = post' [uri| /images/{name}/push{?opts} |]

tagImage :: ImageName -> TagOptions -> DockerM ()
tagImage name opts = post' [uri| /images/{name}/tag{?opts} |]

removeImage :: ImageName -> DockerM [DeletionInfo]
removeImage name = delete [uri| /images/{name} |]

searchImages :: SearchOptions -> DockerM [SearchResult]
searchImages opts = get [uri| /images/search{?opts} |]

buildImage :: ByteString -> BuildOptions -> DockerM ByteString
buildImage stream opts = post [uri| /build{?opts} |] stream

checkAuthConfiguration :: AuthInfo -> DockerM Bool
checkAuthConfiguration = post [uri| /auth |]

getSystemInformation :: DockerM SystemInfo
getSystemInformation = get [uri| /info |]

getDockerVersionInformation :: DockerM VersionInfo
getDockerVersionInformation = get [uri| /version |]

commitImageChanges :: CommitOptions -> DockerM CommittedImage
commitImageChanges opts = post' [uri| /commit{?opts} |]

getEvents :: UTCTime -> DockerM [Event]
getEvents t = get [uri| /events{?since} |]
  where since = t

-- getEventStream :: DockerM (Source (ResourceT IO) Event)
-- getEventStream = get [uri| /events |]
