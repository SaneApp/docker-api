{-# LANGUAGE OverloadedStrings #-}
module Test where
import Control.Lens
import Control.Monad.Trans
import Data.Aeson (encode)
import Data.Conduit (($$+-))
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Default
import Docker
import Docker.Types
import Network.HTTP.API (APIClient(..))

dock m = do
  res <- runDocker "http://127.0.0.1:4243" m
  case res of
    Left err -> do
      putStrLn "error"
      print err
    Right res -> do
      putStr "OK: "
      L.putStrLn $ encode res

dockQuiet m = do
  res <- runDocker "http://127.0.0.1:4243" m
  case res of
    Left err -> putStrLn "error"
    Right res -> putStrLn "OK"

tryExport = runDocker "http://127.0.0.1:4243" $ do
  mCont <- exportContainer stoppedContainerId
  case mCont of
    Nothing -> liftIO $ putStrLn "error exporting"
    Just src -> do
      DockerM $ APIClient $ lift $ lift (src $$+- return ())
      liftIO $ putStrLn "OK"

stoppedContainerId = "83a76f39d3df730c03548cf99fdf6957da006e448ee7c5c589c4b72b763fe753"
test = do
  dockQuiet $ listContainers def
  dockQuiet $ inspectContainer stoppedContainerId
  dockQuiet $ inspectFilesystemChanges stoppedContainerId
  -- dock $ exportContainer stoppedContainerId
  dock $ listImages def
  dockQuiet $ searchImages $ SearchOptions "ubuntu"
  dockQuiet getDockerVersionInformation
  dockQuiet getSystemInformation

