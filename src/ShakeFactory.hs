-- |
-- Copyright: (c) 2020 Red Hat
-- SPDX-License-Identifier: Apache-2.0
-- Maintainer: Software Factory Dev <softwarefactory-dev@redhat.com>
--
-- See README for more info
module ShakeFactory
  ( shakeMain,
    shakeMainConfig,
    getHome,
    homeRelative,
    cleanRules,
    cabalDocs,
    cabalInstallLib,
    cabalTest,
    projectVersion,
    setEnvFromAction,
    osVersionId,
    partialOsVersionId,
  )
where

import Data.List
import Data.Maybe
import Development.Shake
import Development.Shake.FilePath
import System.Environment (setEnv)
import ShakeFactory.Config (loadConfig, SoftwareFactoryConfig)

projectVersion :: Action String
projectVersion = do
  Stdout gitVerInfo <- command [] "git" ["describe", "--always", "--dirty"]
  pure (head (words gitVerInfo))

cabalInstallLib :: String -> Action ()
cabalInstallLib lib = do
  -- See https://github.com/haskell/cabal/issues/6394
  homeGhc <- homeRelative ".ghc"
  cmd_ $ "rm -Rf " <> homeGhc
  cmd_ $ "cabal install --lib " <> lib

cabalTest :: Action ()
cabalTest = cmd_ "cabal build --enable-tests" >> cmd_ "cabal test"

readOsRelease :: Action [String]
readOsRelease = fmap lines (readFile' "/etc/os-release")

osVersionId :: Action (Maybe String)
osVersionId =
  do
    fileLines <- readOsRelease
    case filter (isPrefixOf versionId) fileLines of
      [x] -> pure (stripPrefix versionId x)
      _ -> pure Nothing
  where
    versionId = "VERSION_ID="

partialOsVersionId :: Action String
partialOsVersionId =
  do
    osVersion <- osVersionId
    case osVersion of
      Just version -> pure version
      Nothing -> error "VERSION_ID missing from os-release"

setEnvFromAction :: Action String -> String -> Action ()
setEnvFromAction valueAction key = do
  value <- valueAction
  liftIO (setEnv key value)

getHome :: Action String
getHome = fromMaybe "/root/" <$> getEnv "HOME"

homeRelative :: String -> Action String
homeRelative path = do
  home <- getHome
  pure (home </> path)

cabalDocs :: Rules ()
cabalDocs = do
  phony "docs" $ need ["build/docs/index.html"]
  "build/docs/index.html" %> \_dest -> do
    Stdout out <- command [] "cabal" ["haddock"]
    let docDir = takeDirectory (last $ lines out) <> "/"
    let outDir = "build/docs/"
    cmd_ "mkdir -p" outDir
    cmd_ "rsync -a --delete" docDir outDir

cleanRules :: Rules ()
cleanRules = phony "clean" $ do
  putInfo "Cleaning files in _build"
  removeFilesAfter "_build" ["//*"]

shakeMain :: Rules () -> IO ()
shakeMain = shakeArgs shakeOptions {shakeFiles = "build"}

shakeMainConfig :: (SoftwareFactoryConfig -> Rules ()) -> IO ()
shakeMainConfig f = do
  conf <- loadConfig ".sf.dhall"
  shakeMain (f conf)
