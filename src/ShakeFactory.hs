-- |
-- Copyright: (c) 2020 Red Hat
-- SPDX-License-Identifier: Apache-2.0
-- Maintainer: Software Factory Dev <softwarefactory-dev@redhat.com>
--
-- See README for more info
module ShakeFactory
  ( shakeMain,
    getHome,
    homeRelative,
    clean,
    cabalDocs,
    cabalInstallLib,
    cabalTest,
  )
where

import Data.Maybe
import Development.Shake
import Development.Shake.FilePath

cabalInstallLib :: String -> Action ()
cabalInstallLib lib = do
  -- See https://github.com/haskell/cabal/issues/6394
  homeGhc <- homeRelative ".ghc"
  cmd_ $ "rm -Rf " <> homeGhc
  cmd_ $ "cabal install --lib " <> lib

cabalTest :: Action ()
cabalTest = cmd_ "cabal build --enable-tests" >> cmd_ "cabal test"

getHome :: Action String
getHome = fromMaybe "/root/" <$> getEnv "HOME"

homeRelative :: String -> Action String
homeRelative path = do
  home <- getHome
  pure (home </> path)

cabalDocs :: Rules ()
cabalDocs = do
  phony "docs" $ need ["build/docs/index.html"]
  "build/docs/index.html" %> \dest -> do
    Stdout out <- command [] "cabal" ["haddock"]
    let docDir = (takeDirectory $ last $ lines $ out) <> "/"
    let outDir = "build/docs/"
    cmd_ "mkdir -p" outDir
    cmd_ "rsync -a --delete" docDir outDir

clean :: Rules ()
clean = phony "clean" $ do
  putInfo "Cleaning files in _build"
  removeFilesAfter "_build" ["//*"]

shakeMain :: Rules () -> IO ()
shakeMain = shakeArgs shakeOptions {shakeFiles = "_build"}
