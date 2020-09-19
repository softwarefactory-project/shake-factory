-- | Shake functions for haskell project
module ShakeFactory.Haskell
  ( -- * Actions
    stackInstallRoot,
    stackBuild,
    generateContainerFile,
    buildContainerImage,

    -- * Re-exports
    shakeMain,
    phony,
    publishContainer,
  )
where

import Development.Shake
import Development.Shake.FilePath
import ShakeFactory
import ShakeFactory.Container
import ShakeFactory.Dhall
import System.Directory

stackInstallRoot :: Action FilePath
stackInstallRoot =
  do
    Stdout path <- command [] "stack" ["path", "--local-install-root"]
    cwd <- liftIO (makeAbsolute ".")
    pure (makeRelative cwd (stripLineReturn path))
  where
    stripLineReturn = reverse . dropWhile (== '\n') . reverse

stackBuild :: Action ()
stackBuild = cmd_ "stack build"

generateContainerFile :: String -> String -> Action ()
generateContainerFile imageExpr dhallContainerFileVersion = do
  need [".sf.dhall"]
  setEnvFromAction projectVersion "GIT_REV"
  setEnvFromAction stackInstallRoot "STACK_PATH"
  setEnvFromAction partialOsVersionId "FEDORA_RELEASE"
  dhallContainerAction imageExpr dhallContainerFileVersion "Containerfile"

buildContainerImage :: String -> Action ()
buildContainerImage imageRef = do
  need ["build", "Containerfile"]
  buildContainer "Containerfile" imageRef
