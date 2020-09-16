-- | Shake functions for haskell project
module ShakeFactory.Haskell
  ( -- * Actions
    stackInstallRoot,
    stackBuild,
    generateStackContainerFile,
    buildContainerImage,
    containerRules,

    -- * Re-exports
    shakeMainConfig,
    phony,
    publishContainer,
  )
where

import Development.Shake
import Development.Shake.FilePath
import ShakeFactory
import ShakeFactory.Config
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

generateStackContainerFile :: SoftwareFactoryConfig -> FilePath -> Action ()
generateStackContainerFile conf fp =
  do
    setEnvFromAction projectVersion "GIT_REV"
    dhallContainerAction ("(" <> imageExpr <> ")") dhallContainerFileVersion fp
  where
    showV v = "\"" <> v <> "\""
    dhallContainerFileVersion = "0.3.0"
    imageExpr = "(" <> shakePackage <> ").Container.Stack " <> imageArgs
    imageArgs = showV (builderRef (container conf)) <> " " <> showV (name conf)
    shakePackage = "~/src/softwarefactory-project.io/software-factory/shake-factory/package.dhall"

buildContainerImage :: String -> Action ()
buildContainerImage imageRef' = do
  need ["build", "Containerfile"]
  buildContainer "Containerfile" imageRef'

containerRules :: SoftwareFactoryConfig -> Rules ()
containerRules config =
  do
    phony "build/Containerfile" gen
    phony "container" (gen >> build)
    phony imageRef' (publishContainer imageRef')
  where
    imageRef' = imageRef (container config)
    gen = generateStackContainerFile config "build/Containerfile"
    build = buildContainer "build/Containerfile" imageRef'
