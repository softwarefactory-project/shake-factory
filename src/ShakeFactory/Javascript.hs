-- | Shake functions for javascript project
module ShakeFactory.Javascript
  ( -- * Actions
    containerRules,

    -- * Re-exports
    shakeMainConfig,
    phony,
    publishContainer,
  )
where

import Development.Shake
import ShakeFactory
import ShakeFactory.Config
import ShakeFactory.Container
import ShakeFactory.Dhall

generateNodeContainerFile :: SoftwareFactoryConfig -> FilePath -> Action ()
generateNodeContainerFile conf fp =
  do
    setEnvFromAction projectVersion "GIT_REV"
    dhallContainerAction ("(" <> imageExpr <> ")") dhallContainerFileVersion fp
  where
    showV v = "\"" <> v <> "\""
    dhallContainerFileVersion = "0.3.0"
    imageExpr = "(" <> shakePackage <> ").Container.Node " <> imageArgs
    imageArgs = showV (builderRef (container conf)) <> " " <> showV (name conf)
    shakePackage = "~/src/softwarefactory-project.io/software-factory/shake-factory/package.dhall"

containerRules :: SoftwareFactoryConfig -> Rules ()
containerRules config =
  do
    phony "build/Containerfile" gen
    phony "container" (gen >> build)
    phony imageRef' (publishContainer imageRef')
  where
    imageRef' = imageRef (container config)
    gen = generateNodeContainerFile config "build/Containerfile"
    build = buildContainer "build/Containerfile" imageRef'
