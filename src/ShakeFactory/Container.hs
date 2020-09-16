-- | Shake functions for building containers
module ShakeFactory.Container
  ( publishContainer,
    buildContainer,
  )
where

import Development.Shake
import Development.Shake.FilePath (splitDirectories)
import ShakeFactory

publishContainer :: String -> Action ()
publishContainer imageRef = do
  gitVer <- projectVersion
  cmd_ "podman push" imageRef
  cmd_ "podman push" (imageRef <> ":" <> gitVer)

-- |
buildContainer :: FilePath -> String -> Action ()
buildContainer containerFile imageRef = do
  need [containerFile]
  cwd <- getEnvWithDefault "." "PWD"
  let projectName = last (splitDirectories cwd)
  let containerfileName = last (splitDirectories containerFile)
  let containerfileDir = case splitDirectories containerFile of
        [_] -> "."
        xs -> reverse xs !! 1
  cmd_
    "podman build"
    ("-v " <> cwd <> ":/usr/src/" <> projectName <> ":Z")
    ("-t " <> imageRef)
    ("-f " <> containerfileName)
    containerfileDir
  gitVer <- projectVersion
  cmd_ "podman tag" imageRef (imageRef <> ":" <> gitVer)
