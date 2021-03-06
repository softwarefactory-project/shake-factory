import Development.Shake
import ShakeFactory
import ShakeFactory.Container
import ShakeFactory.Dhall
import ShakeFactory.Javascript

imageRef :: String
imageRef = "quay.io/software-factory/shake-factory"

containerFile :: String
containerFile = "build/Containerfile"

builderRef :: String
builderRef = "quay.io/software-factory/stack-builder"

builderFile :: String
builderFile = "build/Containerfile-haskell-builder"

nodejsBuilderRef :: String
nodejsBuilderRef = "quay.io/software-factory/nodejs-builder"

nodejsBuilderFile :: String
nodejsBuilderFile = "build/Containerfile-nodejs-builder"

generateContainerFile :: Action ()
generateContainerFile = do
  need ["Containerfile.dhall"]
  setEnvFromAction projectVersion "GIT_REV"
  dhallContainerAction "./Containerfile.dhall" "0.1.0" containerFile

main :: IO ()
main = shakeMain $ do
  want ["install", ".zuul.yaml"]
  ".zuul.yaml" %> dhallZuulAction "(./.sf.dhall).zuul"
  phony "build/Containerfile" generateContainerFile
  phony "container" (generateContainerFile >> buildContainer containerFile imageRef)
  phony imageRef (publishContainer imageRef)
  phony "test" cabalTest
  phony "install" $ need ["test"] >> cabalInstallLib "lib:shake-factory dhall shake shake-dhall text"
  cabalDocs
  cleanRules
  -- Builder container rules
  phony "builder-container" (dhallContainerAction "(./package.dhall).Container.Builder" "0.1.0" builderFile >> buildContainer builderFile builderRef)
  phony builderRef (publishContainer builderRef)
  phony
    "nodejs-builder-container"
    ( do
        dhallJson "(./package.dhall).Container.NodeDependencies" "build/package.json"
        dhallContainerAction "(./package.dhall).Container.NodeBuilder" "0.3.0" nodejsBuilderFile
        buildContainer nodejsBuilderFile nodejsBuilderRef
    )
  phony nodejsBuilderRef (publishContainer nodejsBuilderRef)
