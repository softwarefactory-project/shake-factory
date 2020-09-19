import Development.Shake
import ShakeFactory
import ShakeFactory.Container
import ShakeFactory.Dhall

imageRef :: String
imageRef = "quay.io/software-factory/shake-factory"

containerFile :: String
containerFile = "build/Containerfile"

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
