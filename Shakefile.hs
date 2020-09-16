import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import qualified Dhall
import ShakeFactory
import ShakeFactory.Dhall
import System.Environment (setEnv)

publishContainer :: Action ()
publishContainer = do
  gitVer <- readFile' "build/container"
  cmd_ "podman push" imageRef
  cmd_ "podman push" (imageRef <> ":" <> gitVer)

projectVersion :: Action String
projectVersion = do
  Stdout gitVerInfo <- command [] "git" ["log", "--oneline", "-n", "1"]
  pure (head (words gitVerInfo))

buildContainer :: Action ()
buildContainer = do
  need ["build/Containerfile"]
  cwd <- getEnvWithDefault "." "PWD"
  cmd_ "podman build -v" (cwd <> ":/usr/src/shake-factory:Z") "-t" imageRef "-f Containerfile build"
  gitVer <- projectVersion
  cmd_ "podman tag" imageRef (imageRef <> ":" <> gitVer)
  writeFile' "build/container" gitVer

imageRef :: String
imageRef = "quay.io/software-factory/shake-factory"

containerfileDhallPackage = base <> tag <> "/package.dhall " <> hash
  where
    base = "https://raw.githubusercontent.com/softwarefactory-project/dhall-containerfile/"
    tag = "0.1.0"
    hash = "sha256:9ee58096e7ab5b30041c2a2ff0cc187af5bff6b4d7a6be8a6d4f74ed23fe7cdf"

generateContainerFile :: Action ()
generateContainerFile = do
  need ["Containerfile.dhall"]
  gitVer <- projectVersion
  liftIO (setEnv "GIT_REV" gitVer)
  containerFile <- liftIO $ T.unpack <$> Dhall.input Dhall.strictText (T.pack (renderContainerfile <> "./Containerfile.dhall"))
  writeFile' "build/Containerfile" containerFile
  where
    renderContainerfile = "( " <> containerfileDhallPackage <> " ).render "

main :: IO ()
main = shakeMain $ do
  want ["install", ".zuul.yaml"]
  ".zuul.yaml" %> dhallZuulAction "(./.sf.dhall).zuul"
  "build/Containerfile" %> const generateContainerFile
  "build/container" %> const buildContainer
  phony imageRef publishContainer
  phony "test" cabalTest
  phony "install" $ need ["test"] >> cabalInstallLib "lib:shake-factory dhall shake shake-dhall text"
  cabalDocs
  cleanRules
