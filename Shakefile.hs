import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import qualified Dhall
import ShakeFactory

publishContainer :: Action ()
publishContainer = do
  gitVer <- readFile' "build/container"
  cmd_ "podman push" imageRef
  cmd_ "podman push" (imageRef <> ":" <> gitVer)

buildContainer :: Action ()
buildContainer = do
  need ["build/Containerfile"]
  cwd <- getEnvWithDefault "." "PWD"
  cmd_ "podman build -v" (cwd <> ":/usr/src/shake-factory:Z") "-t" imageRef "-f Containerfile build"
  Stdout gitVer <- command [] "git" ["log", "--oneline", "-n", "1"]
  writeFile' "build/container" (head (words gitVer))

imageRef :: String
imageRef = "quay.io/software-factory/shake-factory"

containerfileDhallPackage =
  intercalate
    " ? "
    [ "env:DHALL_CONTAINERFILE",
      "~/src/softwarefactory-project.io/software-factory/dhall-containerfile",
      "https://softwarefactory-project.io/cgit/software-factory/dhall-containerfile/plain/package.dhall"
    ]

generateContainerFile :: Action ()
generateContainerFile = do
  need ["Containerfile.dhall"]
  containerFile <- liftIO $ T.unpack <$> Dhall.input Dhall.strictText (T.pack (renderContainerfile <> "./Containerfile.dhall"))
  writeFile' "build/Containerfile" containerFile
  where
    renderContainerfile = "( " <> containerfileDhallPackage <> " ).render "

main :: IO ()
main = shakeMain $ do
  want ["install"]
  "build/Containerfile" %> \_ -> generateContainerFile
  "build/container" %> \_ -> buildContainer
  phony imageRef publishContainer
  phony "test" cabalTest
  phony "install" $ need ["test"] >> cabalInstallLib "lib:shake-factory shake"
  cabalDocs
  clean
