import Data.Maybe (fromMaybe)
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import ShakeFactory

main :: IO ()
main = shakeMain $ do
  want ["install"]
  phony "test" cabalTest
  phony "install" $ need ["test"] >> cabalInstallLib "lib:shake-factory shake"
  cabalDocs
  clean
