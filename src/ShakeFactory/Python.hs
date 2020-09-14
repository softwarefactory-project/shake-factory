-- | Shake functions for python project
module ShakeFactory.Python
  ( -- * Actions
    pythonDocAction,
  )
where

import Control.Monad (unless)
import Development.Shake

pythonDocAction :: String -> Action ()
pythonDocAction project =
  do
    pdoc3 <- ensurePdoc3
    args <- getCmdFromTox
    putInfo $ "args: " <> args
    cmd_ "rm -Rf build/docs"
    cmd_ pdoc3 args
    cmd_ "mv" ("build/html/" <> project) "build/docs"
  where
    -- Returns the pdoc3 args from the tox.ini file
    getCmdFromTox = do
      tox <- readFileLines "tox.ini"
      let toxs = map words tox
      let pdoccmd = [x | x@("commands" : "=" : "pdoc3" : _) <- toxs]
      let args = map (dropWhile (/= "pdoc3")) pdoccmd
      pure (head (map (unwords . drop 1) args))
    -- Ensures pdoc3 is installed and returns its path
    ensurePdoc3 = do
      home <- getEnvWithDefault "/home/user" "HOME"
      let pdoc3 = home <> "/.local/bin/pdoc3"
      pdoc3Installed <- doesFileExist pdoc3
      unless pdoc3Installed (cmd_ "pip3 install --user pdoc3")
      pure pdoc3
