-- | Shake functions to deploy application on OpenShift
module ShakeFactory.OpenShift
  ( -- * Actions
    deployResources,

    -- * Re-exports
    dhallYaml,
    (%>),
  )
where

import Development.Shake
import ShakeFactory.Dhall

deployResources :: FilePath -> Action ()
deployResources fp = do
  need [fp]
  cmd_ "oc apply -f" fp
