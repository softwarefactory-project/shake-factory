-- | Shake functions to deploy application on OpenShift
module ShakeFactory.OpenShift
  ( deployResources,
  )
where

import Development.Shake

deployResources :: FilePath -> Action ()
deployResources fp = do
  need [fp]
  cmd_ "oc apply -f" fp
