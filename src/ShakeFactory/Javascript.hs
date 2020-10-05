{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Shake functions for javascript project
module ShakeFactory.Javascript
  ( -- * Actions
    writePackage,
  )
where

import Development.Shake

-- TODO
-- import Dhall.TH

-- Dhall.TH.makeHaskellTypes
--   [ SingleConstructor "NpmDependency" "MakeNpmDependency" "~/src/softwarefactory-project.io/software-factory/dhall-npm-package/NpmPackage/Dependency.dhall",
--     SingleConstructor "NpmPackage" "MakeNpmPackage" "~/src/softwarefactory-project.io/software-factory/dhall-npm-package/NpmPackage/Type.dhall"
--   ]

type NpmPackage = String

writePackage :: NpmPackage -> Action ()
writePackage _package = putInfo "todo..."
