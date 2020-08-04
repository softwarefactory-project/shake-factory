-- |
-- Copyright: (c) 2020 Red Hat
-- SPDX-License-Identifier: Apache-2.0
-- Maintainer: Software Factory Dev <softwarefactory-dev@redhat.com>
--
-- See README for more info
module ShakeFactory
  ( someFunc,
  )
where

someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
