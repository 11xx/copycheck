module CopyCheck.Version (versionStr) where

import Data.Version (showVersion)

import Paths_copycheck (version) -- generated by Cabal

progName :: [Char]
progName = "copycheck"

versionStr :: [Char]
versionStr = progName ++ ", " ++ (showVersion version)