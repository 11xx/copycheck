module CopyCheck.OptParsing
  ( Opts(..)
  , customExecParser
  , prefs
  , showHelpOnEmpty
  , optsParser
  , optsParserInfo
  ) where

import Options.Applicative
import CopyCheck.Version (versionStr)

import Data.Kind (Type) -- for Opts data record `* -> *'

import System.FilePath.Posix

data Opts = Opts
  { optFilename :: FilePath
  , optCopyText :: String
  , optDir      :: FilePath
  , optPadNum   :: Int
  , optVersion  :: Type -> Type
  , optRmText   :: Maybe String
  }

optsParser :: Parser Opts
optsParser
  = Opts
  <$> (fileOptParser <|> fileOptParser')
  <*> copyTextOptParser
  <*> dirOptParser
  <*> padNumOptParser
  <*> versionOptParse
  <*> rmTextParse

  where
    fileOptParser
      = strArgument
      $ metavar "FILENAME"
      <> help "Input file"
      <> action "file"

    fileOptParser'
      = strOption
      $ long "file"
      <> short 'f'
      <> metavar "FILENAME"
      <> action "file"

    copyTextOptParser
      = strOption
      $ long "text"
      <> short 't'
      <> metavar "TEXT"
      <> value "_COPY_"
      <> help "Text to append to conflicting filename"

    dirOptParser
      = strOption
      $ long "dir"
      <> short 'd'
      <> metavar "PATH"
      <> value "."
      <> help "Directory to check if FILENAME exists"
      <> action "directory"

    padNumOptParser
      = option auto
      $ long "padding"
      <> short 'p'
      <> metavar "NUM"
      <> value 2
      <> help "Number of zeros to add to the incremental number after TEXT"
      <> completeWith (showNumList 1 (6 :: Int))

    rmTextParse
      = optional
      $ strOption
      $ long "rm-text"
      <> short 'r'
      <> metavar "REGEXP"
      <> help "Remove REGEXP from the filename, e.g.: \"-r 'C[a-z]+_[0-9]+'\", for matching strings like `Cooopppyyy_00044433221' within a filename."

showNumList :: Int -> Int -> [String]
showNumList x y = map show [x..y]

versionOptParse :: Parser (a -> a)
versionOptParse =
  infoOption versionStr
  $ long "version"
  <> short 'V'
  <> help "Display the version number"

optsParserInfo :: ParserInfo Opts
optsParserInfo = info (optsParser <**> helper)
  $ fullDesc
  <> progDesc "Check if input filename exists in a given directory and return it's renamed string if necessary."
  <> header "copycheck - Check if filename exists and return a renamed version if necessary."
