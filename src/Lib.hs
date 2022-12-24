module Lib
    ( copyCheck
    ) where

import Data.List (isInfixOf)
import Options.Applicative
    ( execParser,
      strArgument,
      metavar,
      help,
      strOption,
      long,
      short,
      value,
      option,
      auto,
      info,
      (<**>),
      helper,
      fullDesc,
      progDesc,
      header,
      Parser,
      ParserInfo )
import System.Directory ( doesFileExist )
import System.FilePath.Posix
    ( takeFileName,
      takeBaseName,
      takeExtension,
      (</>),
      hasExtension,
      takeDirectory )
import Text.Regex.Posix ( (=~) )

copyCheck :: (Num p, Show p) => p -> IO ()
copyCheck n = do
  opts <- execParser optsParserInfo
  r <- copyCheckRename opts n
  putStrLn r

copyCheckRename :: (Num p, Show p) => Opts -> p -> IO [Char]
copyCheckRename opts@(Opts f t _ _) n = do
  let he = hasExt opts
      ih = isHidden opts
      dir = getDir opts

  let renamed nn
        | he = heR
        | ih = ihR
        | otherwise = heR
        where
          ct = copyText opts nn
          old = t ++ "[0-9]*"
          heR = replace old "" (takeBaseName f) ++ ct ++ takeExtension f
          ihR = replace old "" (takeFileName f) ++ ct

  let renamedPath = dir </> renamed n

  e <- doesFileExist f
  re <- doesFileExist renamedPath

  let checkExist
        | e = do
            let checkReExist
                  | re = copyCheckRename opts (n + 1)
                  | otherwise = pure $ renamed n
            checkReExist
        | otherwise = pure f

  checkExist

-- unused
hasCopyText :: Show p => Opts -> p -> Bool
hasCopyText opts@(Opts f _ _ _) n
  | copyText opts n `isInfixOf` takeFileName f = True
  | otherwise = False

copyText :: Show p => Opts -> p -> [Char]
copyText (Opts _ t _ p) n = t ++ pad n p


hasExt :: Opts -> Bool
hasExt (Opts f _ _ _)
  | head (takeFileName f) == '.' && takeExtension f == takeFileName f = False
  | hasExtension f = True
  | otherwise = False

isHidden :: Opts -> Bool
isHidden opts@(Opts f _ _ _)
  | head (takeFileName f) == '.' && not (hasExt opts) = True
  | otherwise = False

-- same as getPath but without filename
getDir :: Opts -> FilePath
getDir (Opts f _ d _)
  | takeDirectory f == d = d
  | d == "." && takeDirectory f /= d = takeDirectory f
  | otherwise = d

-- unused
getPath :: Opts -> FilePath
getPath (Opts f _ d _)
  | takeDirectory f == d = d </> takeFileName f
  | d == "." && takeDirectory f /= d = takeDirectory f </> takeFileName f
  | otherwise = d </> takeFileName f

pad :: Show p => p -> Int -> [Char]
pad n p = replicate (p - length sn) '0' <> sn
  where
    sn = show n

-- snippet by chatGPT
replace :: String -> String -> String -> String
replace old new input =
  case input =~ old :: (String, String, String) of
    (before', _, after') -> before' ++ new ++ after'

data Opts = Opts
  { optFilename :: FilePath
  , optCopyText :: String
  , optDir      :: FilePath
  , optPadNum   :: Int
  }

optsParser :: Parser Opts
optsParser
  = Opts
  <$> fileParser
  <*> copyTextParser
  <*> dirParser
  <*> padNumParser
  where
    fileParser
      = strArgument
      $ metavar "FILENAME"
      <> help "Input file"

    copyTextParser
      = strOption
      $ long "text"
      <> short 't'
      <> metavar "TEXT"
      <> value "_COPY_"
      <> help "Text to append to conflicting filename"

    dirParser
      = strOption
      $ long "dir"
      <> short 'd'
      <> metavar "PATH"
      <> value "."
      <> help "Directory to check if FILENAME exists"

    padNumParser
      = option auto
      $ long "padding"
      <> short 'p'
      <> metavar "NUM"
      <> value 2
      <> help "(TODO) number of zeros to add to the incremental number after TEXT"

optsParserInfo :: ParserInfo Opts
optsParserInfo = info (optsParser <**> helper)
  $ fullDesc
  <> progDesc "Check if input filename exists in a given direcory and return it's renamed string if necessary."
  <> header "copycheck - Check if filename exists and return a renamed version if necessary."

