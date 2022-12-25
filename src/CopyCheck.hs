module CopyCheck
    ( copyCheck
    ) where

import Options.Applicative
    ( header,
      progDesc,
      fullDesc,
      helper,
      (<**>),
      info,
      auto,
      option,
      value,
      short,
      long,
      strOption,
      help,
      metavar,
      strArgument,
      execParser,
      Alternative((<|>)),
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
copyCheckRename opts@(Opts fo t d p) n = do
  let f | not (null fo) = fo
        | otherwise = error "ERROR: No filename provided."
        -- #TODO better error handling

  let he = hasExt f
      ih = isHidden f
      dir = getDir f d

  let renamed nn
        | he = heR
        | ih = ihR
        | otherwise = heR
        where
          ct = copyText t p nn
          old = t ++ "[0-9]*"
          heR = replace old "" (takeBaseName f) ++ ct ++ takeExtension f
          ihR = replace old "" (takeFileName f) ++ ct

  e <- doesFileExist $ dir </> f
  re <- doesFileExist $ dir </> renamed n

  let checkExist
        | e = do
            let checkReExist
                  | re = copyCheckRename opts (n + 1)
                  | otherwise = pure $ renamed n
            checkReExist
        | otherwise = pure f

  checkExist

copyText :: Show p => [Char] -> Int -> p -> [Char]
copyText t p n = t ++ pad n p


hasExt :: FilePath -> Bool
hasExt f
  | head (takeFileName f) == '.' && takeExtension f == takeFileName f = False
  | hasExtension f = True
  | otherwise = False

isHidden :: FilePath -> Bool
isHidden f
  | head (takeFileName f) == '.' && not (hasExt f) = True
  | otherwise = False

getDir :: FilePath -> FilePath -> FilePath
getDir f d
  | takeDirectory f == d = d
  | d == "." = takeDirectory f
  | otherwise = d

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
  <$> (fileParser <|> fileOptParser)
  <*> copyTextParser
  <*> dirParser
  <*> padNumParser

  where
    fileParser
      = strArgument
      $ metavar "FILENAME"
      <> help "Input file"

    fileOptParser
      = strOption
      $ long "file"
      <> short 'f'
      <> metavar "FILENAME"

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

