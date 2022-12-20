module Lib
    ( copyCheck
    ) where

import Options.Applicative
import System.Directory
import System.FilePath.Posix

copyCheck :: IO ()
copyCheck = do
  opts <- execParser optsParserInfo
  putStrLn =<< run opts 1

-- run :: (Show p, Num p) => Opts -> p -> IO [Char]
run :: Opts -> Int -> IO String
run opts@(Opts f t d p) n = do
  -- same as `run (Opts f t d p) n = do'
  let filename = takeFileName f
      filepath = d </> filename
      -- ren = filepath ++ t ++ pad n p
  doesExist <- doesFileExist filepath

  if doesExist
    then do doesExist' <- doesFileExist $ filepath ++ t ++ pad n p
            if doesExist'
              then run opts (n + 1)
              else pure $ filepath ++ t ++ pad n p
    else pure filename


pad :: Show p => p -> Int -> [Char]
pad n p = replicate (p - length sn) '0' <> sn
  where
    sn = show n

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

