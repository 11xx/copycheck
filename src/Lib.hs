module Lib
    ( copyCheck
    ) where

import Options.Applicative
import System.Directory
import System.FilePath.Posix

copyCheck :: IO ()
copyCheck = do
  opts <- execParser optsParserInfo
  f <- run opts 1
  putStrLn f

-- run :: (Show p, Num p) => Opts -> p -> IO [Char]
run :: Opts -> Int -> IO String
run opts@(Opts f t d p) n = do
  let he = hasExt opts
      ih = isHidden opts
      hc = hasCopyText opts n
      dir = getDir opts

  let isHiddenNoExt
        | he && ih = False
        | otherwise = True

  let ren = replace fa ta tb
        where
          fa = takeFileName f
          ta = copyText opts n
          tb = copyText opts $ n + 1

  reb <- doesFileExist ren

  let renamed n'
        | he           = replace ct "" (takeBaseName f) ++ ct ++ takeExtension f
        | ih && not he = replace ct "" (takeFileName f) ++ ct
        | otherwise    = replace ct "" (takeBaseName f) ++ ct ++ takeExtension f
        where
          ct = copyText opts n'

  let renamedPath = getDir opts </> renamed n

  renamedE <- doesFileExist renamedPath

  recur <- if hasCopyText opts n && reb || renamedE
           then run opts (n + 1)
           else pure ""

  let doit
        | n > 1000 = error "ERROR: Maximum recursion depth reached."
        | hasCopyText opts n = if reb
                               then recur
                               else ren
        | renamedE = recur
        | otherwise = renamed n

  pure doit

hasCopyText opts@(Opts f _ _ _) n
  | copyText opts n `isInfixOf` takeFileName f = True
  | otherwise = False

copyText (Opts _ t _ p) n = t ++ pad n p


hasExt :: Opts -> Bool
hasExt (Opts f _ _ _)
  | head (takeFileName f) == '.' && takeExtension f == takeFileName f = False
  | hasExtension f = True
  | otherwise = False

isHidden (Opts f _ _ _)
  | head (takeFileName f) == '.' = True
  | otherwise = False

-- same as getPath but without filename
getDir (Opts f _ d _)
  | takeDirectory f == d = d
  | d == "." && takeDirectory f /= d = takeDirectory f
  | otherwise = d

getPath :: Opts -> FilePath
getPath (Opts f _ d _)
  | takeDirectory f == d = d </> takeFileName f
  | d == "." && takeDirectory f /= d = takeDirectory f </> takeFileName f
  | otherwise = d </> takeFileName f

-- rename (Opts f t d p) n
--   | t ++ pad n d `isInfixOf` takeFileName f =

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

