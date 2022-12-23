module Lib
    ( copyCheck
    ) where

import Data.List (isInfixOf)
-- import Data.List.Split (splitOn)
import Options.Applicative
import System.Directory
import System.FilePath.Posix
import Text.Regex.Posix

copyCheck :: IO ()
copyCheck = do
  opts <- execParser optsParserInfo
  f <- run opts 1
  putStrLn f

run opts@(Opts f t _ _) n = do
  let he = hasExt opts
      ih = isHidden opts
      hct = hasCopyText opts n
      dir = getDir opts

  -- trace ("he" ++ heR)
  let renamed nn
        | he = heR
        | ih = ihR
        | otherwise = heR
        where
          ct = copyText opts nn
          old = t ++ "[0-9]*"
          heR = replace old "" (takeBaseName f) ++ ct ++ takeExtension f
          ihR = replace old "" (takeFileName f) ++ ct

  -- debug putStrLn
  -- putStrLn $ "cpyText: " ++ copyText opts n
  -- putStrLn $ "replace: " ++ replace (t ++ "[0-9]*") "" (takeBaseName f)
  -- putStrLn $ "renamedN: " ++ renamed n
  let renamedPath = dir </> renamed n
  -- putStrLn $ "renPath:" ++ renamedPath

  -- let renamedPathInc = dir </> renamed (n + 1)
  -- putStrLn $ "renPathInc:" ++ renamedPathInc

  re <- doesFileExist renamedPath
  -- rei <- doesFileExist renamedPathInc
  -- | re && rei = run opts (n + 3)
  let run'
        | re = run opts (n + 1)
        | otherwise = pure $ renamed n

  run'

hasCopyText opts@(Opts f _ _ _) n
  | copyText opts n `isInfixOf` takeFileName f = True
  | otherwise = False

copyText (Opts _ t _ p) n = t ++ pad n p


hasExt :: Opts -> Bool
hasExt (Opts f _ _ _)
  | head (takeFileName f) == '.' && takeExtension f == takeFileName f = False
  | hasExtension f = True
  | otherwise = False

isHidden opts@(Opts f _ _ _)
  | head (takeFileName f) == '.' && not (hasExt opts) = True
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

-- replace' old new input =
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

