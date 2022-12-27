module CopyCheck.App
    ( copyCheck
    ) where

import CopyCheck.OptParsing
       -- ( execParser, Opts(Opts), optsParserInfo )
import System.Directory ( doesFileExist )
import System.FilePath.Posix
    ( takeFileName,
      takeBaseName,
      takeExtension,
      (</>),
      hasExtension,
      takeDirectory )
import Text.Regex.TDFA ( (=~) )

copyCheck :: (Num p, Show p) => p -> IO ()
copyCheck n = do
  opts <- customExecParser p optsParserInfo
  r <- copyCheckRename opts n
  putStrLn r
    where
      p = prefs showHelpOnEmpty

copyCheckRename :: (Num p, Show p) => Opts -> p -> IO [Char]
copyCheckRename opts@(Opts f t d p _) n = do
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

  e  <- doesFileExist $ dir </> f
  re <- doesFileExist $ dir </> renamed n

  let checkExist
        | e = checkReExist
        | otherwise = pure f
        where
          checkReExist
            | re = copyCheckRename opts (n + 1)
            | otherwise = pure $ renamed n

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

