module CopyCheck.App
    ( copyCheck
    ) where

import CopyCheck.OptParsing
    ( optsParserInfo,
      showHelpOnEmpty,
      prefs,
      customExecParser,
      Opts(Opts) )

import System.Directory ( doesFileExist )

import System.FilePath.Posix
    ( (</>),
      hasExtension,
      takeBaseName,
      takeDirectory,
      takeExtension,
      takeFileName )

import Text.Regex.TDFA ( (=~) )

copyCheck :: Int -> IO ()
copyCheck n = do
  opts <- customExecParser p optsParserInfo
  r <- copyCheckRename opts n
  putStrLn r
    where
      p = prefs showHelpOnEmpty

copyCheckRename :: Opts -> Int -> IO String
copyCheckRename opts@(Opts fo t d p _ r) n = do
  let f = takeFileName fo
  let he = hasExt f
      ih = isHidden f
      dir = getDir fo d
      base = takeBaseName f
      file = takeFileName f
      ext = takeExtension f

  let rmText (Just r) = r
      rmText Nothing = defOld
      defOld = t <> "[0-9]*"

  let renamed nn
        | he = heR
        | ih = ihR
        | otherwise = heR
        where
          ct = copyText t p nn
          heR = replacedHeR
          ihR = replacedIhR
          replacedHeR = replace rt new base <> ct <> ext
          replacedIhR = replace rt new file <> ct
          new = []
          rt = rmText r

  e <- doesFileExist $  dir </> f
  re <- doesFileExist $ dir </> renamed n

  let checkExist
        | e = checkReExist
        | otherwise = pure (takeFileName f)
        where
          checkReExist
            | re = copyCheckRename opts (n + 1)
            | otherwise = pure $ renamed n

  checkExist

copyText :: String -> Int -> Int -> String
copyText t p n = t <> pad n p

hasExt :: FilePath -> Bool
hasExt f
  | beginsWithDot f && takeExtension f == takeFileName f = False
  | hasExtension f = True
  | otherwise = False

beginsWithDot :: FilePath -> Bool
beginsWithDot f
  | head (takeFileName f) == '.' = True
  | otherwise = False

isHidden :: FilePath -> Bool
isHidden f
  | beginsWithDot f && not (hasExt f) = True
  | otherwise = False

getDir :: FilePath -> FilePath -> FilePath
getDir f d
  | takeDirectory f == d = d
  -- | d == "." = takeDirectory f
  | otherwise = d

pad :: Int -> Int -> String
pad n p =
  replicate (p - length sn) zn <> sn
  where
    sn = show n
    zn = '0'

replace :: String -> String -> String -> String
replace old new input =
  case input =~ old :: (String, String, String) of
    (bef, _, aft) -> bef ++ new ++ aft

