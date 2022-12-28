module CopyCheck.App
    ( copyCheck
    ) where

import CopyCheck.OptParsing
    ( optsParserInfo,
      showHelpOnEmpty,
      prefs,
      customExecParser,
      Opts(Opts) )

import RawFilePath ( RawFilePath, doesFileExist )

import System.FilePath.Posix.ByteString
    ( (</>),
      hasExtension,
      takeBaseName,
      takeDirectory,
      takeExtension,
      takeFileName )

import Text.Regex.TDFA ( (=~) )

import qualified Data.ByteString.UTF8 as BB (fromString)
import qualified Data.ByteString.Char8 as C

copyCheck :: (Num p, Show p) => p -> IO ()
copyCheck n = do
  opts <- customExecParser p optsParserInfo
  r <- copyCheckRename opts n
  C.putStrLn $ BB.fromString (C.unpack r) -- from D.BS.Char8 to D.BS.UTF8
    where
      p = prefs showHelpOnEmpty

copyCheckRename :: (Num p, Show p) => Opts -> p -> IO C.ByteString
copyCheckRename opts@(Opts fo t d p _) n = do
  let f = takeFileName fo
  let he = hasExt f
      ih = isHidden f
      dir = getDir fo d
      base = takeBaseName f
      file = takeFileName f
      ext = takeExtension f

  let renamed nn
        | he = heR
        | ih = ihR
        | otherwise = heR
        where
          ct = copyText t p nn
          heR = replacedHeR
          ihR = replacedIhR
          replacedHeR = replace old new base <> ct <> ext
          replacedIhR = replace old new file <> ct
          old = t <> C.pack "[0-9]*"
          new = C.empty

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

copyText :: Show p => C.ByteString -> Int -> p -> C.ByteString
copyText t p n = t <> pad n p

hasExt :: RawFilePath -> Bool
hasExt f
  | begginsWithDot f && takeExtension f == takeFileName f = False
  | hasExtension f = True
  | otherwise = False

begginsWithDot :: RawFilePath -> Bool
begginsWithDot f
  | C.take 1 (takeFileName f) == C.pack "." = True
  | otherwise = False

isHidden :: RawFilePath -> Bool
isHidden f
  | begginsWithDot f && not (hasExt f) = True
  | otherwise = False

getDir :: RawFilePath -> RawFilePath -> RawFilePath
getDir f d
  | takeDirectory f == d = d
  | d == C.pack "." = takeDirectory f
  | otherwise = d

pad :: Show p => p -> Int -> C.ByteString
pad n p =
  C.replicate (p - C.length sn) zn <> sn
  where
    sn = C.pack (show n)
    zn = '0'

replace :: C.ByteString -> C.ByteString -> C.ByteString -> C.ByteString
replace old new input =
  case input =~ old :: (C.ByteString, C.ByteString, C.ByteString) of
    (bef, _, aft) -> p $ b ++ n ++ a
      where
        p = C.pack
        u = C.unpack
        b = u bef
        n = u new
        a = u aft
