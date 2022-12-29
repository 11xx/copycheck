module Main (main) where

import Test.Hspec
import CopyCheck.App (copyCheck)
import System.Environment

main :: IO ()
main = hspec $ do
  describe "copyCheck" $ do
    it "Hidden file WITHOUT extension" $
      withArgs ["-f", f1] $ copyCheck 1
    it "Hidden file WITHOUT extension_COPY_01" $
      withArgs ["-f", f2] $ copyCheck 1
    it "Hidden file WITHOUT extension_COPY_02" $
      withArgs ["-f", f3] $ copyCheck 1
    it "Hidden file WITHOUT extension_COPY_03" $
      withArgs ["-f", f4] $ copyCheck 1
    it "Hidden file WITH extension" $
      withArgs ["-f", f5] $ copyCheck 1
    it "Hidden file WITH extension_COPY_01" $
      withArgs ["-f", f6] $ copyCheck 1
    it "Hidden file WITH extension_COPY_02" $
      withArgs ["-f", f7] $ copyCheck 1
    it "Hidden file WITH extension_COPY_03" $
      withArgs ["-f", f8] $ copyCheck 1
    it "Regular file WITHOUT extension" $
      withArgs ["-f", f9] $ copyCheck 1
    it "Regular file WITHOUT extension_COPY_01" $
      withArgs ["-f", f10] $ copyCheck 1
    it "Regular file WITHOUT extension_COPY_02" $
      withArgs ["-f", f11] $ copyCheck 1
    it "Regular file WITHOUT extension_COPY_03" $
      withArgs ["-f", f12] $ copyCheck 1
    it "Regular file WITH extension" $
      withArgs ["-f", f13] $ copyCheck 1
    it "Regular file WITH extension_COPY_01" $
      withArgs ["-f", f14] $ copyCheck 1
    it "Regular file WITH extension_COPY_02" $
      withArgs ["-f", f15] $ copyCheck 1
    it "Regular file WITH extension_COPY_03" $
      withArgs ["-f", f16] $ copyCheck 1
    it "Regular file with special characters, testing the UTF8 encoding" $
      withArgs ["-f", f17] $ copyCheck 1

      where
        d = "test/.test-cases"
        ct n = " & copy text and number " ++ show 0 ++ show n
        f1 = d ++ "/" ++ ".hid_nodot"
        f2 = d ++ "/" ++ ".hid_nodot_COPY_01"
        f3 = d ++ "/" ++ ".hid_nodot_COPY_02"
        f4 = d ++ "/" ++ ".hid_nodot_COPY_03"
        f5 = d ++ "/" ++ ".hiddot.ext"
        f6 = d ++ "/" ++ ".hiddot_COPY_01.ext"
        f7 = d ++ "/" ++ ".hiddot_COPY_02.ext"
        f8 = d ++ "/" ++ ".hiddot_COPY_03.ext"
        f9 = d ++ "/" ++ "file_nodot"
        f10 = d ++ "/" ++ "file_nodot_COPY_01"
        f11 = d ++ "/" ++ "file_nodot_COPY_02"
        f12 = d ++ "/" ++ "file_nodot_COPY_03"
        f13 = d ++ "/" ++ "filedot.ext"
        f14 = d ++ "/" ++ "filedot_COPY_01.ext"
        f15 = d ++ "/" ++ "filedot_COPY_02.ext"
        f16 = d ++ "/" ++ "filedot_COPY_03.ext"
        f17 = d ++ "/" ++ "filename_specialçãõäüå.ext"

      -- pending
    -- it "appends a copy number to the file name if it already exists" $ do
    --   withArgs ["-f", "~/Code/useless-utils/copycheck/test/MyLibTest.hs"] $ copyCheck 1
    --   pending
