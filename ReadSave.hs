#!/usr/bin/env stack
{- stack runghc
    --resolver lts-11.5
    --package path
    --package bytestring
    --package binary-strict
    --package placeholders
-}

{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  #-}

module ReadSave where

import Data.Binary
import Development.Placeholders
import Data.ByteString hiding (head, readFile)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder
import qualified Data.ByteString as ByteString
import Path
import System.Environment (getArgs)

data FalloutSave = FalloutSave
    { characterName :: ByteString
    , saveName      :: ByteString
    , saveTime      :: ByteString
    , inGameTime    :: ByteString
    , otherFiles    :: ByteString
    , unknown1      :: ByteString
    , everything    :: ByteString
    }

instance Binary FalloutSave where
    get = $(todo "do!")
    put = $(placeholder "put :: FalloutSave -> ByteString is not yet defined.")

main :: IO ()
main = head <$> getArgs >>= parseAbsFile >>= fmap parseRaw . readRaw >>= print

-- % ./ReadSave.hs ~/.wine/fallout/drive_c/Fallout/data/SAVEGAME/SLOT01/SAVE.DAT
-- "03666500"

readRaw :: Path Abs File -> IO ByteString
readRaw = ByteString.readFile . fromAbsFile

parseRaw :: ByteString -> ByteString
parseRaw = toStrict . toLazyByteString . byteStringHex
         . ByteString.take 4 . ByteString.drop 2
         . snd . breakSubstring "\0\0" . ByteString.drop 0x00007f40


