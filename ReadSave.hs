#!/usr/bin/env stack
{- stack runghc
    --resolver lts-11.5
    --package path
    --package bytestring
    --package cereal
    --package placeholders
-}

{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  #-}

module ReadSave where

import Data.ByteString hiding (head, readFile)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder
import qualified Data.ByteString as ByteString
import Data.Serialize
import Development.Placeholders
import Path
import System.Environment (getArgs)

data Save = Save
    { characterName :: ByteString
    , saveName      :: ByteString
    , saveTime      :: ByteString
    , inGameTime    :: ByteString
    , otherFiles    :: ByteString
    , unknown1      :: ByteString
    , everything    :: ByteString
    }

instance Serialize Save where
    get = $(todo "do!")
    put = $(placeholder "put :: FalloutSave -> ByteString is not yet defined.")

main :: IO ()
main = do
    [arg] <- getArgs
    p <- parseAbsFile arg
    raw <- readRaw p
    either (ioError . userError) return . runGet checkSignature $ raw

-- % ./ReadSave.hs ~/.wine/fallout/drive_c/Fallout/data/SAVEGAME/SLOT01/SAVE.DAT
-- "03666500"

readRaw :: Path Abs File -> IO ByteString
readRaw = ByteString.readFile . fromAbsFile

checkSignature :: Get ()
checkSignature = do
    signature <- lookAhead (getBytes 0x12)
    if signature /= "FALLOUT SAVE FILE\NUL"
    then fail $ "Wrong signature: " ++ show signature
    else return ()
