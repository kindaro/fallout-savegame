#!/usr/bin/env stack
{- stack runghc
    --resolver lts-11.5
    --package filepath
    --package bytestring
    --package cereal
    --package placeholders
-}

{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  , NamedFieldPuns
  , TypeSynonymInstances
  , FlexibleInstances
  #-}

module ReadSave where

-- falloutmods.wikia.com/wiki/SAVE.DAT_File_Format

import Control.Applicative
import Control.Exception
import Control.Monad (void, guard)
import Data.ByteString hiding (head, readFile, length)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder
import qualified Data.ByteString as ByteString
import Data.Serialize
import Development.Placeholders
import System.FilePath
import System.Environment (getArgs, getEnv)

dropTrailingZeroes :: ByteString -> ByteString
dropTrailingZeroes = ByteString.takeWhile (/= 0)

data Save = Save
    { header :: Header
    , gVars  :: ByteString
    , maps   :: [ByteString]
    , everything :: ByteString
    } deriving Show

instance Serialize Save where
    get = do
        everything <- lookAhead $ getBytes =<< remaining
        checkSignature
        header <- get :: Get Header
        gVars <- getGVars
        maps <- getMaps
        skip =<< remaining
        return Save { header, gVars, maps, everything }

    put = $(placeholder "put :: FalloutSave -> ByteString is not yet defined.")

data Header = Header
    { version :: (Int, Int)
    , characterName :: ByteString
    , saveName      :: ByteString
    , saveTime      :: ByteString
    , gameTime    :: Int
    , mapName :: ByteString
    } deriving Show

instance Serialize Header where

    get = label "header" . isolate 0x7563 $ do
        checkSignature
        skip 0x0018
        versionMajor  <- getWord16be
        versionMinor  <- getWord16be
        let version = (fromIntegral versionMajor, fromIntegral versionMinor)
        skip 0x0001
        characterName <- dropTrailingZeroes <$> getBytes 0x0020
        saveName      <- dropTrailingZeroes <$> getBytes 0x001e
        saveTime      <- getBytes 0x000a
        someTime      <- getBytes 0x0006
        gameTime      <- fromIntegral <$> getWord32be
        mapNumber     <- getWord32be
        mapName       <- dropTrailingZeroes <$> getBytes 0x0010
        pic           <- getBytes 0x7460
        skip 0x0080
        return $ Header { characterName, saveName, saveTime, mapName, version, gameTime }

    put = undefined

checkSignature :: Get ()
checkSignature = do
    signature <- lookAhead (getBytes 0x12)
    if signature /= "FALLOUT SAVE FILE\NUL"
    then fail $ "Wrong signature: " ++ show signature
    else return ()

function1 :: Get ()
function1 = void getWord32be

getGVars = getBytes 0x9d9

getNonZeroWord8 = do
    word8 <- getWord8
    if word8 == 0
    then fail "getNonZeroWord8: Stumbled upon a word8 == 0."
    else return word8

getFromCString :: Get ByteString
getFromCString = do
    s <- many getNonZeroWord8
    skip 1
    return $ pack s

getMaps :: Get [ByteString]
getMaps = label "maps" $ do
    mapsNumber <- getWord32be
    maps <- many $ do
        s <- getFromCString
        guard (s /= mempty)
        return s
    guard (length maps == fromIntegral mapsNumber)
    return maps

main :: IO ()
main = do
    [arg] <- getArgs
    save <- readSave arg
    print $ header save

readSave :: FilePath -> IO Save
readSave p = ByteString.readFile (p </> "SAVE.DAT") >>= either fail return . runGet get
