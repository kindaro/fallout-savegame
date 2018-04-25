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
  , RecordWildCards
  #-}

module ReadSave where

-- falloutmods.wikia.com/wiki/SAVE.DAT_File_Format

import Control.Applicative
import Control.Exception
import Control.Monad (void, guard)
import Data.Word
import Data.ByteString hiding (head, readFile, length)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder
import qualified Data.ByteString as ByteString
import Data.Serialize
import Data.Semigroup
import Development.Placeholders
import System.FilePath
import System.Environment (getArgs, getEnv)

dropTrailingZeroes :: ByteString -> ByteString
dropTrailingZeroes = fst . spanEnd (== 0)

replace :: ByteString -> Int -> ByteString -> ByteString
replace source offset replacement = prefix <> replacement <> suffix
  where
    prefix = ByteString.take offset source 
    suffix = ByteString.drop (offset + ByteString.length replacement) source

fix :: Int -> ByteString -> ByteString -> ByteString
fix offset replacement source = replace source offset replacement

data Save = Save
    { header :: Header
    , gVars  :: ByteString
    , maps   :: [ByteString]
    , allSave :: ByteString
    } deriving Show

instance Serialize Save where
    get = do
        allSave <- lookAhead $ getBytes =<< remaining
        checkSignature
        header <- get :: Get Header
        gVars <- getGVars
        maps <- getMaps
        skip =<< remaining
        return Save { header, gVars, maps, allSave }

    put Save{..} = putByteString
                 $ replace allSave 0x0 (runPut $ put fixedHeader)
      where
        fixedHeader = header { allHeader = ByteString.take 0x7563 allSave }

data Header = Header
    { version :: (Word16, Word16)
    , characterName :: ByteString
    , saveName      :: ByteString
    , saveTime      :: ByteString
    , gameTime    :: Word32
    , mapNumber :: Word32
    , mapName :: ByteString
    , allHeader :: ByteString
    } deriving Show

instance Serialize Header where

    get = label "header" . isolate 0x7563 $ do
        checkSignature
        allHeader <- lookAhead $ getBytes 0x7563
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
        return $ Header { characterName, saveName, saveTime, mapNumber, mapName, version
            , gameTime, allHeader = "" }

    put Header{..} = putByteString
                   $ fix 0x18 (runPut $ putWord16be (fst version) >> putWord16be (snd version))
                   . fix 0x1d characterName
                   . fix 0x3d saveName
                   . fix 0x6b (runPut $ putWord32be gameTime)
                   . fix 0x6f (runPut $ putWord32be mapNumber)
                   . fix 0x73 mapName
                   $ allHeader

checkSignature :: Get ()
checkSignature = do
    signature <- lookAhead (getBytes 0x12)
    if signature /= "FALLOUT SAVE FILE\NUL"
    then fail $ "Wrong signature: " ++ show signature
    else return ()

function1 :: Get ()
function1 = void getWord32be

getGVars = getBytes 0x9d9  -- I wonder why it's odd. It should be an array of Word32, thus even.

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
