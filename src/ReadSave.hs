
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ReadSave where

-- falloutmods.wikia.com/wiki/SAVE.DAT_File_Format

import           Control.Applicative
import           Control.Exception
import           Control.Monad              (guard, void, when)
import           Data.ByteString            hiding (head, length, readFile)
import qualified Data.ByteString            as ByteString
import           Data.ByteString.Builder
import           Data.ByteString.Lazy       (toStrict)
import           Data.Maybe
import           Data.Semigroup
import           Data.Serialize
import           Data.Time.Calendar
import           Data.Word
import           GHC.TypeLits
import           Language.Haskell.TH.Syntax
import           Lens.Micro.Platform
import           System.Environment         (getArgs, getEnv)
import           System.FilePath

import           FixedLengthCString
import           Timestamp

replace :: ByteString -> Int -> ByteString -> ByteString
replace source offset replacement = prefix <> replacement <> suffix
  where
    prefix = ByteString.take offset source
    suffix = ByteString.drop (offset + ByteString.length replacement) source

fix :: Int -> ByteString -> ByteString -> ByteString
fix offset replacement source = replace source offset replacement

fix' :: KnownNat n => Int -> FixedLengthCString n -> ByteString -> ByteString
fix' offset replacement source = replace source offset (runPut . put $ replacement)

data Save = Save
    { header  :: Header
    , gVars   :: ByteString
    , maps    :: [ByteString]
    , allSave :: ByteString
    } deriving (Show, Eq)

data Header = Header
    { version       :: (Word16, Word16)
    , characterName :: FixedLengthCString 0x20
    , saveName      :: FixedLengthCString 0x1e
    , saveTime      :: ByteString
    , gameTime      :: Timestamp
    , mapLevel      :: Word16
    , mapNumber     :: Word16
    , mapName       :: FixedLengthCString 0x10
    , allHeader     :: ByteString
    } deriving (Show, Eq)

flip makeLensesWith ''Header $ lensRules & lensField .~ \_ _ x -> [TopName . mkName $ '_': nameBase x]

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

instance Serialize Header where

    get = label "header" . isolate 0x7563 $ do
        checkSignature
        allHeader <- lookAhead $ getBytes 0x7563
        skip 0x0018
        versionMajor  <- getWord16be
        versionMinor  <- getWord16be
        let version = (fromIntegral versionMajor, fromIntegral versionMinor)
        skip 0x0001
        characterName <- get
        saveName      <- get
        saveTime      <- getBytes 0x0a
        gameMonth     <- fromIntegral <$> getWord16be
        gameDay       <- fromIntegral <$> getWord16be
        gameYear      <- fromIntegral <$> getWord16be
        let gameDate = fromMaybe ( error $ "Header.get: In-game date is not valid." )
                                 $ fromGregorianValid gameYear gameMonth gameDay
        gameTime      <- toTimestamp <$> getWord32be
        mapLevel      <- getWord16be
        mapNumber     <- getWord16be
        mapName       <- get
        pic           <- getBytes 0x7460
        skip 0x0080
        return $ Header { characterName, saveName, saveTime, mapLevel, mapNumber, mapName, version
            , gameTime, allHeader = "" }

    put Header{..} = putByteString
                   $ fix 0x18 (runPut $ putWord16be (fst version) >> putWord16be (snd version))
                   . fix' 0x1d characterName
                   . fix' 0x3d saveName
                   . fix 0x6b (runPut $ putWord32be . fromMaybe (error "Invalid timestamp!")
                                      . fromTimestamp $ gameTime)
                   . fix 0x6f (runPut $ putWord16be mapLevel)
                   . fix 0x71 (runPut $ putWord16be mapNumber)
                   . fix' 0x73 mapName
                   $ allHeader

checkSignature :: Get ()
checkSignature = do
    signature <- lookAhead (getBytes 0x12)
    when (signature /= "FALLOUT SAVE FILE\NUL") (fail $ "Wrong signature: " ++ show signature)

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

readSave :: FilePath -> IO Save
readSave p = ByteString.readFile (p </> "SAVE.DAT") >>= either fail return . runGet get

writeSave save p = ByteString.writeFile (p </> "SAVE.DAT") (runPut . put $ save)
