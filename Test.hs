
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Test where

import           Data.ByteString.Lazy
import           Data.Serialize
import           Data.Word
import           System.FilePath
import           Test.Properties
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Timestamp

import           ReadSave              hiding (main)

exampleIO = readSave "SLOT06"

exampleIO' = tweak <$> exampleIO
  where
    tweak save = save { header = (header save) { characterName = "Conglomerate" } }

exampleHeader = Header
    { version       = (1,2)
    , characterName = "Bulgur"
    , saveName      = "Zero save."
    , saveTime      = "\NUL\ETB\NUL\EOT\a\226\NUL\NUL\NUL\RS"
    , gameTime      = toTimestamp 264606
    , mapLevel      = 0
    , mapNumber     = 35
    , mapName       = "V13ENT.sav"
    , allHeader     = ""
    }

main = do
    example <- exampleIO
    example' <- exampleIO'
    defaultMain $ testGroup "Testing ReadSave."
        [ testGroup "Parser."
            [ testCase "Header of example is parsed as expected"
                $ header example @?= exampleHeader
            , goldenVsString "get /= undefined => put . get == id"
                ("SLOT06" </> "SAVE.DAT") (fromStrict . runPut . put <$> exampleIO)
            , goldenVsString "Tweaked data serializes correctly"
                ("SLOT07" </> "SAVE.DAT") (fromStrict . runPut . put <$> exampleIO')
            ]
        , testGroup "Date."
            [ testProperty "Word32 -> Timestamp is invertible"
                $ fmap toTimestamp `inverts` (>>= fromTimestamp)
            ]
        ]

