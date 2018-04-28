
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Test where

import           Control.Exception
import           Control.Monad
import           Data.ByteString.Lazy
import           Data.Serialize
import           Data.Word
import           System.FilePath
import           Test.Properties
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Text.Read

import           FixedLengthCString
import           ReadSave
import           Timestamp

exampleIO = readSave "Test/SAVEGAME/SLOT06"

exampleIO' = tweak <$> exampleIO
  where
    tweak save = save { header = (header save)
        { characterName = "Slum"
        , saveName = "Edited save."
        , gameTime = toTimestamp 23364610
        } }

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
    defaultMain $ testGroup "* * *" [ testParser, testTimestamp, testFixedLengthCString ]

testParser = testGroup "Parser."

    [ testCase "Header of example is parsed as expected"
        $ exampleIO >>= \example -> header example @?= exampleHeader

    , goldenVsString "get /= undefined => put . get == id"
        ("Test/SAVEGAME/SLOT06" </> "SAVE.DAT") (fromStrict . runPut . put <$> exampleIO)

    , goldenVsString "Tweaked data serializes correctly"
        ("Test/SAVEGAME/SLOT07" </> "SAVE.DAT") (fromStrict . runPut . put <$> exampleIO')

    , testCase "Bogus file does not pass signature check."
        $ do
            x <- try $ readSave "Test/SAVEGAME/bogus"
            when (x /= Left signatureError) (assertFailure (show x))
    ]
  where
  signatureError = userError "Failed reading: Wrong signature:\
                   \ \"\\219e-\\248\\ETB\\t\\173\\DC2]\\128y\\218m\\FS\\227\\234\\EMB\"\n\
                   \Empty call stack\n"

testTimestamp = testGroup "Timestamp."

    [ testProperty "Word32 -> Timestamp is invertible"
        $ toTimestamp `partiallyIsomorphic` fromTimestamp

    , testProperty "read & show are an isomorphism."
        $ (show :: Timestamp -> String) `partiallyIsomorphic` readMaybe
    ]

testFixedLengthCString = testGroup "Fixed length strings."

    [ testProperty "Fixed length strings are ..."
        $ (show :: FixedLengthCString 10 -> String) `partiallyIsomorphic` readMaybe
    ]
