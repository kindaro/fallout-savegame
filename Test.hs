#!/usr/bin/env stack
{- stack runghc
    --resolver lts-11.5
    --package filepath
    --package bytestring
    --package cereal
    --package placeholders
    --package type-iso
    --package time
    --package tasty
    --package tasty-quickcheck
    --package tasty-hunit
    --package tasty-golden
-}

{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module Test where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.Golden
import Data.ByteString.Lazy
import System.FilePath
import Data.Serialize

import ReadSave hiding (main)

exampleIO = readSave "SLOT06"

exampleIO' = tweak <$> exampleIO
  where
    tweak save = save { header = (header save) { characterName = "Conglomerate" } } 

exampleHeader = Header
    { version = (1,2)
    , characterName = "Bulgur"
    , saveName = "Zero save."
    , saveTime = "\NUL\ETB\NUL\EOT\a\226\NUL\NUL\NUL\RS"
    , gameTime = 264606
    , mapNumber = 35
    , mapName = "V13ENT.sav"
    , allHeader = ""
    }

main = do
    example <- exampleIO
    example' <- exampleIO'
    defaultMain $ testGroup "X"
        [ testCase "Header of example is parsed as expected"
            $ header example @?= exampleHeader
        , goldenVsString "`put . get` equals initial file"
            ("SLOT06" </> "SAVE.DAT") (fromStrict . runPut . put <$> exampleIO)
        , goldenVsString "Tweaked data serializes correctly"
            ("SLOT07" </> "SAVE.DAT") (fromStrict . runPut . put <$> exampleIO')
        ]

