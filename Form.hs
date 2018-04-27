
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Form where

import           Brick
import           Brick.Focus
import           Brick.Forms
import           Brick.Widgets.Center
import           Brick.Widgets.Edit
import           Graphics.Vty
import           System.Environment

import           ReadSave

data Name = Jack

data FormLabels = VersionField | CharacterNameField | SaveNameField | SaveTimeField | GameTimeField
           | MapLevelField | MapNumberField | MapNameField | AllHeaderField
    deriving (Eq, Ord, Show, Read)

theMap :: AttrMap
theMap = attrMap defAttr
  [ (editAttr, white `on` black)
  , (editFocusedAttr, black `on` yellow)
  , (invalidFormInputAttr, white `on` red)
  , (focusedFormInputAttr, black `on` yellow)
  ]

form :: Header -> Form Header e FormLabels
form = newForm

    [ editShowableField _version       VersionField
    , editShowableField _characterName CharacterNameField
    , editShowableField _saveName      SaveNameField
    , editShowableField _saveTime      SaveTimeField
    , editShowableField _gameTime      GameTimeField
    , editShowableField _mapLevel      MapLevelField
    , editShowableField _mapNumber     MapNumberField
    , editShowableField _mapName       MapNameField
    , editShowableField _allHeader     AllHeaderField
    ]

main = do
    [slot] <- getArgs
    save <- readSave slot

    header' <- formState <$> defaultMain app (form $ header save)
    writeSave (save { header = header' }) slot

app :: App (Form Header e FormLabels) e FormLabels
app = App { appDraw = pure . renderForm
          , appChooseCursor = focusRingCursor formFocus
          , appHandleEvent = \s ev ->
            case ev of
                VtyEvent (EvResize {})     -> continue s
                VtyEvent (EvKey KEsc [])   -> halt s

                VtyEvent (EvKey KEnter []) -> halt s
                _ -> do
                    s' <- handleFormEvent ev s
                    continue s'


          , appStartEvent = return
          , appAttrMap = const theMap
          }
