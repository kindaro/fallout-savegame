
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Timestamp (fromTimestamp, toTimestamp, Timestamp) where

import           Data.Bifunctor      hiding (second)
import           Data.Serialize
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Word


unit_millisecond :: Integer
unit_millisecond = 1 * 10^9
unit_decisecond  = 10^2 * unit_millisecond
unit_second      = 10^3 * unit_millisecond
unit_minute      = 60   * unit_second
unit_hour        = 60   * unit_minute
unit_day         = 24   * unit_hour

data Timestamp = Timestamp { day :: Day, time :: TimeOfDay } deriving (Show, Read, Eq)

instance Serialize Timestamp
  where
    get = undefined

    put = undefined

fromTimestamp :: Timestamp -> Maybe Word32
fromTimestamp Timestamp{..} = fromInteger <$> checkBounds timestamp'
  where
    checkBounds x = if (fromIntegral (minBound :: Word32)) <= x
                       && x <= (fromIntegral (maxBound :: Word32))
                    then Just x
                    else Nothing
    timestamp' = day' + time'
    day' = (day `diffDays` zeroDate) * (unit_day `div` unit_decisecond)
    time' = (diffTimeToPicoseconds . timeOfDayToTime $ time) `div` unit_decisecond

toTimestamp :: Word32 -> Timestamp
toTimestamp deciseconds = Timestamp { day, time }
  where
    (day, time) = bimap (`addDays` zeroDate) (timeToTimeOfDay . picosecondsToDiffTime)
                $ (fromIntegral deciseconds * unit_decisecond) `divMod` unit_day

zeroDate = fromGregorian 2161 12 05
