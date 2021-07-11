{-# LANGUAGE DeriveGeneric #-}
module Utils where

import           Data.Fixed
import           Data.Maybe
import           Data.Time
import           Database.Esqueleto.Experimental as E
import           Prelude

coerceMaybe :: Maybe (Maybe a) -> Maybe a
coerceMaybe Nothing        = Nothing
coerceMaybe (Just Nothing) = Nothing
coerceMaybe (Just a)       = a

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

mkUTCTime :: (Integer, Int, Int)
          -> (Int, Int, Pico)
          -> UTCTime
mkUTCTime (year, mon, day_) (hour, mn, sec) =
    UTCTime (fromGregorian year mon day_)
       (timeOfDayToTime (TimeOfDay hour mn sec))


mkDay :: Int -> Int -> Int -> Day
mkDay y m d = utctDay $ mkUTCTime (fromIntegral y, m, d) (0, 0, 0)

extractValFromTuple :: (E.Value a, E.Value b) -> (a, b)
extractValFromTuple (a, b) = (E.unValue a, E.unValue b)

extractValFromTuple_ :: (a -> a') -> (b -> b') -> (E.Value a, E.Value b) -> (a', b')
extractValFromTuple_ fa fb (a, b) = (fa $ E.unValue a, fb $ E.unValue b)
