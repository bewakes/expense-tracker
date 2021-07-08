module Utils where

import           Data.Fixed
import           Data.Time
import           Data.Time.Calendar
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
mkUTCTime (year, mon, day) (hour, mn, sec) =
    UTCTime (fromGregorian year mon day)
       (timeOfDayToTime (TimeOfDay hour mn sec))


mkDay :: Int -> Int -> Int -> Day
mkDay y m d = utctDay $ mkUTCTime (fromIntegral y, m, d) (0, 0, 0)
