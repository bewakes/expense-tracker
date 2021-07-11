{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
module Utils where

import           Data.Fixed
import           Data.Maybe
import           Data.Time
import           Database.Esqueleto.Experimental as E
import           Import

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

getUserGroups :: UserId -> DB [Entity Group]
getUserGroups u = E.select $ do
    (ug E.:& grp) <-
        E.from $ E.table @UsersGroups `E.InnerJoin` E.table @Group
        `E.on` (\(ug E.:& grp) -> ug E.^. UsersGroupsGroupId E.==. grp E.^. GroupId)
    E.where_ (ug E.^. UsersGroupsUserId E.==. E.val u)
    E.orderBy [E.asc $ ug E.^. UsersGroupsIsDefault]
    return grp
