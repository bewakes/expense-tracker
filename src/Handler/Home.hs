{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
module Handler.Home where

import           Data.Maybe
import qualified Data.Text                       as T
import           Data.Time.Calendar
import           Data.Time.Clock
import qualified Database.Esqueleto.Experimental as E
import           Import
import qualified Prelude                         as P
import           Text.Read
import           Utils.Data
import           Utils.Db                        hiding (DB)


parseParams :: Maybe Text -> Maybe Text -> Maybe Text -> IO (Day, Day, Day, Maybe GroupId)
parseParams maybeMonthraw maybeYearraw maybeGrpraw = do
    currTime <- getCurrentTime
    let monthMaybe = maybeMonthraw >>= readMaybe . T.unpack
        yearMaybe = maybeYearraw >>= readMaybe . T.unpack
        grpMaybe = maybeGrpraw >>= readMaybe . T.unpack
        curr_ = mkDay <$> yearMaybe <*> monthMaybe <*> Just 1
        currMonth = case curr_ of
                Nothing -> utctDay currTime
                Just x  -> x
        prevMonth = addDays (-32) currMonth
        nextMonth = addDays 32 currMonth
    return (prevMonth, currMonth, nextMonth, E.toSqlKey <$> grpMaybe)

getGroup :: UserId -> Maybe GroupId -> Handler (Entity Group, [Entity Group])
getGroup uid gidMaybe = do
    userGroups <- runDB $ getUserGroups uid
    let l = length userGroups
        grp_ = if l > 0 then P.head userGroups else error "Something went wrong"
        grp = case gidMaybe of
                Nothing -> grp_
                Just g -> P.head $ P.filter ((== g) . entityKey) userGroups ++ [grp_] -- last addition is for failsafe case
    return (grp, userGroups)

getHomeR :: Handler Html
getHomeR = do
    uidMaybe <- maybeAuthId
    mraw <- lookupGetParam "month"
    graw <- lookupGetParam "group"
    yraw <- lookupGetParam "year"
    (prev, curr, next, gidMaybe) <- liftIO $ parseParams mraw yraw graw
    let (prevYear, prevMonth, _) = toGregorian prev
        (nextYear, nextMonth, _) = toGregorian next
    case uidMaybe of
      Nothing -> defaultLayout $ do
        setTitle "Welcome To Expense Tracker!"
        $(widgetFile "welcome")
      Just uid -> do
        (grp, userGroups) <- getGroup uid gidMaybe
        expenses <- runDB $ getAllGroupExpenses (entityKey grp) curr

        let total = P.sum <$> mapM fst4 expenses
            grpId = E.fromSqlKey $ entityKey grp
            isSelected k = k == entityKey grp
        defaultLayout $ do
          setTitle "Expenses Home"
          $(widgetFile "homepage")

getExpenseSummaryR :: Handler Value
getExpenseSummaryR = do
    uidMaybe <- maybeAuthId
    mraw <- lookupGetParam "month"
    yraw <- lookupGetParam "year"
    graw <- lookupGetParam "group"
    (_, curr, _, gidMaybe) <- liftIO $ parseParams mraw yraw graw
    case uidMaybe of
      Nothing -> returnJson $ object []
      Just uid -> do
        (grp, _) <- getGroup uid gidMaybe
        month_summary <- runDB $ getMonthAggregated (entityKey grp) curr
        category_summary <- runDB $ getCategoryAggregated (entityKey grp) curr
        let month_processed = map (extractValFromTuple_ id (fromMaybe 0)) month_summary
            cat_processed = map (extractValFromTuple_ id (fromMaybe 0)) category_summary
            total = P.sum $ P.map snd cat_processed
        returnJson $ object
            [ "category_data" .= toJSON cat_processed
            , "month_data" .= toJSON month_processed
            , "month" .= formatTime defaultTimeLocale "%B" curr
            , "total" .= total
            ]
