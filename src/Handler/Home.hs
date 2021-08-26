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


parseParams :: Maybe Text -> Maybe Text ->  IO (Day, Day, Day)
parseParams maybeMonthraw maybeYearraw = do
    currTime <- getCurrentTime
    let monthMaybe = maybeMonthraw >>= readMaybe . T.unpack
        yearMaybe = maybeYearraw >>= readMaybe . T.unpack
        curr_ = mkDay <$> yearMaybe <*> monthMaybe <*> Just 1
        currMonth = case curr_ of
                Nothing -> utctDay currTime
                Just x  -> x
        prevMonth = addDays (-32) currMonth
        nextMonth = addDays 32 currMonth
    return (prevMonth, currMonth, nextMonth)

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
    yraw <- lookupGetParam "year"
    (prev, curr, next) <- liftIO $ parseParams mraw yraw
    let (prevYear, prevMonth, _) = toGregorian prev
        (nextYear, nextMonth, _) = toGregorian next
    case uidMaybe of
      Nothing -> defaultLayout $ do
        setTitle "Welcome To Expense Tracker!"
        $(widgetFile "welcome")
      _ -> do
        (selGrpMaybe, _) <- getUserCurrentGroupFromParam
        expenses <- case selGrpMaybe of
          Nothing               -> pure []
          Just (Entity grpid _) -> runDB $ getAllGroupExpenses grpid curr
        let grpId = case selGrpMaybe of
                    Nothing -> -1
                    Just e  -> E.fromSqlKey $ entityKey e

        let total = P.sum <$> mapM fst4 expenses
        defaultLayout $ do
          setTitle "Expenses Home"
          $(widgetFile "homepage")

getExpenseSummaryR :: Handler Value
getExpenseSummaryR = do
    uidMaybe <- maybeAuthId
    mraw <- lookupGetParam "month"
    yraw <- lookupGetParam "year"
    (_, curr, _) <- liftIO $ parseParams mraw yraw
    case uidMaybe of
      Nothing -> returnJson $ object []
      _ -> do
        (selGrpMaybe, _) <- getUserCurrentGroupFromParam
        case selGrpMaybe of
          Nothing -> sendResponseStatus status400 $ object []
          Just grp -> do
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
