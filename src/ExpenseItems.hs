module ExpenseItems
    ( getDefaultCategories
    )
where

import           Data.Text (Text)
import           Model
import           Prelude

defaultGroups :: [Text]
defaultGroups =
    [ "Groccery"
    , "Utility"
    , "Medicine"
    , "Fruits"
    , "Stationery"
    , "Miscellaneous"
    ]


getDefaultCategories :: UserId -> GroupId -> [Category]
getDefaultCategories usrid grpid = map (\x -> Category x usrid grpid) defaultGroups
