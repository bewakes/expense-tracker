{-# LANGUAGE QuasiQuotes #-}
module Handler.Category where

import           Import


data CategoryForm = CategoryForm
    { categoryName :: Text
    , categoryUser :: Int
    }

getCategoryR :: Handler Html
getCategoryR = do
    defaultLayout $ do
        setTitle "Categories"
        [whamlet| <h2> This is categories page|]


postCategoryR :: Handler Html
postCategoryR = do
    defaultLayout $ do
        setTitle "Posting categories"
        [whamlet| <h2> This is categories post page|]
