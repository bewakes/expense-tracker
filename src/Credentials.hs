{-# LANGUAGE DeriveGeneric #-}

module Credentials where

import           Data.Aeson
import           Data.Text  (Text)
import           Prelude
import           Yesod.Auth

data ExtraCreds = ExtraCreds
    { extraCredsPicture       :: Text
    , extraCredsFirstName     :: Text
    , extraCredsLastName      :: Text
    , extraCredsEmail         :: Text
    , extraCredsEmailVerified:: Bool
    } deriving (Show, Eq)

instance FromJSON ExtraCreds where
    parseJSON = withObject "ExtraCreds" $ \v -> ExtraCreds
        <$> v .: "picture"
        <*> v .: "given_name"
        <*> v .: "family_name"
        <*> v .: "email"
        <*> v .: "email_verified"


getFromCredsExtra :: Text -> Creds a -> Maybe Text
getFromCredsExtra key creds =
    case result of
        (x:_) -> Just (snd x)
        _     -> Nothing
    where result = filter ((== key) . fst) $ credsExtra creds
