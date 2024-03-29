{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Foundation where

import           Control.Monad.Logger            (LogSource)
import           Credentials
import           Data.Aeson
import           Data.Maybe
import qualified Data.Text.Lazy                  as LT
import qualified Database.Esqueleto.Experimental as E
import           Database.Persist.Sql            (ConnectionPool, runSqlPool)
import           ExpenseItems
import           Import.NoFoundation
import           Text.Hamlet                     (hamletFile)
import           Text.Jasmine                    (minifym)

-- Used only when in "auth-dummy-login" setting is enabled.

import qualified Data.CaseInsensitive            as CI
import qualified Data.Text.Encoding              as TE
import           Yesod.Auth.OAuth2.Google
import           Yesod.Core.Types                (Logger)
import qualified Yesod.Core.Unsafe               as Unsafe
import           Yesod.Default.Util              (addStaticContentExternal)

import           Utils.Data
import           Utils.Db


-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings        :: AppSettings
    , appStatic          :: Static -- ^ Settings for static file serving.
    , appConnPool        :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager     :: Manager
    , appLogger          :: Logger
    , appGoogleOAuthKeys :: (Text, Text)
    }

data MenuItem = MenuItem
    { menuItemLabel          :: Text
    , menuItemRoute          :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadUnliftIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing   -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        (60 * 24 * 3)    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        muser <- maybeAuthPair
        mcurrentRoute <- getCurrentRoute

        -- Define the menu items of the header.
        let menuItems =
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = "Home"
                    , menuItemRoute = HomeR
                    , menuItemAccessCallback = True
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Groups"
                    , menuItemRoute = GroupR
                    , menuItemAccessCallback = True
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Categories"
                    , menuItemRoute = CategoryR
                    , menuItemAccessCallback = True
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "About"
                    , menuItemRoute = AboutR
                    , menuItemAccessCallback = True
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Login"
                    , menuItemRoute = AuthR LoginR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Logout"
                    , menuItemRoute = AuthR LogoutR
                    , menuItemAccessCallback = isJust muser
                    }
                ]

        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        (mgrp, navUserGroups) <- getUserCurrentGroupFromParam
        let selectedGroupName = case mgrp of
                              Nothing  -> ""
                              Just grp -> groupName $ E.entityVal grp

        let selectedGroupId = case mgrp of
                                Nothing  -> -1
                                Just grp -> E.fromSqlKey $ E.entityKey grp

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_custom_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute
        :: App
        -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _           = return Authorized
    isAuthorized HomeR _               = return Authorized
    isAuthorized AboutR _              = return Authorized
    isAuthorized FaviconR _            = return Authorized
    isAuthorized RobotsR _             = return Authorized
    isAuthorized (StaticR _) _         = return Authorized
    -- the profile route requires that the user is authenticated, so we
    -- delegate to that function
    isAuthorized CategoryR _           = isAuthenticated
    isAuthorized UserR _               = isAuthenticated
    isAuthorized GroupR _              = isAuthenticated
    isAuthorized GroupNewR _           = isAuthenticated
    isAuthorized (GroupDetailR _) _    = isAuthenticated
    isAuthorized ExpenseNewR _         = isAuthenticated
    isAuthorized (ExpenseEditR _) _    = isAuthenticated
    isAuthorized (ExpenseDeleteR _) _  = isAuthenticated
    isAuthorized ExpenseSummaryR _     = isAuthenticated
    isAuthorized (GroupNewMemberR _) _ = isAuthenticated
    isAuthorized (UserQueryR _) _      = isAuthenticated
    isAuthorized CategoryNewR _        = isAuthenticated
    isAuthorized (CategoryEditR _) _   = isAuthenticated
    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

-- Define breadcrumbs.
{-
instance YesodBreadcrumbs App where
    -- Takes the route that the user is currently on, and returns a tuple
    -- of the 'Text' that you want the label to display, and a previous
    -- breadcrumb route.
    breadcrumb
        :: Route App  -- ^ The route the user is visiting currently.
        -> Handler (Text, Maybe (Route App))
    breadcrumb HomeR     = return ("Home", Nothing)
    breadcrumb (AuthR _) = return ("Login", Just HomeR)
    breadcrumb  _        = return ("home", Nothing)
-}

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = AuthR LoginR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
        let maybedata = getFromCredsExtra "userResponse" creds
            maybeExtraCreds = coerceMaybe $ (decode . encodeUtf8 . LT.fromStrict) <$> maybedata
        x <- getBy $ UniqueUser $ credsIdent creds
        now <- liftIO getCurrentTime
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> do
                usrid <- insert User
                    { userIdent = credsIdent creds
                    , userPassword = Nothing
                    , userFirstName = maybe "" extraCredsFirstName maybeExtraCreds
                    , userLastName = maybe "" extraCredsLastName maybeExtraCreds
                    , userEmail = fromJust $ extraCredsEmail <$> maybeExtraCreds
                    }
                -- Create a group for the user
                grpid <- insert Group
                    { groupName = "Personal"
                    , groupCreatedAt = now
                    , groupCreatedBy = usrid
                    , groupDescription = "This is your personal account"
                    }
                time <- liftIO getCurrentTime
                _ <- insert UsersGroups
                    { usersGroupsUserId     = usrid
                    , usersGroupsGroupId    = grpid
                    , usersGroupsIsDefault  = True
                    , usersGroupsJoinedAt   = time
                    , usersGroupsAddedBy = Nothing
                    , usersGroupsRole       = SuperAdmin
                    }
                -- Add default categories
                let categories = getDefaultCategories usrid grpid
                _ <- insertMany_ categories
                return $ Authenticated usrid

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins app = [oauth2GoogleScoped ["profile", "email", "openid"] cid csec]
        where (cid, csec) = appGoogleOAuthKeys app

    loginHandler = authLayout $ do
        setTitle "Expenses Login"
        $(widgetFile "login")

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _  -> Authorized

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

getUserCurrentGroupFromParam :: Handler (Maybe (Entity Group), [Entity Group])
getUserCurrentGroupFromParam = do
    uidMaybe <- maybeAuthId
    gidMaybe <- parseIntegerFromParam <$> lookupGetParam "groupId"
    usrGroups <- case uidMaybe of
                    Nothing  -> return []
                    Just uid -> runDB $ getUserGroups uid
    let selectedGroup = case usrGroups of
                        [] -> Nothing
                        xs@(x:_) -> case gidMaybe of
                                  Nothing -> Just x
                                  Just gid -> case filter (\(Entity k _) -> k == E.toSqlKey (fromInteger gid)) xs of
                                                []   -> Nothing
                                                g: _ -> Just g
    pure (selectedGroup, usrGroups)

data UserInfo = UserInfo
    { userId     :: UserId
    , currGroup  :: Entity Group
    , userGroups :: [Entity Group]
    }

loginRedirectOr :: (UserInfo -> Handler Html) -> Handler Html
loginRedirectOr handler = do
    uidMaybe <- maybeAuthId
    case uidMaybe of
      Nothing  -> redirect $ AuthR LoginR
      Just uid -> do
          (mgrp, grps) <- getUserCurrentGroupFromParam
          case mgrp of
            Nothing -> sendResponseStatus status404 (TypedContent typeHtml "<h3>No groups found for user</h3>")
            Just grp -> handler $ UserInfo uid grp grps
