{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.State (get, gets)
import           Control.Lens
import           Data.Maybe
import           Data.ByteString (ByteString)
import           Data.ByteString as B
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time.Clock (getCurrentTime)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
import qualified Text.XmlHtml as X
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

testPost = 
  [  ("url 1", "this is body")
  , ("url 2", "this is it's body")
  ]

handleSearch :: Handler App App ()
handleSearch = method GET showQuery <|> method POST handleQuery
  where
    showQuery = do
      query <- getQueryParam "query"
      liftIO $ print query
      renderWithSplices "search_result" $ searchResultSplice
    handleQuery = do
      mquery <- getParam "query"
      liftIO $ print mquery
      case mquery of
        Just query -> do 
          let redirectUrl = B.concat [searchUrl, "?query=", query]
          redirect redirectUrl
        Nothing -> redirect "/"

searchResultSplice :: Splices (SnapletISplice App)
searchResultSplice = "searchResult" ## (renderPosts testPost)

renderPosts :: [(T.Text, T.Text)] -> SnapletISplice App
renderPosts = I.mapSplices $ I.runChildrenWith . postSplices

postSplices :: Monad n => (T.Text, T.Text) -> Splices (I.Splice n)
postSplices (url, body) = do
  "postUrl" ## I.textSplice url
  "postBody" ## I.textSplice body

------------------------------------------------------------------------------
-- | The application's routes.
searchUrl = "/search"

routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , (searchUrl,   handleSearch)
         , ("",          serveDirectory "static")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    modifyHeistState $ I.bindSplices $ do
      "searchUrl" ## I.textSplice (decodeUtf8 searchUrl)
    addRoutes routes
    addAuthSplices h auth

    return $ App h s a "This is spartan"

