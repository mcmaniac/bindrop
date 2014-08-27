{-# LANGUAGE OverloadedStrings #-}

module HTML.Login where

import Control.Lens.Operators
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString as B

import HTML.Base
import HTML.Frames
import Bindrop.State
import Bindrop.State.UploadDB
import Bindrop.State.Users
import Happstack.Server.ClientSession

loginPage :: Maybe User -> Html
loginPage u = baseHtml $ do
  H.head $ do
    H.title "login"
  H.body $ do
    H.header $ mainHeader
    mainMenu u
    case u of
      Nothing -> do
        H.div ! A.id "login-page" $ do
          H.h2 "Login"
          H.br
          H.form ! action "pl"
                 ! A.method "post" $ do
                   H.label "Username: " >> input ! A.type_ "text"
                                                 ! A.name "username"
                   H.br
                   H.label "Password: " >> input ! A.type_ "password"
                                                 ! A.name "pass"
                   H.br
                   input ! type_ "submit"
                         ! name "login"
                         ! value "Login"

          H.p $ do "Don't have an account yet? "
                   a ! href ("/r") $ "Register"
                   " here."
      (Just u) -> do
        let userName = u ^. uName
        H.div ! A.id "user-info" $ do
          H.p (H.toHtml $ "You are already logged in as " ++ userName)

loginSuccessful :: Maybe User -> Html
loginSuccessful u = baseHtml $ do
  H.head $ do
    H.title "login successful!"
  H.body $ do
    H.header $ mainHeader
    mainMenu u
    case u of
      (Just u) -> do
        let userName = u ^. uName
        H.div ! A.id "user-info" $ do
          H.h2 "Login"
          H.br
          H.p "Login successful!"
          H.p (toHtml $ "Welcome back " ++ userName ++ "!")
          H.p $ do "Click "
                   a ! href ("/") $ "here"
                   " to return to the home page"
      Nothing -> H.p "Login failed"

loginFailed :: Maybe User ->  Html
loginFailed u = baseHtml $ do
  H.head $ do
    H.title "login failed"
  H.body $ do
    H.header $ mainHeader
    mainMenu u
    H.div ! A.id "user-info" $ do
      case u of
        Nothing -> do
          H.h2 "Login failed"
          H.br
          H.p "Login failed. Invalid username or password "
          H.br
          H.p $ do "Click "
                   a ! href ("/") $ "here"
                   " to return to the home page"
        (Just u) -> H.p "You are already logged in"

logout :: Html
logout = baseHtml $ do
  H.head $ do
    H.title "logout"
  H.body $ do
    H.header $ mainHeader
    mainMenu Nothing
    H.div ! A.id "user-info" $ do
      H.h2 "Log out"
      H.br
      H.p "You have logged out"
      H.p $ do "Click "
               a ! href ("/") $ "here"
               " to return to the home page"

myAcct :: Maybe User -> Html
myAcct u = baseHtml $ do
  H.head $ do
    H.title "my account"
  H.body $ do
    H.header $ mainHeader
    mainMenu u
    H.div ! A.id "user-info" $ do
      H.h2 "Account details"
      H.br
      case u of
        (Just u) -> do
          let userName  = u ^. uName
          let userEmail = u ^. uEmail
          let uploadCount = u ^. count
          H.p (H.toHtml $ "Username: " ++ userName)
          H.p (H.toHtml $ "E-mail:   " ++ userEmail)
          H.p (H.toHtml $ "Number of uploads: " ++ show uploadCount)
        Nothing -> H.p "You are not logged in"

