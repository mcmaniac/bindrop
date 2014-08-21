{-# LANGUAGE OverloadedStrings #-}

module HTML.Login where

import Control.Lens.Operators
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString as B

import HTML.Base
import HTML.Frames
import Bindrop.State.Users

loginPage :: Html
loginPage = baseHtml $ do
  H.head $ do
    H.title "login"
  H.body $ do
    H.header $ mainHeader
    mainMenu
    H.div ! A.id "login-page" $ do
      H.h2 "Login"
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

loginSuccessful :: String -> Html
loginSuccessful userName = baseHtml $ do
  H.head $ do
    H.title "login successful!"
  H.body $ do
    H.header $ mainHeader
    mainMenu
    H.div ! A.id "user-info" $ do
      H.p "Login successful!"
      H.p (H.toHtml $ "Welcome back " ++ userName ++ "!")
      H.p $ do "Click "
               a ! href ("/") $ "here"
               " to return to the home page"

loginFailed :: Html
loginFailed = baseHtml $ do
  H.head $ do
    H.title "login failed"
  H.body $ do
    H.header $ mainHeader
    mainMenu
    H.div ! A.id "user-info" $ do
      H.p "Login failed :@ "
      H.p $ do "Click "
               a ! href ("/") $ "here"
               " to return to the home page"

