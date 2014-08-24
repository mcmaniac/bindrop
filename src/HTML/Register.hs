{-# LANGUAGE OverloadedStrings #-}

module HTML.Register where

import Control.Lens.Operators
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString as B

import HTML.Base
import HTML.Frames
import Bindrop.State.Users

register :: Maybe User -> Html
register u = baseHtml $ do
  H.head $ do
    H.title "register"
  H.body $ do
    H.header $ mainHeader
    mainMenu u
    case u of
      Nothing -> do
        H.div ! A.id "register-form" $ do
          H.h2 "Register a new user"
          H.form ! action "pr"
                 ! A.method "post" $ do
                   H.label "Username: " >> input ! A.type_ "text"
                                                 ! A.name "username"
                   H.br
                   H.label "E-mail: "   >> input ! A.type_ "text"
                                                 ! A.name "email"
                   H.br
                   H.label "Password: " >> input ! A.type_ "password"
                                                 ! A.name "pass"
                   H.br
                   input ! type_ "submit"
                         ! name "register"
                         ! value "Register"
      (Just u) -> do
        H.div ! A.id "user-info" $ do
          H.p "You already have an account"

registrationSuccess :: User -> Html
registrationSuccess u = baseHtml $ do
  let username  = u ^. uName
  let userEmail = u ^. uEmail
  let userPass  = u ^. uPass

  H.head $ do
    H.title "registration successful"
  H.body $ do
    H.header $ mainHeader
    mainMenu Nothing
    H.div ! A.id "user-info" $ do
      H.p (H.toHtml $ "Your username: "   ++ username)
      H.p (H.toHtml $ "Your e-mail: "     ++ userEmail)
      H.p (H.toHtml $ "Your password (hehe): " ++ show userPass)  -- obviously temporary

registrationFail :: Html
registrationFail = baseHtml $ do
  H.head $ do
    H.title "registration failed"
  H.body $ do
    H.header $ mainHeader
    mainMenu Nothing
    H.div ! A.id "user-info" $ do
      H.p "Registration failed :( "

