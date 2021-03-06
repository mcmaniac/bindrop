{-# LANGUAGE OverloadedStrings #-}

module HTML.Register where

import Control.Lens.Operators
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString as B

import Bindrop.State.Users
import HTML.Base
import HTML.Frames

register :: Maybe User -> Html
register u = baseHtml (Just "register") $ do
  H.head $ do
    H.title "register"
  H.body $ do
    H.header $ mainHeader
    mainMenu u
    case u of
      Nothing -> do
        H.div ! A.id "register-form" $ do
          H.h2 "Register a new user"
          H.p "Please provide a valid e-mail address. It will be used to help confirm your identity in the case of a password reset."
          H.form ! action "r/process"
                 ! A.method "post" $ do
                   H.label "Username: " >> input ! A.type_ "text"
                                                 ! A.name "username"
                   H.br
                   H.label "E-mail: "   >> input ! A.type_ "text"
                                                 ! A.name "email"
                   H.br
                   H.label "Recovery question: " >> input ! A.type_ "text"
                                                          ! A.name "phrase"
                   H.br
                   H.label "Answer: " >> input ! A.type_ "text"
                                               ! A.name "answer"
                   H.br
                   H.label "Password: " >> input ! A.type_ "password"
                                                 ! A.name "pass"
                   H.br
                   H.label "Confirm password: " >> input ! A.type_ "password"
                                                         ! A.name "cpass"
                   H.br
                   input ! type_ "submit"
                         ! name "register"
                         ! value "Register"
      (Just u) -> do
        H.div ! A.id "user-info" $ do
          H.p "You already have an account"

registrationSuccess :: User -> Html
registrationSuccess u = baseHtml (Just "registration successful!") $ do
  let username  = u ^. uName
  let userEmail = u ^. uEmail
  let userPass  = u ^. uPass

  H.head $ do
    H.title "registration successful"
  H.body $ do
    H.header $ mainHeader
    mainMenu Nothing
    H.div ! A.id "user-info" $ do
      H.h2 "Registration successful!"
      H.p (H.toHtml $ "Your username: "   ++ username)
      H.p (H.toHtml $ "Your e-mail: "     ++ userEmail)
      H.br
      H.p $ do
        "Click "
        a ! href ("/") $ "here"
        " to return to the home page"

registrationFail :: Html
registrationFail = baseHtml (Just "registration failed") $ do
  H.head $ do
    H.title "registration failed"
  H.body $ do
    H.header $ mainHeader
    mainMenu Nothing
    H.div ! A.id "user-info" $ do
      H.h2 "Registration failed"
      H.p "Registration failed. Username and e-mail must be unique. Passwords must also match. "
      H.p $ do
        "Click "
        a ! href ("/") $ "here"
        " to return to the home page"

