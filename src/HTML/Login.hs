{-# LANGUAGE OverloadedStrings #-}

module HTML.Login where

import Control.Lens.Operators
import Happstack.Server.ClientSession
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import qualified Data.ByteString as B

import Bindrop.State
import Bindrop.State.UploadDB
import Bindrop.State.Users
import HTML.Base
import HTML.Frames

loginPage :: Maybe User -> Html
loginPage u = baseHtml (Just "login") $ do
  H.head $ do
    H.title "login"
  H.body $ do
    H.header $ mainHeader
    mainMenu u
    case u of
      Nothing -> do
        H.div ! A.id "login-page" $ do
          H.h2 "Login"
          H.form ! action "u/login"
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

          H.p $ do
            "Don't have an account yet? "
            a ! href ("/u/r") $ "Register"
            " here."
          H.p $ do
            "Forgot your password? "
            a ! href ("/u/lost-password") $ "Reset"
            " it here."
      (Just u) -> do
        let userName = u ^. uName
        H.div ! A.id "user-info" $ do
          H.p (H.toHtml $ "You are already logged in as " ++ userName)

login :: Maybe User -> Html
login u = baseHtml (Just "login") $ do
  H.head $ do
    H.title "login"
  H.body $ do
    H.header $ mainHeader
    mainMenu u
    case u of
      (Just u) -> do
        let userName = u ^. uName
        H.div ! A.id "user-info" $ do
          H.h2 "Login"
          H.p "Login successful!"
          H.p (toHtml $ "Welcome back " ++ userName ++ "!")
          H.p $ do
            "Click "
            a ! href ("/") $ "here"
            " to return to the home page"
      Nothing -> do
        H.div ! A.id "user-info" $ do
          H.h2 "Login failed"
          H.p "Login failed. Invalid username or password"

logout :: Maybe User -> Html
logout u = baseHtml (Just "logout") $ do
  H.head $ do
    H.title "logout"
  H.body $ do
    H.header $ mainHeader
    mainMenu Nothing
    H.div ! A.id "user-info" $ do
      H.h2 "Log out"
      case u of
        (Just u) -> do
          H.p "You have logged out"
          H.p $ do
            "Click "
            a ! href ("/") $ "here"
            " to return to the home page"
        Nothing -> H.p "You were never logged in."

myAcct :: Maybe User -> Html
myAcct u = baseHtml (Just "my account") $ do
  H.head $ do
    H.title "my account"
  H.body $ do
    H.header $ mainHeader
    mainMenu u
    H.div ! A.id "user-info" $ do
      H.h2 "Account details"
      case u of
        (Just u) -> do
          let userName  = u ^. uName
          let userEmail = u ^. uEmail
          let uploadCount = u ^. count
          H.p (H.toHtml $ userName)
          H.p (H.toHtml $ userEmail)
          H.p (H.toHtml $ show uploadCount ++ " uploads")
          H.div ! A.id "change-pass" $ do
            H.form ! action "/u/m/p"
                   ! A.method "post" $ do
              H.p "Change your password"
              H.label "Old password: " >> input ! A.type_ "password"
                                                ! A.name "oldPass"
              H.br
              H.label "New password: " >> input ! A.type_ "password"
                                                ! A.name "newPass"
              H.br
              H.label "Confirm new password: " >> input ! A.type_ "password"
                                                        ! A.name "cNewPass"

              H.br

              --submit button
              input ! type_ "submit"
                    ! name "changePass"
                    ! value "Submit"

        Nothing -> H.p "You are not logged in"

changePassSuccess :: Maybe User -> Html
changePassSuccess u = baseHtml (Just "password change") $ do
  H.head $ do
    H.title "password change"
  H.body $ do
    H.header $ mainHeader
    mainMenu u
    H.div ! A.id "user-info" $ do
      H.h2 "Password change"
      case u of
        (Just u) -> H.p "Password changed successfully!"
        Nothing  -> H.p "You are not logged in"

changePassFail :: Maybe User -> Html
changePassFail u = baseHtml (Just "password change") $ do
  H.head $ do
    H.title "password change"
  H.body $ do
    H.header $ mainHeader
    mainMenu u
    H.div ! A.id "user-info" $ do
      H.h2 "Password change"
      case u of
        (Just u) -> H.p "Password change unsuccessful. Please make sure your passwords match"
        Nothing  -> H.p "You are not logged in"

lostPassInitial :: Maybe User -> Html
lostPassInitial u = baseHtml (Just "lost password") $ do
  H.head $ do
    H.title "lost password"
  H.body $ do
    H.header $ mainHeader
    mainMenu u
    H.div ! A.id "user-info" $ do
      H.h2 "Reset password"
      case u of
        (Just u) -> H.p "You are already logged in"

        Nothing -> do
          H.div ! A.id "reset-form" $ do
            H.form ! action "/u/lost-password/f"
                   ! A.method "post" $ do
              H.p "What is your username?"
              H.label "Username: " >> input ! A.type_ "text"
                                            ! A.name "username"
              H.br
              --submit button
              input ! type_ "submit"
                    ! name "resetPass"
                    ! value "Submit"


lostPassReset :: Maybe User -> String -> Html
lostPassReset u phrase = baseHtml (Just "reset password") $ do
  H.head $ do
    H.title "reset password"
  H.body $ do
    H.header $ mainHeader
    mainMenu u
    H.div ! A.id "user-info" $ do
      H.h2 "Reset password"
      case u of
        (Just u) -> H.p "You are already logged in"

        Nothing -> do -- present the reset form: email, phrase, answer
          H.div ! A.id "reset-form" $ do
            H.form ! action "/u/lost-password/r"
                   ! A.method "post" $ do
              H.p "Reset your password"
              H.br
              H.label "Email: " >> input ! A.type_ "text"
                                         ! A.name "email"
              H.br
              H.label (H.toHtml phrase) >> input ! A.type_ "text"
                                                 ! A.name "answer"
              H.br
              H.label "New password: " >> input ! A.type_ "password"
                                                ! A.name "pass"
              H.br
              H.label "Confirm new password: " >> input ! A.type_ "password"
                                                        ! A.name "cpass"
              H.br

              --submit button
              input ! type_ "submit"
                    ! name "resetPass"
                    ! value "Reset"


lostPassSuccess :: Maybe User -> Html
lostPassSuccess u = baseHtml (Just "password reset") $ do
  H.head $ do
    H.title "reset password"
  H.body $ do
    H.header $ mainHeader
    mainMenu u
    H.div ! A.id "user-info" $ do
      case u of
        (Just u) -> H.p "You are already logged in."

        Nothing -> H.p "Password reset successful!"

lostPassFail :: Maybe User -> Html
lostPassFail u = baseHtml (Just "password reset") $ do
  H.head $ do
    H.title "reset password"
  H.body $ do
    H.header $ mainHeader
    mainMenu u
    H.div ! A.id "user-info" $ do
      case u of
        (Just u) -> H.p "You are already logged in."

        Nothing -> H.p "Password reset failed"

