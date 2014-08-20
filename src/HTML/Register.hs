{-# LANGUAGE OverloadedStrings #-}

module HTML.Register where

import Control.Lens.Operators
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import HTML.Base
import HTML.Frames
import Users

register :: Html
register = baseHtml $ do
  H.head $ do
    H.title "bindrop - register"
  H.body $ do
    H.header $ mainHeader
    mainMenu
    H.div ! A.id "register-form" $ do
      H.h2 "Register a new user"
      H.form ! enctype "multipart/form-data"
             ! action "/u/r"
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

registrationSuccess :: User -> Html
registrationSuccess newUser = baseHtml $ do
  let username = newUser ^. uName
  let userEmail = newUser ^. uEmail
  let userPass = newUser ^. uPass

  H.head $ do
    H.title "bindrop - registration successful"
  H.body $ do
    H.header $ mainHeader
    mainMenu
    H.div ! A.id "user-info" $ do
      H.p (H.toHtml $ "Your username: "     ++ username)
      H.p (H.toHtml $ "Your e-mail: "     ++ userEmail)
      H.p (H.toHtml $ "Your password (hehe): " ++ userPass) -- obviously temporary

