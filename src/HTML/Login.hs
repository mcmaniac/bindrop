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
      H.form ! enctype "multipart/form-data"
             ! action "/u/l"
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

