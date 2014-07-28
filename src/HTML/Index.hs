{-# LANGUAGE OverloadedStrings #-}

module HTML.Index where

import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import HTML.Base

index :: Html
index = baseHtml $ do
  H.title "asd"
  H.h1 "asd"

--  Practice with text only first:
  H.form ! enctype "multipart/form-data"
         ! action "/"
         ! A.method "post" $ do
           H.label "String: " >> input ! type_ "text"
                                       ! name "testString"
                                       ! size "25"
           input ! type_ "submit"
                 ! name "upload"

