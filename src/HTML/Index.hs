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

  H.form ! enctype "multipart/form-data"
         ! action "/"
         ! A.method "post" $ do
           H.label "Upload a file: " >> input ! A.type_ "file"
                                              ! A.name "fileUpload"
                                              ! A.size "50"
           input ! type_ "submit"
                 ! name "upload"

