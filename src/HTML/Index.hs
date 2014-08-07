{-# LANGUAGE OverloadedStrings #-}

module HTML.Index where

import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import HTML.Base

index :: Html -> Html
index mostRecentUploadList = baseHtml $ do
  H.title "bindrop"
  H.h1 "New Upload:"

  H.form ! enctype "multipart/form-data"
         ! action "/"
         ! A.method "post" $ do
           H.label "Upload a file: " >> input ! A.type_ "file"
                                              ! A.name "fileUpload"
                                              ! A.size "50"
           input ! type_ "submit"
                 ! name "upload"
  H.br
  H.h1 "Recent Uploads:"
  mostRecentUploadList

