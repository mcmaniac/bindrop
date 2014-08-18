{-# LANGUAGE OverloadedStrings #-}

module HTML.Index where

import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import HTML.Base
import HTML.Frames

index :: Html -> Html
index mostRecentUploadList = baseHtml $ do
  H.title "bindrop"
  H.header $ mainHeader
  H.body $ do
    mainMenu

    H.div ! A.id "new-upload" $ do
      H.h2 "New Upload:"

      H.form ! enctype "multipart/form-data"
             ! action "/"
             ! A.method "post" $ do
               H.label "Upload a file: " >> input ! A.type_ "file"
                                                  ! A.name "fileUpload"
                                                  ! A.size "50"
               input ! type_ "submit"
                     ! name "upload"

    H.div ! A.id "recent" $ do
      H.br
      H.h2 "Recent Uploads:"
      mostRecentUploadList

