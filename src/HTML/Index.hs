{-# LANGUAGE OverloadedStrings #-}

module HTML.Index where

import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import HTML.Base
import HTML.Frames
import Bindrop.State.Users

index :: Maybe User -> Html -> Html
index u mostRecentUploadList = baseHtml $ do
  H.head $ do
    H.title "bindrop"
  H.body $ do
    H.header $ mainHeader
    mainMenu u
    case u of
      (Just u) -> do
        H.div ! A.id "new-upload" $ do
          H.h2 "New Upload:"
          -- upload form
          H.form ! enctype "multipart/form-data"
                 ! action "/"
                 ! A.method "post" $ do
            H.label "Upload a file: " >> input ! A.type_ "file"
                                               ! A.name "fileUpload"
                                               ! A.size "50"

            H.br

            --submit button
            input ! type_ "submit"
                  ! name "upload"
                  ! value "Upload"

      Nothing -> do
        H.div ! A.id "new-upload" $ do
          H.br
          H.br
          H.br
          H.p $ do "You must "
                   a ! href ("/u") $ "log in"
                   " to upload files"

    H.div ! A.id "recent" $ do
      H.br
      H.h2 "Recent Uploads:"
      mostRecentUploadList

