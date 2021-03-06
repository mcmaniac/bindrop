{-# LANGUAGE OverloadedStrings #-}

module HTML.Index where

import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import HTML.Base
import HTML.Frames
import Bindrop.State.Users

index :: Maybe User -> Html -> Html
index u mostRecentUploadList = baseHtml (Just "home") $ do
  H.head $ do
    H.title "home"
  H.body $ do
    H.header $ mainHeader
    mainMenu u
    case u of
      (Just u) -> do
        H.div ! A.id "new-upload" $ do
          H.h2 "New upload"
          H.p ! A.id "info-upload" $ "10^8 bytes maximum allowed file size"
          -- upload form
          H.form ! enctype "multipart/form-data"
                 ! action "/"
                 ! A.method "post" $ do
            input ! A.type_ "file"
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
      H.h2 "Recent uploads"
      mostRecentUploadList

