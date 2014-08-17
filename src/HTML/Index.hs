{-# LANGUAGE OverloadedStrings #-}

module HTML.Index where

import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import HTML.Base

index :: Html -> Html
index mostRecentUploadList = baseHtml $ do
  H.title "bindrop"
  H.body $ do
    H.h1 $ do
      "bindrop "
      H.span $ ":: AcidState UploadDB"

    H.ul ! A.id "menu" $ do
      H.li $ H.a ! A.href "/" $ "Home"
      H.li $ H.a ! A.href "/a" $ "About"

    H.h2 "New Upload:"

    H.form ! enctype "multipart/form-data"
           ! action "/"
           ! A.method "post" $ do
             H.label "Upload a file: " >> input ! A.type_ "file"
                                                ! A.name "fileUpload"
                                                ! A.size "50"
             input ! type_ "submit"
                   ! name "upload"
    H.br
    H.h2 "Recent Uploads:"
    mostRecentUploadList

