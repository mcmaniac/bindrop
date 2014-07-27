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
  H.p "Select a file: "
  H.form $ do H.input ! A.type_  "file" ! A.name  "fileUpload"

