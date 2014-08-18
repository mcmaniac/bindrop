{-# LANGUAGE OverloadedStrings #-}

module HTML.About where

import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import HTML.Base
import HTML.Frames

about :: Html -> Html
about = baseHtml $ do
  H.head $ do
    H.title "About"
  H.body $ do
    H.header $ mainHeader
    mainMenu

