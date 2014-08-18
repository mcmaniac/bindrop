{-# LANGUAGE OverloadedStrings #-}

module HTML.About where

import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import HTML.Base
import HTML.Frames

about :: Html
about = baseHtml $ do
  let gitLink = "https://github.com/mcmaniac/bindrop"
  H.head $ do
    H.title "About"
  H.body $ do
    H.header $ mainHeader
    mainMenu
    H.div ! A.id "about" $ do
      H.h2 "About bindrop"
      H.p "About us... todo"
      H.p $ do
            a ! href (gitLink) $ "bindrop"
            " on github"

