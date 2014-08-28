{-# LANGUAGE OverloadedStrings #-}

module HTML.About where

import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Bindrop.State.Users
import HTML.Base
import HTML.Frames

about :: Maybe User -> Html
about u = baseHtml (Just "about us") $ do
  let gitLink = "https://github.com/mcmaniac/bindrop"
  H.head $ do
    H.title "About"
  H.body $ do
    H.header $ mainHeader
    mainMenu u
    H.div ! A.id "about" $ do
      H.h2 "About bindrop"
      H.p $ do
            "Bindrop is a convenient solution for your file hosting needs "
            "with dependable functionality backed by a simple design."
      H.p $ do
            "Acid-state offers a NoSQL, RAM-cloud persistent data store, and "
            "in combination with the Happstack webserver library, makes for "
            " a very reliable service."
      H.p $ do
            a ! href (gitLink) $ "bindrop"
            " on github"

