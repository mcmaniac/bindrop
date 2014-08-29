{-# LANGUAGE OverloadedStrings #-}

module HTML.About where

import Control.Lens.Operators ( (^.) )
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Bindrop.State.Journal
import Bindrop.State.Users
import HTML.Base
import HTML.Frames

about :: Maybe User -> Maybe Journal -> Html
about u  j = baseHtml (Just "about us") $ do
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

      H.br
      H.h2 "bindrop statistics"

      case j of
        (Just j) -> do
          let totalUploads   = j ^. ucount
          let totalDownloads = j ^. dcount
          let totalUsers     = j ^. acount

          H.p (H.toHtml $ show totalUploads ++ " uploads")
          H.p (H.toHtml $ show totalDownloads ++ " downloads")
          H.p (H.toHtml $ show totalUsers ++ " users")

        Nothing -> H.p "No statistics to report"

