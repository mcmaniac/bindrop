{-# LANGUAGE OverloadedStrings #-}

module HTML.Download where

import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import HTML.Base

download :: String -> Html
download fileName = baseHtml $ do
  H.title "Download"
  H.p     (H.toHtml $ "File name: " ++ fileName)

