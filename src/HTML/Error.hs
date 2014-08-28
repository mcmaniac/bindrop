{-# LANGUAGE OverloadedStrings #-}

module HTML.Error where

import Text.Blaze.Html5

import HTML.Base

notFound :: Html
notFound = baseHtml (Just "error") $ do
  h1 "404 - Not Found"
  p "The page you requested could not be found."

