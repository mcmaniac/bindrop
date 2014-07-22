{-# LANGUAGE OverloadedStrings #-}

module HTML.Index where

import Text.Blaze.Html
import Text.Blaze.Html5

import HTML.Base

index :: Html
index = baseHtml $ do
  title "asd"
  h1 "asd"
