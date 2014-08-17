{-# LANGUAGE OverloadedStrings #-}

module HTML.Frames where

import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import HTML.Base

mainHeader :: Html
mainHeader = do
    H.h1 $ do
      "bindrop "
      H.span $ ":: AcidState UploadDB"
      H.p ! A.id "info" $ "a haskell happstack file hosting service"

