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

mainMenu :: Html
mainMenu = do
  H.div ! A.id "mainMenu" $
    H.ul ! A.id "menu" $ do
      H.li $ H.a ! A.href "/" $ "Home"
      H.li $ H.a ! A.href "/a" $ "About"

