{-# LANGUAGE OverloadedStrings #-}

module HTML.Frames where

import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import HTML.Base
import Bindrop.State.Users

mainHeader :: Html
mainHeader = do
    H.h1 $ do
      "bindrop "
      H.span $ ":: AcidState UploadDB"
      H.p ! A.id "info" $ "a haskell happstack file hosting service"

mainMenu :: Maybe User -> Html
mainMenu u = do
  H.div ! A.id "mainMenu" $ do
    H.ul $ do -- ! A.id "menu" $ do
      case u of
        (Just u) -> do
          H.li $ H.a ! A.href "/"   $ "Home"
          H.li $ H.a ! A.href "/m"  $ "My account"
          H.li $ H.a ! A.href "/mu" $ "My uploads"
          H.li $ H.a ! A.href "/l"  $ "Logout"
          H.li $ H.a ! A.href "/a"  $ "About"

        Nothing -> do
          H.li $ H.a ! A.href "/"  $ "Home"
          H.li $ H.a ! A.href "/u" $ "Login"
          H.li $ H.p ""
          H.li $ H.p ""
          H.li $ H.a ! A.href "/a" $ "About"

