{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

module HTML.Base where

import Control.Monad
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

htmlHeader
  :: Maybe String     -- ^ HTML <title> value
  -> [FilePath]       -- ^ CSS files
  -> [FilePath]       -- ^ JS scripts
  -> Html
htmlHeader title cssFiles scripts =

  H.head $ do
    H.title . toHtml $
      maybe "bindrop" ("bindrop - " ++) title

    -- load javascript
    forM_ scripts $ \s ->
      H.script ! A.type_ "text/javascript" !
        A.src (toValue $ "/static/js/" ++ s) $ return ()

    -- load css
    forM_ cssFiles $ \c ->
      H.link ! A.type_ "text/css" !
        A.href (toValue $ "/static/css/" ++ c) ! A.rel "stylesheet"
    --Ubuntu font
    H.link ! A.type_ "text/css" !
      A.href ("http://fonts.googleapis.com/css?family=Ubuntu") !
      A.rel "stylesheet"

baseHtml :: Html -> Html
baseHtml content = docTypeHtml $ do
  htmlHeader Nothing ["base.css"] []
  content
