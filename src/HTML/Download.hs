{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module HTML.Download where

import Control.Lens.Operators
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import HTML.Base
import UploadDB

download :: String -> FileUpload -> Html
download sName file = baseHtml $ do
  H.title "Download"
  H.p     (H.toHtml $ "File name on server: " ++ sName)

  let filename = file ^. fname
  let filepath = file ^. fpath
  H.p (H.toHtml $ "Original file name: " ++ filename)
  H.p (H.toHtml $ "File path: "          ++ filepath)
--put the query in downloadPart or something in Main THEN work with it here
