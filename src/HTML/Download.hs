{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module HTML.Download where

import Control.Lens.Operators
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import HTML.Base
import UploadDB

viewDownload :: FileUpload -> Html
viewDownload file = baseHtml $ do
  H.title "Download"

  let filename = file ^. fname
  let filepath = file ^. fpath
  let sfilename = file ^. sfname
  H.p (H.toHtml $ "Original file name: " ++ filename)
  H.p (H.toHtml $ "File name on server: " ++ sfilename)
  H.p (H.toHtml $ "File path: "          ++ filepath)

