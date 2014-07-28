{-# LANGUAGE OverloadedStrings #-}

module HTML.Upload where

import Happstack.Server (ContentType)
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import HTML.Base

upload :: (FilePath, FilePath, ContentType) -> Html
upload fileUpload = baseHtml $ do
  H.title "File Information"
  mkBody fileUpload
    where mkBody (tmpFile, uploadName, contentType) = do
            H.p (H.toHtml $ "temp file: " ++ tmpFile)
            H.p (H.toHtml $ "uploaded name: " ++ uploadName)
            H.p (H.toHtml $ "content type: " ++ show contentType)

