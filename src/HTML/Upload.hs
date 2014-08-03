{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module HTML.Upload where

import Control.Lens.Operators
--import Happstack.Server (ContentType)
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import HTML.Base
import UploadDB

upload :: FileUpload -> Html
upload f = baseHtml $ do
  H.title "File Information"
  mkBody f
    where mkBody file = do
            let filePath = file ^. fpath
            let fileName = file ^. fname
            let infoLink = "localhost:8082/f/" ++ file ^. sfname
            let dlLink   = "localhost:8082/s/" ++ file ^. sfname
            H.p (H.toHtml $ "file location: " ++ filePath)
            H.p (H.toHtml $ "uploaded name: " ++ fileName)
            --H.p (H.toHtml $ "content type: " ++ show contentType)
            H.p (H.toHtml $ "link to file info: " ++ infoLink)
            H.p (H.toHtml $ "download link: " ++ dlLink)

