{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module HTML.Upload where

import Control.Lens.Operators
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import HTML.Base
import HTML.Frames
import Bindrop.State.UploadDB
import Bindrop.State.Users

upload :: Maybe User -> FileUpload -> Html
upload u f = baseHtml $ do
  H.head $ do
    H.title "File Information"

  mkBody f
    where mkBody file = do
            let filePath = file ^. fpath
            let fileName = file ^. fname
            let fileTime = file ^. uploadTime
            let privacy  = file ^. public
            let infoLink = "localhost:8082/f/"    ++ file ^. sfname

            H.body $ do
              H.header $ mainHeader
              mainMenu u
              H.div ! A.id "file-info" $ do
                H.p (H.toHtml $ "file location: "     ++ filePath)
                H.p (H.toHtml $ "uploaded name: "     ++ fileName)
                H.p (H.toHtml $ "time of upload: "    ++ show fileTime)
                H.p (H.toHtml $ "link to file info: " ++ infoLink)
                H.p (H.toHtml $ "public: "            ++ show privacy)

