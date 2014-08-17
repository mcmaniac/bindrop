{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module HTML.Upload where

import Control.Lens.Operators
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
            let fileTime = file ^. uploadTime
            let infoLink = "localhost:8082/f/"    ++ file ^. sfname
            H.body $ do
              H.ul ! A.id "menu" $ do
                H.li $ H.a ! A.href "/" $ "Home"
                H.li $ H.a ! A.href "/a" $ "About"

              H.p (H.toHtml $ "file location: "     ++ filePath)
              H.p (H.toHtml $ "uploaded name: "     ++ fileName)
              H.p (H.toHtml $ "time of upload: "    ++ show fileTime)
              H.p (H.toHtml $ "link to file info: " ++ infoLink)

