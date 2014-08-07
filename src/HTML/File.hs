{-# LANGUAGE OverloadedStrings #-}

module HTML.File where

import Control.Lens.Operators
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import UploadDB

recentFile :: FileUpload -> Html
recentFile file = toHtml $ do
  let fileName = file ^. fname
  let fileTime = file ^. uploadTime
  let dlLink   = "/s/" ++ file ^. sfname
  let infoLink = "localhost:8082/f/" ++ file ^. sfname

  H.p $ do a ! href (toValue dlLink) $ H.toHtml fileName
  H.p (H.toHtml $ "time of upload: "    ++ show fileTime)
  H.p (H.toHtml $ "link to file info: " ++ infoLink)
  H.br

