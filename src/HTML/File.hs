{-# LANGUAGE OverloadedStrings #-}

module HTML.File where

import Control.Lens.Operators
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Bindrop.State.UploadDB

uploadedFile :: FileUpload -> Html
uploadedFile file = toHtml $ do
  let fileName = file ^. fname
  let fileTime = file ^. uploadTime
  let dlLink   = "/s/" ++ file ^. sfname
  let infoLink = "localhost:8082/f/" ++ file ^. sfname

  H.div ! A.id "recentInfo" $ do
    H.p $ do
      a ! href (toValue dlLink) $ H.toHtml fileName
    H.p (H.toHtml $ show fileTime)
    H.p (H.toHtml $ infoLink)

