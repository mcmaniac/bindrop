{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module HTML.Download where

import Control.Lens.Operators
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import HTML.Base
import HTML.Frames
import UploadDB

viewDownload :: FileUpload -> Html
viewDownload file = baseHtml $ do
  H.header $ do
    mainHeader
    H.title "Download"

  let filename = file ^. fname
  let filepath = file ^. fpath
  let sfilename = file ^. sfname
  let fileTime = file ^. uploadTime
  let dlLink   = "/s/" ++ file ^. sfname

  H.body $ do
    H.ul ! A.id "menu" $ do
      H.li $ H.a ! A.href "/" $ "Home"
      H.li $ H.a ! A.href "/a" $ "About"

    H.p (H.toHtml $ "Original file name: "   ++ filename)
    H.p (H.toHtml $ "File name on server: "  ++ sfilename)
    H.p (H.toHtml $ "File path: "            ++ filepath)
    H.p (H.toHtml $ "Uploaded at: "          ++ show fileTime)
    p $ do "Click "
           a ! href (toValue dlLink) $ "here"
           " to download the file."

