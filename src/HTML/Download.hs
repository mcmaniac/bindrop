{-# LANGUAGE OverloadedStrings #-}

module HTML.Download where

import Control.Lens.Operators
import Data.Time.Format
import System.Locale
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import HTML.Base
import HTML.Frames
import Bindrop.State.UploadDB
import Bindrop.State.Users

viewDownload :: Maybe User -> FileUpload -> Html
viewDownload u file = baseHtml $ do
  H.head $ do
    H.title "Download"

  let filename  = file ^. fname
  let filepath  = file ^. fpath
  let sfilename = file ^. sfname
  let fileTime  = file ^. uploadTime
  let count     = file ^. dlCount
  let dlLink    = "/s/" ++ file ^. sfname

  H.body $ do
    H.header $ mainHeader
    mainMenu u
    H.div ! A.id "file-info" $ do
      H.h2 "Download"
      H.p (H.toHtml $ "File name: " ++ filename)
      H.p (H.toHtml $ "Uploaded at "        ++
        formatTime defaultTimeLocale "%H:%M - %a %Y.%m.%d" fileTime)
      if (count /= 1)
        then H.p (H.toHtml $ "Downloaded " ++ show count ++ " times")
        else H.p (H.toHtml $ "Downloaded " ++ show count ++ " time")
      H.p $ do "Click "
               a ! href (toValue dlLink) $ "here"
               " to download the file."

