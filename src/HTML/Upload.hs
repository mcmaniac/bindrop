{-# LANGUAGE OverloadedStrings #-}

module HTML.Upload where

import Control.Lens.Operators
import Data.Time.Format
import System.Locale
import Text.Blaze.Html
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Bindrop.State.UploadDB
import Bindrop.State.Users
import HTML.Base
import HTML.Frames

upload :: Maybe User -> FileUpload -> Html
upload u f = baseHtml (Just "file info") $ do
  H.head $ do
    H.title "File Information"

  mkBody f
    where mkBody file = do
            let filePath = file ^. fpath
            let fileName = file ^. fname
            let fileTime = file ^. uploadTime
            let privacy  = file ^. public
            let infoLink = "bindrop.de/f/" ++ file ^. sfname

            H.body $ do
              H.header $ mainHeader
              mainMenu u
              H.div ! A.id "file-info" $ do
                H.h2 "Upload successful"
                H.p (H.toHtml $ fileName)
                H.p (H.toHtml $ formatTime defaultTimeLocale
                  "%H:%M - %a %Y.%m.%d" fileTime)
                H.p (H.toHtml $ "Link: " ++ infoLink)

myUploads :: Maybe User -> Html -> Html
myUploads u userUploadList = baseHtml (Just "my uploads") $ do
  H.head $ do
    H.title "my uploads"
  H.body $ do
    H.header $ mainHeader
    mainMenu u

    H.div ! A.id "user-uploads" $ do
      H.h2 "My uploads"
      H.br
      userUploadList

