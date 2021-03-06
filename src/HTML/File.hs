{-# LANGUAGE OverloadedStrings #-}

module HTML.File where

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

recentUpload :: FileUpload -> Html
recentUpload file = toHtml $ do
  let fileName = file ^. fname
  let fileTime = file ^. uploadTime
  let counter  = file ^. dlCount
  let dlLink   = "/s/" ++ file ^. sfname
  let infoLink = "bindrop.de/f/" ++ file ^. sfname

  H.div ! A.id "uploadInfo" $ do
    H.p $ do
      a ! href (toValue dlLink) $ H.toHtml fileName
    H.p (H.toHtml $ formatTime defaultTimeLocale "%H:%M - %a %Y.%m.%d" fileTime)
    H.p (H.toHtml $ infoLink)
    H.div ! A.id "dl-counter-recent" $ do
      if(counter /= 1)
        then H.p (H.toHtml $ show counter ++ " downloads")
        else H.p (H.toHtml $ show counter ++ " download")


uploadedFile :: User -> FileUpload -> Html
uploadedFile mU file = toHtml $ do
  let fileName = file ^. fname
  let fileTime = file ^. uploadTime
  let counter  = file ^. dlCount
  let dlLink   = "/s/" ++ file ^. sfname
  let infoLink = "bindrop.de/f/" ++ file ^. sfname
  let uploader = unUserName $ file ^. userName
  let userName = mU ^. uName
  let privacyStatus = file ^. public
  let privacyDir = "/u/m/privacy/" ++ file ^. sfname

  H.div ! A.id "uploadInfo" $ do
    H.div ! A.id "dl-counter-user" $ do
      if(counter /= 1)
        then H.p (H.toHtml $ show counter ++ " downloads")
        else H.p (H.toHtml $ show counter ++ " download")

    H.p $ a ! href (toValue dlLink) $ H.toHtml fileName
    H.p (H.toHtml $ formatTime defaultTimeLocale "%H:%M - %a %Y.%m.%d" fileTime)
    H.p (H.toHtml $ infoLink)
    H.p (H.toHtml $ "Public :: " ++ show privacyStatus)

    if (userName == uploader)
      then do
        case privacyStatus of
          True -> do
            H.form ! action (toValue privacyDir)
                   ! A.method "post" $ do
              --privacy button
              H.div ! A.id "privacyButton" $ do
                input ! type_ "submit"
                      ! name "makePrivate"
                      ! value "-> Private"
          False -> do
            H.form ! action (toValue privacyDir)
                   ! A.method "post" $ do
              --privacy button
              H.div ! A.id "privacyButton" $ do
                input ! type_ "submit"
                      ! name "makePublic"
                      ! value "-> Public"
      else H.p ""


makePrivate :: FileUpload -> Maybe User -> Html
makePrivate f u = baseHtml (Just "privacy") $ do
  H.head $ do
    H.title "privacy changed"
  H.body $ do
    H.header $ mainHeader
    mainMenu u

    let fileName = f ^. fname
    let privacyStatus = f ^. public

    H.div ! A.id "user-info" $ do
      H.h2 "Privacy change"
      H.p (H.toHtml $ fileName)
      H.p (H.toHtml $ "Public :: " ++ show privacyStatus)

      if not privacyStatus
        then do
          H.p $ do "You can only view this file on your "
                   a ! href ("/u/m/uploads") $ "My Uploads"
                   " page"
        else do
          H.p $ do
            "This file now appears on the "
            a ! href ("/") $ "Recent Uploads"
            " list"
      H.br
      H.p $ do
        "Click "
        a ! href ("/") $ "here"
        " to return to the home page"

