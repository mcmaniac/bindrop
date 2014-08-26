{-# LANGUAGE OverloadedStrings #-}

module HTML.File where

import Control.Lens.Operators
import Control.Lens.Iso ( non )
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
  let dlLink   = "/s/" ++ file ^. sfname
  let infoLink = "localhost:8082/f/" ++ file ^. sfname

  H.div ! A.id "uploadInfo" $ do
    H.p $ do
      a ! href (toValue dlLink) $ H.toHtml fileName
    H.p (H.toHtml $ show fileTime)
    H.p (H.toHtml $ infoLink)

uploadedFile :: User -> FileUpload -> Html
uploadedFile mU file = toHtml $ do
  let fileName = file ^. fname
  let fileTime = file ^. uploadTime
  let dlLink   = "/s/" ++ file ^. sfname
  let infoLink = "localhost:8082/f/" ++ file ^. sfname
  let uploader = unUserName $ file ^. userName
  let userName = mU ^. uName
  let privacyStatus = file ^. public
  let privacyDir = "/mp/" ++ file ^. sfname

  H.div ! A.id "uploadInfo" $ do
    H.ul $ do
      H.li $ a ! href (toValue dlLink) $ H.toHtml fileName
      H.li (H.toHtml $ show fileTime)
      H.li (H.toHtml $ infoLink)
      H.li (H.toHtml $ "Public :: " ++ show privacyStatus)

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
                      ! value "Make Private"
          False -> do
            H.form ! action (toValue privacyDir)
                   ! A.method "post" $ do
              --privacy button
              H.div ! A.id "privacyButton" $ do
                input ! type_ "submit"
                      ! name "makePublic"
                      ! value "Make Public"
      else H.p ""


makePrivate :: FileUpload -> Maybe User -> Html
makePrivate f u = baseHtml $ do
  H.head $ do
    H.title "privacy changed"
  H.body $ do
    H.header $ mainHeader
    mainMenu u

    let fileName = f ^. fname
    let privacyStatus = f ^. public

    H.div ! A.id "user-info" $ do
      H.h2 "Privacy change"
      H.p (H.toHtml $ "Your file: " ++ fileName)
      H.p (H.toHtml $ " Now has the public status: " ++ show privacyStatus)

      if not privacyStatus
        then do
          H.p $ do "You can only view this file on your "
                   a ! href ("/mu") $ "My Uploads"
                   " page"
        else do
          H.p $ do "This file now appears on the "
                   a ! href ("/") $ "Recent Uploads"
                   " list"

      H.p $ do "Click "
               a ! href ("/") $ "here"
               " to return to the home page"

