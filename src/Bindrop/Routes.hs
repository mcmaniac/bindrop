module Bindrop.Routes where

import Control.Monad
import Control.Monad.IO.Class
import Control.Lens.Operators

import Data.Acid
import Data.Acid.Advanced ( query' )

import Happstack.Server
import Happstack.Server.ClientSession

import Bindrop.Session
import Bindrop.State
import Bindrop.State.Journal
import Bindrop.Responses.IndexResponses
import Bindrop.Responses.FileResponses
import Bindrop.Responses.UserResponses

import HTML.About
import HTML.Login
import HTML.Register
import qualified HTML.Error as Error

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" (10*10^(6 :: Int)) 1000 1000)

mainRoute :: AcidState BindropState
  -> ClientSessionT SessionData (ServerPartT IO) Response
mainRoute acid = do
  s <- getSession
  let mU = s ^. sessionUser

  j <- query' acid RetrieveJournal

  do decodeBody myPolicy
     msum [ indexPart mU acid -- update index with file uploads

          , do -- the "/" index page
            nullDir
            indexMostRecent acid mU

          , do -- to view download info
            dir "f" $ path $ \fileName -> fpart acid mU fileName

          , do -- to download:
            dir "s" $ path $ \fileName -> spart acid fileName

          , do -- login
            dirs "u/login" $ loginPart acid

          , do -- logout
            dirs "u/logout" $ logoutPart mU

          , do -- process reset password request
            dirs "u/lost-password/r" $ resetPassPart acid mU

          , do -- lost password reset form
            dirs "u/lost-password/f" $ lostPassPart acid mU

          , do --lost password initial page (asks for username)
            dirs "u/lost-password" $ ok $ toResponse $ lostPassInitial mU

          , do -- process registration
            dirs "u/r/process" $ uRegisterPart acid

          , do -- user registration page
            dirs "u/r" $ ok $ toResponse $ register mU

          , do -- password change
            dirs "u/m/p" $ changePassPart acid mU

          , do -- user specific uploads
            dirs "u/m/uploads" $ myUploadsPart acid mU

          , do -- make a file private via button
            dirs "u/m/privacy" $ path $ \fileName -> makePrivatePart acid fileName --lmao private part

          , do -- my account page
            dirs "u/m" $ myAcctPart mU

          , do -- user login page
            dir "u" $ ok $ toResponse $ loginPage mU

          , do -- about page
            dir "a" $ ok $ toResponse $ about mU j

          , do -- server files from "/static/"
            dir "static" $ serveDirectory DisableBrowsing [] "static"

          , do -- anything else could not be found
            notFound $ toResponse Error.notFound
          ]


