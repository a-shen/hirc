{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , MultiParamTypeClasses
           , OverloadedStrings #-}
module Commenter.Controllers where

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import           Data.Maybe
import		 Debug.Trace
import           Control.Monad
import           LIO
import           LIO.DCLabel
import           LIO.Concurrent
import           Hails.Data.Hson (ObjectId, labeledRequestToHson)
import           Hails.Database
import           Hails.Database.Structured
import           Hails.HttpServer.Types
import           Hails.Web
import           Hails.Web.REST (RESTController)
import qualified Hails.Web.REST as REST
import qualified Hails.Web.Frank as Frank
import           Network.HTTP.Types
import           Commenter.Policy
import           Commenter.Models
import           Commenter.Views
import           LBH.MP
--import           LBH.ActiveCode
import           Data.Aeson (decode, encode, toJSON)

server :: Application
server = mkRouter $ do
  routeVar "pid" $ do
    routeName "comments" commentController

commentController :: RESTController
commentController = do
  REST.index $ maybeRegister $ trace "REST.index" $ do
    mu <- currentUser
    sid <- queryParam "pid"
    let str = S8.unpack $ fromJust sid  -- post id as a string
    let pid = read str
    comments <- liftLIO . withCommentPolicy $ findAll $ select [] "comments"
    case mu of
      Just u -> do
        let username = userId $ fromJust mu
        return $ trace ("pid: " ++ str) $
          respondHtml "Comments" $ showPage comments username $ Just pid
      Nothing -> do
        let username = T.pack "Anonymous" --for now the persona stuff doesn't work, so people will just have to deal with being anonymous
        return $ trace ("pid: " ++ str) $ 
          respondHtml "Comments" $ showPage comments username $ Just pid

  REST.create $ withAuthUser $ const $ do --find out what const stands for
    ldoc <- request >>= labeledRequestToHson
    liftLIO . withCommentPolicy $ insert "comments" ldoc
    return $ redirectTo $ "/"

  --REST.delete

  --REST.update

--stolen from LBH
maybeRegister :: Controller Response -> Controller Response
maybeRegister ctrl = trace "maybeRegister" $ do
  muName <- getHailsUser
  musr   <- currentUser
  if isJust muName && isNothing musr
    then return $ redirectTo "/users/new"
    else ctrl

withAuthUser :: (User -> Controller Response) -> Controller Response
withAuthUser act = maybeRegister $ withUserOrDoAuth $ const $  do
  musr <- currentUser
  maybe (return serverError) act musr

currentUser :: Controller (Maybe User)
currentUser = do
  mu <- getHailsUser
  case mu of
    Nothing -> trace "user is Nothing" $ return Nothing
    Just u -> liftLIO $ withLBHPolicy $ do
      findBy "users" "email" u

