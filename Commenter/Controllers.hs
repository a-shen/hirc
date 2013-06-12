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
import           LBH.Controllers
import           Data.Aeson (decode, encode, toJSON)

server :: Application
server = mkRouter $ do
  Frank.post "/users" usersCreate
  routeAll . personaLoginEmailToUid . mkRouter $ do
    routeVar "pid" $ do
      routeTop testController
      routeName "comments" commentController
    routeName "users" usersController

commentController :: RESTController
commentController = do
  REST.index $ maybeRegister $ trace "REST.index" $ index

  REST.create $ trace "REST.create" $ withAuthUser $ const $ trace "REST.create again" $ do
    let ctype = "text/json"
        respJSON403 msg = Response status403 [(hContentType, ctype)] $
                           L8.pack $ "{ \"error\" : " ++
                                       show (msg :: String) ++ "}"
    ldoc <- request >>= labeledRequestToHson
    liftLIO . withCommentPolicy $ insert "comments" ldoc
    index

index = trace "index called" $ do
  mu <- currentUser
  sid <- queryParam "pid"
  let str = S8.unpack $ fromJust sid  -- post id as a string
  let pid = read str -- post id as an ObjectId
  comments <- liftLIO . withCommentPolicy $ 
    findAll $ select ["post" -: pid] "comments"
  let username = case mu of
                   Just u -> userId u
                   Nothing -> "Anonymous"
  matype <- requestHeader "accept"
  case matype of
    Just atype |  "application/json" `S8.isInfixOf` atype ->
       return $ trace ("encoding to json; comments: " ++ (show comments)) $ ok "application/json" (encode $ toJSON comments)
    _ -> return $ trace "accepting html" $ respondHtml mu $ showPage comments username pid

testController :: Controller Response
testController = do
  mu <- currentUser
  sid <- queryParam "pid"
  let str = S8.unpack $ fromJust sid  -- post id as a string
  return $ respondHtml mu $ showFrame str

