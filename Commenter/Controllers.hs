{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , MultiParamTypeClasses
           , OverloadedStrings #-}
module Commenter.Controllers where

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import           Data.Maybe

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

--import           LBH.ActiveCode
import           Data.Aeson (decode, encode, toJSON)


server :: Application
server = mkRouter $ do
  routeName "comments" commentController

commentController :: RESTController
commentController = do
  REST.index $ do
    comments <- liftLIO . withCommentPolicy $ findAll $ select [] "comments"
    return $ respondHtml $ showPage comments "currentUser" Nothing
    --return $ respondHtml $ showPage comments (currentUser) (postId)
    --find out how to add in the id of the post you're commenting on
    --find out how to get the username of the current user
 
  REST.create $ do
    ldoc <- request >>= labeledRequestToHson
    liftLIO . withCommentPolicy $ insert "comments" ldoc
    return $ redirectTo $ "/"

  --REST.delete

  --REST.update
