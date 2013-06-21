{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , MultiParamTypeClasses
           , OverloadedStrings #-}
module Hirc.Controllers where

import           Data.Bson
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.List hiding (insert)
import           Data.Ord
import qualified Data.Text as T
import           Data.Maybe
import           Data.Aeson (decode, encode, toJSON)

import           Text.Blaze.Html5 hiding (select)

import           Control.Monad
import           Debug.Trace

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

import           Hirc.Policy
import           Hirc.Models
import           Hirc.Views

server :: Application
server = mkRouter $ do
  routeName "channels" channelsController
  routeVar "chanId" $ do
    routeName "chats" chatsController
    --routeName "auth" authController

channelsController :: RESTController
channelsController = do
  REST.index $ withUserOrDoAuth $ \usr -> trace "channels.index" $ do
    chans <- liftLIO . withHircPolicy $ trace "here" $ 
      findAll $ select [] "channels"
    trace ("channels: " ++ (show chans)) $
      return . respondHtml "Channels" $ indexChannels chans

  REST.new $ withUserOrDoAuth $ \usr -> trace "channels.new" $ do
    return $ respondHtml "New Channel" $ newChannel
    
  REST.create $ withUserOrDoAuth $ \usr -> trace "channels.create" $ do
    let ctype = "text/json"
        respJSON403 msg = Response status403 [(hContentType, ctype)] $
                           L8.pack $ "{ \"error\" : " ++
                                       show (msg :: String) ++ "}"
    ldoc <- request >>= labeledRequestToHson
    liftLIO . withHircPolicy $ insert "channels" ldoc
    return $ redirectTo "/channels"
 
   
chatsController :: RESTController
chatsController = do
  REST.index $ withUserOrDoAuth $ \usr -> trace "chats.index" $ do
    sid <- queryParam "chanId"
    listChats usr

  REST.create $ withUserOrDoAuth $ \usr -> trace "chats.create" $ do
    let ctype = "text/json"
        respJSON403 msg = Response status403 [(hContentType, ctype)] $
                           L8.pack $ "{ \"error\" : " ++
                                       show (msg :: String) ++ "}"
    ldoc <- request >>= labeledRequestToHson
    liftLIO . withHircPolicy $ insert "chats" ldoc
    listChats usr

listChats usr = trace "listChats" $ do
  sid <- queryParam "chanId"
  let str = S8.unpack $ fromJust sid  -- chanId id as a string
  let chanId = read (S8.unpack $ fromJust sid) :: ObjectId
  allChats <- liftLIO . withHircPolicy $ findAll $ select [] "chats"
  let sorted = sortBy (comparing (timestamp . fromJust . chatId)) allChats
  let cchats = filter (\c -> (chatAssocChan c) == chanId) allChats
  let chats = filter (\c -> (chatAssocChan c) == chanId) allChats --todo
  trace ("found chats: " ++ (show chats)) $ do
    mchannel <- liftLIO . withHircPolicy $
      findWhere $ select ["_id" -: chanId] "channels"
    case mchannel of
      Just channel -> do
        let cname = toHtml $ channelName channel
        trace ("channel: " ++ (channelName $ fromJust mchannel)) $ do
          matype <- requestHeader "accept"
          case matype of
            Just atype |  "application/json" `S8.isInfixOf` atype ->
              return $ ok "application/json" (encode $ toJSON chats)
            _ -> return $ respondHtml cname $ showChatPage chats usr channel
      _ -> return notFound

