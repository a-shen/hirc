{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , MultiParamTypeClasses
           , OverloadedStrings #-}
module Hirc.Controllers where

import           Data.Bson hiding (at)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.List hiding (insert, find)
import           Data.Ord
import qualified Data.Text as T
import           Data.Maybe
import           Data.Aeson (decode, encode, toJSON)

import           Text.Blaze.Html5 hiding (select, map)

import           Control.Monad
import           Debug.Trace

import           LIO
import           LIO.DCLabel
import           LIO.Concurrent

import           Hails.Data.Hson (ObjectId, labeledRequestToHson)
import           Hails.Database
import           Hails.Database.Structured
import           Hails.HttpServer.Types hiding (Query)
import           Hails.Web
import           Hails.Web.REST (RESTController)
import qualified Hails.Web.REST as REST
import qualified Hails.Web.Frank as Frank

import           Network.HTTP.Types hiding (Query)

import           Hirc.Policy
import           Hirc.Models
import           Hirc.Views

server :: Application
server = mkRouter $ do
  routeName "channels" channelsController
  routeVar "chanId" $ do
    routeName "chats" chatsController

channelsController :: RESTController
channelsController = do
  REST.index $ withUserOrDoAuth $ \usr -> trace "channels.index" $ do
    liftLIO $ withHircPolicy $ checkUser usr
    chans <- liftLIO . withHircPolicy $ trace "here" $ 
      findAll $ select [] "channels"
    trace ("channels: " ++ (show chans)) $
      return . respondHtml "Channels" $ indexChannels chans usr

  REST.new $ withUserOrDoAuth $ \usr -> trace "channels.new" $ do
    liftLIO $ withHircPolicy $ checkUser usr
    return $ respondHtml "New Channel" $ newChannel usr

  REST.create $ withUserOrDoAuth $ \usr -> trace "channels.create" $ do
    let ctype = "text/json"
        respJSON403 msg = Response status403 [(hContentType, ctype)] $
                           L8.pack $ "{ \"error\" : " ++
                                       show (msg :: String) ++ "}"
    ldoc <- request >>= labeledRequestToHson
    liftLIO . withHircPolicy $ insert "channels" ldoc
    return $ redirectTo "/channels"

  REST.edit $ withUserOrDoAuth $ \usr -> trace "channels.edit" $ do
    liftLIO $ withHircPolicy $ checkUser usr
    sid <- queryParam "chanId"
    return $ respondHtml "Edit Channel" $ newChannel usr
    
  REST.update $ withUserOrDoAuth $ const $ do
    let ctype = "text/json"
        respJSON403 msg = Response status403 [(hContentType, ctype)] $
                           L8.pack $ "{ \"error\" : " ++
                                       show (msg :: String) ++ "}"
    ldoc <- request >>= labeledRequestToHson
    liftLIO . withHircPolicy $ do
      lrec <- fromLabeledDocument ldoc
      saveLabeledRecord (lrec :: DCLabeled Channel)
    return $ redirectTo "/channels"
 
   
chatsController :: RESTController
chatsController = do
  REST.index $ withUserOrDoAuth $ \usr -> trace "chats.index" $ do
    liftLIO $ withHircPolicy $ checkUser usr
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
  let str = S8.unpack $ fromJust sid  -- chanId as a string
  let chanId = read (S8.unpack $ fromJust sid) :: ObjectId
  allChats <- liftLIO . withHircPolicy $ findAll $ select [] "chats"
  userdocs <- liftLIO . withHircPolicy $ findAllD $ select [] "users"
  let users = map (\d -> "name" `at` d) userdocs
  trace ("found allChats") $ do
    let sorted = sortBy (comparing (timestamp . fromJust . chatId)) allChats
    let cchats = filter (\c -> (chatAssocChan c) == chanId) allChats
    let len = trace ("cchats: " ++ (show cchats)) $ length cchats
    let maxchats = 100
    let chats = if len <= maxchats
                  then cchats
                  else drop (len - maxchats) cchats -- show first (maxchats) chats on page
    trace ("filtered chats: " ++ (show (chats :: [Chat]))) $ do
      mchannel <- liftLIO . withHircPolicy $
        findWhere $ select ["_id" -: chanId] "channels"
      case mchannel of
        Just channel -> do
          let cname = toHtml $ channelName channel
          trace ("channel: " ++ (channelName $ fromJust mchannel)) $ do
            matype <- requestHeader "accept"
            case matype of
              Just atype |  "application/json" `S8.isInfixOf` atype ->
                return $ ok "application/json" (encode $ toJSON (chats :: [Chat]))
              _ -> return $ respondHtml cname $ showChatPage chats usr channel users
        _ -> return notFound

checkUser :: UserName -> DBAction ()
checkUser uname = do  -- add user to db if it's not already in there
  let username = (T.unpack uname) :: String
  usr <- liftLIO $ withHircPolicy $ findAllD $ select ["name" -: username] "users"
  case usr of
    [] -> do
      let udoc = ["name" -: (username :: String)] :: HsonDocument
      insert "users" udoc
      return ()
    _ -> return ()

findAllD :: Query -> DBAction [HsonDocument]
findAllD q = do
  cur <- find q
  getAll cur []
  where getAll cur list = do
          mldoc <- next cur
          case mldoc of
            Nothing -> return list
            Just ldoc -> do
              doc <- liftLIO $ unlabel ldoc
              getAll cur (list ++ [doc])

