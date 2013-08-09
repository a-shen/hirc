{-# LANGUAGE FlexibleInstances
           , FlexibleContexts
           , MultiParamTypeClasses
           , OverloadedStrings #-}
module Hirc.Controllers where

import           Data.Bson hiding (at, merge, include)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.List hiding (insert, find)
import           Data.Ord
import qualified Data.Text as T
import           Data.Maybe
import           Data.Aeson (decode, encode, toJSON)
import           Data.List.Split

import           Text.Blaze.Html5 hiding (select, map)

import           Control.Monad
import           Debug.Trace

import           LIO
import           LIO.DCLabel
import           LIO.Concurrent

import           Hails.Data.Hson (ObjectId, labeledRequestToHson)
import           Hails.Database hiding (delete)
import           Hails.Database.Structured
import           Hails.HttpServer.Types hiding (Query)
import           Hails.Web
import           Hails.Web.REST (RESTController)
import qualified Hails.Web.REST as REST
import qualified Hails.Web.Frank as F

import           Network.HTTP.Types hiding (Query)

import           Hirc.Policy
import           Hirc.Models
import           Hirc.Views

server :: Application
server = mkRouter $ do
  --routeName "channels" channelsController
  routeVar "chanId" $ do
    routeName "chats" chatsController

  F.get "/channels" $ withUserOrDoAuth $ \usr -> do
    liftLIO $ withHircPolicy $ checkUser usr
    chans <- liftLIO . withHircPolicy $ trace "here" $ 
      findAll $ select [] "channels"
    trace ("channels: " ++ (show chans)) $
      return . respondHtml "Channels" $ indexChannels chans usr

  F.get "/channels/new" $ withUserOrDoAuth $ \usr -> do
    liftLIO $ withHircPolicy $ checkUser usr
    allusers <- liftLIO $ withHircPolicy $ findAllUsers $ select [] "users"
    return $ respondHtml "New Channel" $ newChannel usr allusers

  F.post "/channels" $ withUserOrDoAuth $ \usr -> do
    ldoc <- request >>= labeledRequestToHson
    doc <- liftLIO $ unlabel ldoc
    let admins = map T.pack $ splitOn ("," :: String) ("admins" `at` doc)
        newdoc = merge [ "mems"   -: ([] :: [UserName])
                       , "admins" -: (admins :: [UserName]) ] 
                       doc
    liftLIO . withHircPolicy $ insert "channels" newdoc
    return $ redirectTo "/channels"

  F.get "/:chanId/edit" $ withUserOrDoAuth $ \usr -> do
    liftLIO $ withHircPolicy $ checkUser usr
    (Just sid) <- queryParam "chanId"
    let cid = read (S8.unpack sid) :: ObjectId
    mchan <- liftLIO $ withHircPolicy $ findWhere $ select ["_id" -: cid] "channels"
    case mchan of
      Just chan -> do
        allusers <- liftLIO $ withHircPolicy $ findAllUsers $ select [] "users"
        return $ respondHtml "Edit Channel" $ editChannel usr chan allusers
      _ -> return $ notFound
    
  F.post "/channels/edit" $ withUserOrDoAuth $ \usr -> trace "post channels/edit" $ do
    ldoc <- request >>= labeledRequestToHson
    doc <- liftLIO $ unlabel ldoc
    let admins = map T.pack $ splitOn ("," :: String) ("admins" `at` doc)
        id = read ("_id" `at` doc) :: ObjectId
        newdoc = merge [ "admins" -: (admins :: [UserName])
                       , "_id"    -: id] 
                       doc
    liftLIO $ withHircPolicy $ trace "saving doc" $ save "channels" newdoc
    trace "saved doc" $ return $ redirectTo "/channels"

  F.get "/:chanId/users" $ trace "getting users" $ do
    (Just sid) <- queryParam "chanId"
    let cid = read (S8.unpack sid) :: ObjectId
    mchan <- liftLIO $ withHircPolicy $ findWhere $ select ["_id" -: cid] "channels"
    let users = case mchan of
                  Just chan -> map T.unpack (channelMems chan)
                  _ -> []
    matype <- requestHeader "accept"
    case matype of
      Just atype |  "application/json" `S8.isInfixOf` atype ->
        return $ ok "application/json" (encode $ toJSON users)
      _ -> return $ okHtml ""

  -- Add user to the channel's member list. Username is the "user" field in the form.
  F.post "/:chanid/adduser" $ do
    (Just sid) <- queryParam "chanid"
    let cid = read (S8.unpack sid) :: ObjectId
    mlcdoc <- liftLIO $ withHircPolicy $ findOne $ select ["_id" -: cid] "channels"
    cdoc <- liftLIO $ unlabel $ fromJust mlcdoc
    reqdoc <- request >>= labeledRequestToHson >>= (liftLIO . unlabel)
    trace ("reqdoc: " ++ show reqdoc) $ return ()
    let newmem = ("user" `at` reqdoc) :: UserName
    let newmems = joinList newmem ("mems" `at` cdoc)
    let newdoc = merge ["mems" -: newmems] cdoc
    liftLIO $ withHircPolicy $ trace "saving channel doc" $ save "channels" newdoc
    trace "saved channel doc" $ return $ redirectTo "/channels"
 
  -- Remove user from the channel's member list. Username is the "user" field in the form.
  F.post "/:chanid/remuser" $ trace "REMOVE USER CALLED" $ do
    (Just sid) <- queryParam "chanid"
    let cid = read (S8.unpack sid) :: ObjectId
    mlcdoc <- liftLIO $ withHircPolicy $ findOne $ select ["_id" -: cid] "channels"
    cdoc <- liftLIO $ unlabel $ fromJust mlcdoc
    reqdoc <- request >>= labeledRequestToHson >>= (liftLIO . unlabel)
    trace ("reqdoc: " ++ show reqdoc) $ return ()
    let curmem = ("user" `at` reqdoc) :: UserName
    let newmems = delete curmem ("mems" `at` cdoc)
    let newdoc = merge ["mems" -: newmems] cdoc
    liftLIO $ withHircPolicy $ trace "saving channel doc" $ save "channels" newdoc
    trace "saved channel doc" $ return $ redirectTo "/channels"


chatsController :: RESTController
chatsController = do
  REST.index $ withUserOrDoAuth $ \usr -> do
    liftLIO $ withHircPolicy $ checkUser usr
    listChats usr

  REST.create $ withUserOrDoAuth $ \usr -> do
    ldoc <- request >>= labeledRequestToHson
    liftLIO . withHircPolicy $ insert "chats" ldoc
    listChats usr

listChats usr = trace "listChats" $ do
  (Just sid) <- queryParam "chanId"
  let str = S8.unpack sid  -- chanId as a string
      chanId = read (S8.unpack sid) :: ObjectId
  allChats <- liftLIO . withHircPolicy $ findAll $ select [] "chats"
  users <- liftLIO . withHircPolicy $ findAllUsers $ select [] "users"
  let sorted = sortBy (comparing (timestamp . fromJust . chatId)) allChats
      cchats = filter (\c -> (chatAssocChan c) == chanId) allChats
      len = length cchats
      maxchats = 100
      chats = if len <= maxchats
                then cchats
                else drop (len - maxchats) cchats -- show first (maxchats) chats on page
  --trace ("filtered chats: " ++ (show (chats :: [Chat]))) $ do
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
          _ -> return $ respondHtml cname $ showChatPage chats usr channel $ map T.unpack users
    _ -> return notFound

checkUser :: UserName -> DBAction ()
checkUser uname = do  -- add user to db if it's not already in there
  let username = (T.unpack uname) :: String
  usr <- liftLIO $ withHircPolicy $ findAllUsers $ select ["name" -: username] "users"
  case usr of
    [] -> do
      let udoc = ["name" -: (username :: String)] :: HsonDocument
      insert "users" udoc
      return ()
    _ -> return ()

findAllUsers :: Query -> DBAction [UserName]
findAllUsers q = do
  cur <- find q
  getAll cur []
  where getAll cur list = do
          mldoc <- next cur
          case mldoc of
            Nothing -> return list
            Just ldoc -> do
              doc <- liftLIO $ unlabel ldoc
              let username = "name" `at` doc
              getAll cur (list ++ [username])

joinList :: UserName -> [UserName] -> [UserName]
joinList e list = if e `elem` list
                then list
                else list ++ [e]


      --lrec <- fromLabeledDocument ldoc
      --saveLabeledRecord (lrec :: DCLabeled Channel)
