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

  F.get "/" $ withUserOrDoAuth $ \usr -> do
    return $ redirectTo "/channels"

  F.get "/channels" $ withUserOrDoAuth $ \usr -> do
    liftLIO $ withHircPolicy $ checkUser usr
    chans <- liftLIO . withHircPolicy $ trace "here" $ 
      findAll $ select [] "channels"
    trace ("channels: " ++ (show chans)) $
      return . respondHtml "Channels" $ indexChannels chans usr

  F.get "/channels/new" $ withUserOrDoAuth $ \usr -> do
    return $ respondHtml "New Channel" $ newChannel usr

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
    (Just sid) <- queryParam "chanId"
    let cid = read (S8.unpack sid) :: ObjectId
    mchan <- liftLIO $ withHircPolicy $ findWhere $ select ["_id" -: cid] "channels"
    case mchan of
      Just chan -> do
        return $ respondHtml "Edit Channel" $ editChannel usr chan
      _ -> return $ notFound
    
  F.post "/channels/edit" $ withUserOrDoAuth $ \usr -> trace "post channels/edit" $ do
    ldoc <- request >>= labeledRequestToHson
    doc <- liftLIO $ unlabel ldoc
    let admins = map T.pack $ splitOn ("," :: String) ("admins" `at` doc)
        id = read ("_id" `at` doc) :: ObjectId
        newdoc = merge [ "admins" -: (admins :: [UserName])
                       , "_id"    -: id ] 
                       doc
    liftLIO $ withHircPolicy $ trace "saving doc" $ save "channels" newdoc
    trace "saved doc" $ return $ redirectTo "/channels"

  F.get "/:chanId/users" $ withUserOrDoAuth $ \usr -> trace "getting users" $ do
    (Just sid) <- queryParam "chanId"
    let cid = read (S8.unpack sid) :: ObjectId
    mchan <- liftLIO $ withHircPolicy $ findWhere $ select ["_id" -: cid] "channels"
    let users = case mchan of
                  Just chan -> map T.unpack $ filter (/= usr) $ channelMems chan
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
  F.post "/:chanId/remuser" $ trace "REMOVE USER CALLED" $ do
    (Just sid) <- queryParam "chanId"
    let cid = read (S8.unpack sid) :: ObjectId
    mlcdoc <- liftLIO $ withHircPolicy $ findOne $ select ["_id" -: cid] "channels"
    cdoc <- liftLIO $ unlabel $ fromJust mlcdoc
    reqdoc <- request >>= labeledRequestToHson >>= (liftLIO . unlabel)
    trace ("reqdoc: " ++ show reqdoc) $ return ()
    let curmem = ("user" `at` reqdoc) :: UserName
    --mcurmem <- getHailsUser
    --let curmem = fromJust mcurmem
    let newmems = delete curmem ("mems" `at` cdoc)
    let newdoc = merge ["mems" -: newmems] cdoc
    liftLIO $ withHircPolicy $ trace "saving channel doc" $ save "channels" newdoc
    trace "saved channel doc" $ return $ redirectTo "/channels"
    
  -- Edit username
  F.post "/:chanId/edituser" $ withUserOrDoAuth $ \usr -> trace "post /edituser" $ do
    (Just sid) <- queryParam "chanId"
    let cid = read (S8.unpack sid) :: ObjectId
    mlcdoc <- liftLIO $ withHircPolicy $ findOne $ select ["_id" -: cid] "channels"
    cdoc <- liftLIO $ unlabel $ fromJust mlcdoc
    reqdoc <- request >>= labeledRequestToHson >>= (liftLIO . unlabel)
    let newname = ("newname" `at` reqdoc) :: UserName
        oldname = ("oldname" `at` reqdoc) :: UserName
        newmems = replaceList oldname newname ("mems" `at` cdoc)
        newcdoc = ["mems" -: newmems] `merge` cdoc
    mludoc <- liftLIO $ withHircPolicy $ findOne $ select ["name" -: usr] "users"
    udoc <- liftLIO $ unlabel $ fromJust mludoc
    let newudoc = ["name" -: newname] `merge` udoc
    liftLIO $ withHircPolicy $ do
      save "channels" newcdoc
      save "users" newudoc
    matype <- requestHeader "accept"
    case matype of
      Just atype |  "application/json" `S8.isInfixOf` atype ->
        return $ ok "application/json" (encode $ toJSON $ T.unpack newname)
      _ -> return $ redirectTo ("/channels/" ++ S8.unpack sid)


chatsController :: RESTController
chatsController = do
  REST.index $ withUserOrDoAuth $ \usr -> do
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
  let sorted = sortBy (comparing (timestamp . fromJust . chatId)) allChats
      cchats = filter (\c -> (chatAssocChan c) == chanId) allChats
      len = length cchats
      maxchats = 100
      chats = if len <= maxchats
                then cchats
                else drop (len - maxchats) cchats -- show first (maxchats) chats on page
  --trace ("filtered chats: " ++ (show (chats :: [Chat]))) $ do
  mlchandoc <- liftLIO $ withHircPolicy $ findOne $ select ["_id" -: chanId] "channels"
  case mlchandoc of
    Just lchandoc -> do
      chandoc <- liftLIO $ unlabel lchandoc
      channel <- fromDocument chandoc
      matype <- requestHeader "accept"
      case matype of
        Just atype |  "application/json" `S8.isInfixOf` atype ->
          return $ ok "application/json" (encode $ toJSON (chats :: [Chat]))
        _ -> do
          let cname = toHtml $ channelName channel
          return $ respondHtml cname $ showChatPage chats usr channel
    _ -> return notFound

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

replaceList :: UserName -> UserName -> [UserName] -> [UserName]
replaceList old new list = (filter (\e -> e /= old) list) ++ [new]

joinList :: UserName -> [UserName] -> [UserName]
joinList e list =
  if e `elem` list
  then list
  else list ++ [e]

