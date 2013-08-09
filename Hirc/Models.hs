{-# LANGUAGE OverloadedStrings
           , DeriveGeneric  #-}
module Hirc.Models ( Channel(..)
                   , Chat(..) 
                   ) where

import           Prelude hiding (lookup)

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8
import		 Data.Aeson
import           Debug.Trace

import           GHC.Generics

import           Hails.Data.Hson
import           Hails.Web
import           Hails.Database
import           Hails.Database.Structured

data Chat = Chat
    { chatId        :: Maybe ObjectId
    , chatAuthor    :: UserName
    , chatAssocChan :: ObjectId -- what channel it belongs to
    , chatText      :: String
    } deriving Show

instance ToJSON Chat where
  toJSON (Chat i a p t) = 
    object [ "_id"    .= (show $ fromJust i)
           , "author" .= a
           , "chan"   .= (show p)
           , "text"   .= t
           ]

instance DCRecord Chat where
  fromDocument doc = do
    let cid = lookupObjId "_id" doc
    author <- lookup "author" doc
    let chan = lookupObjId "chan" doc
    text <- lookup "text" doc
    return Chat { chatId = cid
                , chatAuthor = author
                , chatAssocChan = fromJust chan
                , chatText = text }

  toDocument c =
    [ "_id"  -: chatId c
    , "author" -: chatAuthor c
    , "chan" -: chatAssocChan c
    , "text" -: chatText c ]

  recordCollection _ = "chats"

data Channel = Channel
    { channelId     :: Maybe ObjectId
    , channelName   :: String
    , channelMems   :: [UserName]  -- who's currently in this chat room
    , channelAdmins :: [UserName]
    , channelListed :: String
    } deriving Show

instance DCRecord Channel where
  fromDocument doc = do
    let cid = lookupObjId "_id" doc
    name <- lookup "name" doc
    mems <- lookup "mems" doc
    admins <- lookup "admins" doc
    listed <- lookup "listed" doc
    return Channel { channelId     = cid
                   , channelName   = name
                   , channelMems   = mems
                   , channelAdmins  = admins
                   , channelListed = listed }

  toDocument c = trace "toDoc" $
    [ "_id"    -: channelId c
    , "name"   -: channelName c
    , "mems"   -: channelMems c
    , "admins" -: channelAdmins c
    , "listed" -: channelListed c ]

  recordCollection _ = "channels"

instance ToJSON Channel where
  toJSON (Channel i n m a l) = 
    object [ "_id"    .= (show $ fromJust i)
           , "name"   .= n
           , "mems"   .= m
           , "admins" .= a
           , "listed" .= l
           ]


lookupObjId :: Monad m => FieldName -> HsonDocument -> m ObjectId
lookupObjId n d = case lookup n d of
    Just i -> return (i :: ObjectId)
    _ -> case do { s <- lookup n d; maybeRead s } of
          Just i -> return i
          _ -> fail $ "lookupObjId: cannot extract id from " ++ show n
  where maybeRead = fmap fst . listToMaybe . reads

