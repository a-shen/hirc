{-# LANGUAGE OverloadedStrings #-}

module Hirc.Views where

import           Prelude hiding (div, span, head, id)

import           Control.Monad
import           Data.Maybe
import           Data.Ord
import           Data.List hiding (head)
import qualified Data.Bson as B
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Monoid (mempty)
import           Data.Time
import		 Data.String.Utils
import           Debug.Trace

import           Hails.Web hiding (body)
import           Hails.HttpServer.Types

import           Text.Blaze.Html5 hiding (Tag, map)
import           Text.Blaze.Html5.Attributes hiding ( label, form, span
                                                    , title, style )
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html.Renderer.String as SR
import           Text.Regex

import		 Hirc.Models


indexChannels :: [Channel] -> Html
indexChannels channels = do
  h1 $ "Channels"
  h3 ! href "/channels/new" $ "New Channel"
  ul $ do
    forM_ channels $ \chan -> do
      case (channelListed chan) of
        "on" -> do
          let cid = show $ fromJust $ channelId chan
          let link = toValue ("/" ++ cid ++ "/chats")
          li $ a ! href link $ toHtml $ channelName chan  
        _ -> ""

newChannel :: Html
newChannel = do
  let listExp = "The names of listed channels are displayed to the public and can be joined by anyone. Unlisted channels are only available to those who you choose to share the link with."
  h3 $ "Create a new channel"
  form ! action "/channels" ! method "POST" $ do
    div $ do
      label ! for "name" $ "Channel name:"
      input ! type_ "text" ! name "name" ! id "name"
    div $ do
      label ! for "listed" $ "Listed?"
      input ! type_ "checkbox" ! name "listed" ! id "published"
      p ! id "listexp" $ listExp  -- todo: make this gray in css
    p $ input ! type_ "submit" ! value "Create"

showChatPage :: [Chat] -> UserName -> B.ObjectId -> Html
showChatPage chats user chanId = trace "showChatPage" $ do
  li ! id "username" $ toHtml user
  script ! src "http://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.js" $ ""
  script ! src "http://code.jquery.com/jquery-1.10.1.min.js" $ ""
  --script ! src "/static/js/chats.js" $ ""
  --todo: add a script to append usernames to list of usernames
  trace "loaded scripts" $ do
    indexChats chats chanId user -- index all chats
    newChat user chanId -- show form for making new chat

indexChats :: [Chat] -> B.ObjectId -> UserName -> Html
indexChats cs chanId user = trace "indexChats" $ do
  let chats = sortBy (comparing (B.timestamp . fromJust . chatId)) cs
  trace ("indexing chats: " ++ (show chats)) $ div ! id "chats" $ do
    forM_ chats $ \c -> do
      if (chatAssocChan c) == chanId
        then p $ do
          let cid = fromJust $ chatId c
          let longt = show $ utcToLocalTime (pdt) $ B.timestamp cid
          let time = drop 11 longt  --show only hh:mm
          toHtml ("<" ++ time ++ ">  ")
          b $ toHtml ((T.unpack $ chatAuthor c) ++ ": ")
          toHtml $ chatText c
        else ""

newChat :: UserName -> B.ObjectId -> Html
newChat username chanId = trace "newChats" $ do
  let act = ("/" ++ (show chanId) ++ "/chats")
  let cid = toValue $ show chanId
  form ! id "chatForm" ! action (toValue act) ! method "POST" $ do
    input ! type_ "hidden" ! name "author" ! id "author" ! value (toValue username)
    input ! type_ "hidden" ! name "chan" ! id "chan" ! value cid
    div $ do
      input ! type_ "text" ! name "text" ! id "text"
    p $ input ! type_ "submit" ! id "chatsub" ! value "Post" --todo: hide this in css

pdt :: TimeZone
pdt = TimeZone { timeZoneMinutes = -420,
                 timeZoneSummerOnly = True, 
                 timeZoneName = "PDT" }

respondHtml ctitle content = okHtml $ renderHtml $ docTypeHtml $ do
  head $ do
    title ctitle
    stylesheet "/static/css/bootstrap.css"
    stylesheet "/static/css/application.css"
    script ! src "/static/js/jquery.min.js" $ ""
    script ! src "/static/js/jquery.cookie.js" $ ""
    script ! src "/static/js/bootstrap.min.js" $ ""
    script ! src "https://login.persona.org/include.js" $ ""
    script ! src "/static/js/application.js" $ ""
  body $ do
    script ! src "/static/js/jquery.js" $ ""
    script ! src "/static/js/bootstrap.js" $ ""
    content

stylesheet :: String -> Html
stylesheet uri = link ! rel "stylesheet" ! type_ "text/css" ! href (toValue uri)

