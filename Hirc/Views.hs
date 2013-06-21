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


indexChannels :: [Channel] -> UserName -> Html
indexChannels channels username = do
  h1 $ "Channels"
  li $ a ! href "/channels/new" $ "New Channel"
  ul $ do
    forM_ channels $ \chan -> do
      if ((channelListed chan) == False) 
        then if ((channelAdmin chan) == username)
               then showChannel chan True
               else ""
        else showChannel chan False

showChannel :: Channel -> Bool -> Html
showChannel chan private = do
  let cid = show $ fromJust $ channelId chan
  let link = toValue ("/" ++ cid ++ "/chats")
  li $ a ! href link $ toHtml $
    case private of
      True -> toValue((channelName chan) ++ " (private)")
      _ -> toValue $ channelName chan

newChannel :: UserName -> Html
newChannel username = do
  {-
  let listedExp  = toHtml("Yes, put my channel on the hIRC channel " ++
                          "listings page so that anyone can join " ++
                          "my channel.")
  let ulistedExp = toHtml("No, don't list my channel; " ++ 
                          "I'll only share the channel's URL with " ++ 
                          "the people who I invite to join my channel.")
  -}
  let listedExp = "Yes, put my channel on the hIRC channel listings page so that anyone can join my channel." :: String
  let ulistedExp = "No, don't list my channel; I'll only share the channel's URL with the people who I invite to join my channel." :: String
  h3 $ "Create a new channel"
  form ! id "chanForm" ! action "/channels" ! method "POST" $ do
    input ! type_ "hidden" ! name "admin" ! value (toValue username)
    div $ do
      label ! for "name" $ "Channel name:"
      input ! type_ "text" ! name "name" ! id "name"
    div $ do
      label ! for "listed" $ "Listed?"
      input ! type_ "radio" ! name "listed" ! value "True" 
      toHtml listedExp
      input ! type_ "radio" ! name "listed" ! value "False" 
      toHtml ulistedExp
    p $ input ! type_ "submit" ! value "Create"

showChatPage :: [Chat] -> UserName -> Channel -> Html
showChatPage chats user channel = trace "showChatPage" $ do
  let chanId = fromJust $ channelId channel
  script ! src "http://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.js" $ ""
  script ! src "http://code.jquery.com/jquery-1.10.1.min.js" $ ""
  --script ! src "/static/js/chats.js" $ ""
  --div ! class_ "hidden" ! id "bookmark" $ 
  div ! id "chats" $ "" -- chats.js will index all chats
  script $ toHtml("$('#chats').append('" ++ (T.unpack user) ++ " has joined.');")
  newChat user chanId -- show form for making new chat

{-
reqPwd :: Channel -> Html
reqPwd channel = do
  let mpwd = channelPassword channel
  case mpwd of
    Nothing -> ""
    Just "" -> ""
    Just pwd -> do
      form ! id "pwdForm" ! method "POST" $ do
        label ! for "upwd" $ "Enter password:"
        input ! type_ "text" ! name "upwd" ! id "upwd" --upwd = user-entered password
        p $ input ! type_ "submit" ! value "Enter"
      script $ toHtml ("var pwd = " ++ pwd)
      script ! src "/static/js/auth.js" $ ""
-}

newChat :: UserName -> B.ObjectId -> Html
newChat username chanId = trace "newChats" $ do
  let act = ("/" ++ (show chanId) ++ "/chats")
  let cid = toValue $ show chanId
  form ! id "chatForm" ! action (toValue act) ! method "POST" $ do
    input ! type_ "hidden" ! name "author" ! id "author" ! value (toValue username)
    input ! type_ "hidden" ! name "chan" ! id "chan" ! value cid
    div $ do
      input ! type_ "text" ! name "text" ! id "text"
    p $ input ! type_ "submit" ! class_ "chatsub" ! value "Post"

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

