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
  li $ a ! href "/channels/new" $ "New Channel"
  ul $ do
    forM_ channels $ \chan -> do
      let cid = show $ fromJust $ channelId chan
      let link = toValue ("/" ++ cid ++ "/chats")
      li $ a ! href link $ toHtml $ channelName chan

newChannel :: Html
newChannel = do
  let pwdExp = "If you enter a password, only those with the password will be able to join your channel. Otherwise, your channel will be open to the public. Be sure to choose your password carefully, as it cannot be changed."
  h3 $ "Create a new channel"
  form ! action "/channels" ! method "POST" $ do
    div $ do
      label ! for "name" $ "Channel name:"
      input ! type_ "text" ! name "name" ! id "name"
    div $ do
      label ! for "pwd" $ "Password (optional)"
      input ! type_ "text" ! name "pwd" ! id "pwd"
      p ! id "pwdexp" $ pwdExp  -- todo: make this gray in css
    p $ input ! type_ "submit" ! value "Create"

showChatPage :: [Chat] -> UserName -> Channel -> Html
showChatPage chats user channel = trace "showChatPage" $ do
  let chanId = fromJust $ channelId channel
  li ! id "username" $ toHtml user
  script ! src "http://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.js" $ ""
  script ! src "http://code.jquery.com/jquery-1.10.1.min.js" $ ""
  --script ! src "/static/js/chats.js" $ ""
  --todo: add a script to append usernames to list of usernames
  trace "loaded scripts" $ do
    div ! id "chats" $ "" -- chats.js will index all chats
    newChat user chanId -- show form for making new chat

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

