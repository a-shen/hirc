{-# LANGUAGE OverloadedStrings #-}

module Hirc.Views where

import           Prelude hiding (div, span, head, id, showList)

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
import           Data.String.Utils
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

import		     Hirc.Models


indexChannels :: [Channel] -> UserName -> Html
indexChannels channels username = trace "indexChannels" $ do
  h1 $ "Channels"
  a ! href "/channels/new" $ "New Channel"
  ul $ do
    forM_ channels $ \chan -> do
      case (channelListed chan) of
        "True" -> do
          let link = "/" ++ (show $ fromJust $ channelId chan) ++ "/chats"
          li $ a ! href (toValue link) $ toHtml $ channelName chan
        _ ->
          if (username `elem` (channelAdmins chan))
            then do  -- channel is not listed, but this is an admin
              let link = "/" ++ (show $ fromJust $ channelId chan) ++ "/chats"
              trace ("link: " ++ link) $ return ()
              li $ a ! href (toValue link) $ toHtml ((channelName chan) ++ " (private)")
            else ""

newChannel :: UserName -> Html
newChannel username = do
  h3 $ "Create a new channel"
  form ! id "chanForm" ! action "/channels" ! method "POST" $ 
    formChannel username Nothing

editChannel :: UserName -> Channel -> Html
editChannel username chan = do
  h3 $ "Edit your channel"
  form ! id "chanForm" ! action "/channels/edit" ! method "POST" $ 
    formChannel username (Just chan)

formChannel :: UserName -> Maybe Channel -> Html
formChannel username mchan = do
  let listedExp = ("Yes, put my channel on the hIRC channel listings page " ++ 
                  "so that anyone can join my channel.") :: String
  let ulistedExp = ("No, don't list my channel; I'll only share my channel's " ++ 
                   "URL with the people who I invite to join my channel.") :: String
  case mchan of
    Just chan -> do
      let id = fromJust $ channelId chan
      input ! type_ "hidden" ! name "_id" ! value (toValue $ show id)
      forM_ (channelMems chan) $ \mem ->
        input ! type_ "hidden" ! name "mems[]" ! value (toValue $ T.unpack mem)
      div $ do
        label ! for "name" $ "Channel name:"
        input ! type_ "text" ! name "name" ! value (toValue $ channelName chan)
      div $ do
        label ! for "admins" $
          "Enter a comma-separated list of channel admins (no spaces after commas)"
        input ! type_ "text" ! name "admins" ! value (toValue $ showList $ channelAdmins chan)
    Nothing -> do
      div $ do
        label ! for "name" $ "Channel name:"
        input ! type_ "text" ! name "name"
      div $ do
        label ! for "admins" $
          "Enter a comma-separated list of channel admins (no spaces after commas)"
        input ! type_ "text" ! name "admins"
  div $ do
    label ! for "listed" $ "Listed?"
    input ! type_ "radio" ! name "listed" ! value "True" 
    toHtml listedExp
    br
    input ! type_ "radio" ! name "listed" ! value "False" 
    toHtml ulistedExp
  p $ input ! type_ "submit" ! value "Create"

showChatPage :: [Chat] -> UserName -> Channel -> Html
showChatPage chats user chan = trace "showChatPage" $ do
  script ! src "http://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.js" $ ""
  script ! src "http://code.jquery.com/jquery-1.10.1.min.js" $ ""
  script ! src "/static/js/chats.js" $ ""
  let url = "/" ++ (show $ fromJust $ channelId chan)
  h2 $ toHtml ("Channel: " ++ channelName chan)
  if (user `elem` channelAdmins chan)
    then p $ a ! href (toValue (url ++ "/edit")) $ "Edit Channel"
    else ""
  form ! id "removeMemForm" ! action (toValue (url ++ "/remuser")) ! method "POST" $ do
    -- remove user from channel; form will be submitted when the user leaves the page
    input ! type_ "hidden" ! name "user" ! value (toValue $ T.unpack user)
  let chanId = fromJust $ channelId chan
  div ! class_ "row-fluid" $ do
    div ! id "chats" ! class_ "span6" $ "" -- chats.js will index all chats
    div ! id "users" $ h3 $ "Users in chat room:" -- chats.js will list users
  div ! id "currentUser" ! class_ "hidden" $ toHtml $ T.unpack user
  newChat user chanId -- show form for making new chat

newChat :: UserName -> B.ObjectId -> Html
newChat username chanId = trace "newChats" $ do
  let act = ("/" ++ (show chanId) ++ "/chats")
  let cid = toValue $ show chanId
  form ! id "chatForm" ! action (toValue act) ! method "POST" $ do
    input ! type_ "hidden" ! name "author" ! id "author" ! value (toValue username)
    input ! type_ "hidden" ! name "chan" ! id "chan" ! value cid
    div $ do
      input ! type_ "text" ! name "text" ! id "text"
    p $ input ! type_ "submit" ! id "newchatbttn" ! value "Post"

pdt :: TimeZone
pdt = TimeZone { timeZoneMinutes = -420,
                 timeZoneSummerOnly = True, 
                 timeZoneName = "PDT" }

respondHtml ctitle content = okHtml $ renderHtml $ docTypeHtml $ do
  head $ do
    title ctitle
    --stylesheet "/static/css/bootstrap.css"
    stylesheet "/static/css/bootstrap1.css"
    stylesheet "/static/css/application.css"
    script ! src "/static/js/jquery.min.js" $ ""
    script ! src "/static/js/jquery.cookie.js" $ ""
    script ! src "/static/js/bootstrap.min.js" $ ""
    script ! src "/static/js/application.js" $ ""
  body $ do
    script ! src "/static/js/jquery.js" $ ""
    script ! src "/static/js/bootstrap.js" $ ""
    content

stylesheet :: String -> Html
stylesheet uri = link ! rel "stylesheet" ! type_ "text/css" ! href (toValue uri)

showList :: [UserName] -> String
showList [] = ""
showList [a] = T.unpack a
showList (x:xs) = (showList [x]) ++ "," ++ showList xs


