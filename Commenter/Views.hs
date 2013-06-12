
{-# LANGUAGE OverloadedStrings #-}

module Commenter.Views where

import           Prelude hiding (div, span, head, id)
import           LBH.MP
import           LBH.Utils
import		 Debug.Trace

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
import           Text.Regex
import		 Data.String.Utils
import           Hails.Web hiding (body)
import           Hails.HttpServer.Types
import           Text.Blaze.Html5 hiding (Tag, map)
import           Text.Blaze.Html5.Attributes hiding ( label, form, span
                                                    , title, style )
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html.Renderer.String as SR

--import           LBH.ActiveCode ( extractActieCodeBlocks -- not a typo
                                --, activeCodeToInactiveBlocks ) 
import		 LBH.Views
import		 Commenter.Models

url :: String
--url = ""
url = "https://www.comments.learnbyhacking.org"

showPage :: [Comment] -> UserName -> B.ObjectId -> Html
showPage comments user pid = trace "showPage " $ do
  li ! id "username" $ toHtml user
  script ! src "http://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.js" $ ""
  script ! src "http://code.jquery.com/jquery-1.10.1.min.js" $ ""
  script ! src "/static/js/comments.js" $ ""
  newComment user pid Nothing -- show form for making new comment
  indexComments comments pid user -- index all comments

indexComments :: [Comment] -> B.ObjectId -> UserName -> Html
indexComments coms pid user = trace "indexComments" $ do
  let comments = sortBy (comparing (B.timestamp . fromJust . commentId)) coms
  ul ! id "root" $ do
    forM_ comments $ \c -> do
      if (commentAssocPost c) == pid
        then case (commentInReplyTo c) of
          Nothing -> do
            let divid = toValue $ show $ fromJust $ commentId c
            div ! class_ "comment" ! id divid $ do
              h6 $ "line break"
              showComment c user
              showAllReplies c comments user
              button ! class_ "reply-button" $ "Reply"
          Just reply -> ""  -- it'll be taken care of in showAllReplies
        else ""

newComment :: UserName -> B.ObjectId -> Maybe B.ObjectId -> Html
newComment username postId mparent = trace "newComment" $ do
  let act = (url ++ "/" ++ (show postId) ++ "/comments")
  let pid = toValue $ show postId
  trace "form" $ form ! id "commentForm" ! action (toValue act) ! method "POST" $ do
    input ! type_ "hidden" ! name "author" ! id "author" ! value (toValue username)
    input ! type_ "hidden" ! name "post" ! id "post" ! value pid
    input ! type_ "hidden" ! name "parent" ! value ""
    div $ do
      label ! for "text" $ b $ "Post a comment"
      textarea ! type_ "text" ! name "text" ! id "text" $ ""
    p $ input ! type_ "submit" ! value "Post"

showComment :: Comment -> UserName -> Html
showComment comment user = do
  let cid = commentId comment
  --let ltime = show $ utcToLocalTime (utc) $ B.timestamp $ fromJust cid
  let ltime = show $ utcToLocalTime (tz) $ B.timestamp $ fromJust cid
  --let text = subRegex (mkRegex "1") (commentText comment) "<br>"
  h3 $ toHtml $ commentAuthor comment
  p $ toHtml $ take ((length ltime) - 3) ltime
  blockquote $ toHtml $ (commentText comment)

showAllReplies :: Comment -> [Comment] -> UserName -> Html
showAllReplies comment allComments user = do
  let cid = commentId comment
  forM_ allComments $ \c -> do
    if ((commentInReplyTo c) == cid)
      then ul $ showComment c user
      else "" -- do nothing

tz :: TimeZone
tz = TimeZone { timeZoneMinutes = -420,
                timeZoneSummerOnly = True, 
                timeZoneName = "PDT" }
--tz = do
  --timezone <- getCurrentTimeZone
  --timezone

showFrame :: String -> Html
showFrame pid = do
  let path = url ++ "/" ++ pid ++ "/comments"
  --iframe ! src (toValue path) ! width "1000" ! height "500" $ ""
  iframe ! src (toValue path) ! A.style "overflow: scroll;" ! width "1000" ! height "500" $ ""

respondHtml muser content = okHtml $ renderHtml $ docTypeHtml $ do
  head $ do
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

