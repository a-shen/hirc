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

showPage :: [Comment] -> UserName -> B.ObjectId -> Html
showPage comments user pid = trace "showPage " $ do
  script ! src "http://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.js" $ ""
  --script ! src "http://malsup.github.com/jquery.form.js" $ ""
  script ! src "http://code.jquery.com/jquery-1.10.1.min.js" $ ""
  script ! src "http://github.com/douglascrockford/JSON-js" $ ""
  --script ! src "/static/js/comments.js" $ ""
  --script ! src "/static/js/comments.old.js" $ ""
  --script ! src "/static/js/comments.mult.js" $ ""
  --script ! src "/static/js/tmp.js" $ ""
  trace ("pid:" ++ (show pid)) $ newComment user pid Nothing
  indexComments comments pid user

indexComments :: [Comment] -> B.ObjectId -> UserName -> Html
indexComments coms pid user = trace "indexComments" $ do
  let comments = reverse $ sortBy (comparing (B.timestamp . fromJust . commentId)) coms
  ul ! id "root" $ do
    forM_ comments $ \c -> do
      if (commentAssocPost c) == pid
        then case (commentInReplyTo c) of
        --case (commentInReplyTo c) of
          Nothing -> do
            showComment c user
            showAllReplies c comments user
          Just reply -> ""  -- it'll be taken care of in showAllReplies
        else ""

newComment :: UserName -> B.ObjectId -> Maybe B.ObjectId -> Html
newComment username postId mparent = trace "newComment" $ 
  createComment username postId mparent "Post a comment" "commentForm"

createComment :: UserName -> B.ObjectId -> Maybe B.ObjectId -> Html -> AttributeValue -> Html
--createComment :: UserName -> B.ObjectId -> Maybe B.ObjectId -> Html -> Html
--createComment username postId mparent tag = do
createComment username postId mparent tag formId = do
  let act = ("/" ++ (show postId) ++ "/comments")
  let pid = toValue $ show postId
  --trace "form" $ form ! action (toValue act) ! id formId ! method "POST" $ do
  trace "form" $ form ! action (toValue act) ! method "POST" $ do
    input ! type_ "hidden" ! id "author" ! name "author" ! value (toValue username)
    input ! type_ "hidden" ! id "post" ! name "post" ! value pid
    div $ do
      label ! for "text" $ tag -- either "post comment" or "reply"
      input ! type_ "text" ! id "text" ! name "text" ! id "text"
    case mparent of
      Just parent ->
        input ! type_ "hidden" ! name "parent" ! id "parent" ! value (toValue $ show parent)
      Nothing ->
        input ! type_ "hidden" ! name "parent" ! id "parent" ! value ""
    p $ input ! type_ "submit" ! class_ "csubmit" ! id "submit" ! value "Post"

showComment :: Comment -> UserName -> Html
showComment comment user = do
  div ! id (toValue $ show $ fromJust $ commentId comment) $ do
    h3 $ toHtml $ commentAuthor comment
    p $ toHtml $ show $ B.timestamp $ fromJust $ commentId comment
    p $ toHtml $ commentText comment
  createComment user (commentAssocPost comment) (commentId comment) "Reply to this comment" "commentForm"

showAllReplies :: Comment -> [Comment] -> UserName -> Html
showAllReplies comment allComments user = do
  let cid = commentId comment
  --let comments = sortBy (comparing commentAssocPost) coms
  forM_ allComments $ \c -> do
    if ((commentInReplyTo c) == cid)
      then do
        ul $ showComment c user
        showAllReplies c allComments user
      else "" -- do nothing

showFrame :: String -> Html
showFrame pid = do
  let path = "/" ++ pid ++ "/comments"
  iframe ! src (toValue path) ! width "800" ! height "700" $ ""

{-
respondHtml ctitle content = okHtml $ renderHtml $ docTypeHtml $ do
  head $ do 
    title ctitle
    stylesheet "/static/css/bootstrap.css"
    stylesheet "/static/css/application.css"
    script ! src "https://login.persona.org/include.js" $ ""
  body $ do
    script ! src "/static/js/jquery.js" $ ""
    script ! src "/static/js/bootstrap.js" $ ""
    content
-}

