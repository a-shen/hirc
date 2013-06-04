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
  --script ! src "http://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.js" $ ""
  --script ! src "http://malsup.github.com/jquery.form.js" $ ""
  --script ! src "/static/js/comments.js" $ ""
  trace ("pid:" ++ (show pid)) $ newComment user pid Nothing
  indexComments comments pid $ Just user   -- list all comments

indexComments :: [Comment] -> B.ObjectId -> Maybe UserName -> Html
indexComments coms pid muser = do
  let comments = reverse $ sortBy (comparing (B.timestamp . fromJust . commentId)) coms
  p ! name "commentList" ! id "commentList" $ trace "here" $ do
    forM_ comments $ \comment -> do
      if (commentAssocPost comment) == pid
        then case (commentInReplyTo comment) of
          Nothing -> do
            li $ showComment comment muser
            showAllReplies comment comments muser
          Just reply -> ""
        else ""

newComment :: UserName -> B.ObjectId -> Maybe B.ObjectId -> Html
newComment username postId mparent = trace "newComment" $ 
  createComment username postId mparent "Post a comment"

createComment :: UserName -> B.ObjectId -> Maybe B.ObjectId -> Html -> Html
createComment username postId mparent tag = do
  --script ! src "http://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.js" $ ""
  --trace ("username: " ++ (T.unpack username)) $ do
    let act = ("/" ++ (show postId) ++ "/comments")
    form ! action (toValue act) ! name "commentForm" ! id "commentForm" ! method "POST" $ do
      input ! type_ "hidden" ! name "author" ! value (toValue username)
      input ! type_ "hidden" ! name "post" ! value (toValue $ show postId)
      div $ do
        label ! for "text" $ tag -- either "post comment" or "reply"
        input ! type_ "text" ! name "text" ! id "text"
      case mparent of
        Just parent -> do
          input ! type_ "hidden" ! name "parent" ! value (toValue $ show parent)
        Nothing -> ""
      p $ input ! type_ "submit" ! value "Post"

showComment :: Comment -> Maybe UserName -> Html
showComment comment muser = do
  h3 $ toHtml $ commentAuthor comment
  p $ toHtml $ show $ B.timestamp $ fromJust $ commentId comment
  p $ toHtml $ commentText comment
  case muser of
    Just user -> do
      createComment user (commentAssocPost comment) (commentId comment) "Reply to this comment"
    Nothing -> ""

showAllReplies :: Comment -> [Comment] -> Maybe UserName -> Html
showAllReplies comment allComments muser = do
  let id = commentId comment
  --let comments = sortBy (comparing commentAssocPost) coms
  forM_ allComments $ \c -> do
    if ((commentInReplyTo c) == id)
      then do
        li $ showComment c muser
        showAllReplies c allComments muser
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

