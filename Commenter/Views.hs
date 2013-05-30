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

showPage :: [Comment] -> UserName -> Maybe B.ObjectId -> Html
showPage comments user mpid = trace "showPage " $ do
  case mpid of
    Just pid -> trace ("pid:" ++ (show pid)) $ createComment user pid Nothing
    Nothing -> trace "pid is Nothing" $ ""
  indexComments comments $ Just user   -- list all comments

indexComments :: [Comment] -> Maybe UserName -> Html
indexComments coms muser = trace "indexComments" $ do
  let comments = sortBy (comparing (B.timestamp . fromJust . commentId)) coms
  div ! name "commentList" $ do
    forM_ comments $ \comment -> do
      case (commentInReplyTo comment) of
        Nothing -> do
          li $ showComment comment muser
          showAllReplies comment comments muser
        Just reply -> ""

createComment :: UserName -> B.ObjectId -> Maybe B.ObjectId -> Html
createComment username postId mparent = trace "createComment" $ do
  script ! src "/static/js/comments.js" $ ""
  trace ("username: " ++ (T.unpack username)) $ do
    form ! action "/comments" ! name "commentForm" ! method "POST" $ do
      input ! type_ "hidden" ! name "author" ! value (toValue username)
      input ! type_ "hidden" ! name "post" ! value (toValue $ show postId)
      case mparent of
        Just parent -> trace ("parent: " ++ (show parent)) $ do
          input ! type_ "hidden" ! name "parent" ! value (toValue $ show parent)
        Nothing -> ""

showComment :: Comment -> Maybe UserName -> Html
showComment comment muser = do
  p $ do
    b $ toHtml $ commentAuthor comment
  p $ toHtml $ show $ B.timestamp $ fromJust $ commentId comment
  p $ toHtml $ commentText comment
  case muser of
    Just user -> do
      p $ "Reply to this comment:"
      createComment user (commentAssocPost comment) $ commentId comment
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

