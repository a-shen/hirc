{-# LANGUAGE DeriveDataTypeable
           , FlexibleContexts
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , OverloadedStrings #-}
module Commenter.Policy ( CommentPolicy
                          , withCommentPolicy
                          , findAll, findAllP
                          ) where

import           Prelude hiding (lookup)
import           Data.Maybe
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import           Data.Typeable

import           Control.Monad

import           LIO
import           LIO.DCLabel

import           Hails.Data.Hson
import           Hails.Web hiding (body)
import           Hails.Database
import           Hails.Database.Structured
import           Hails.PolicyModule
import           Hails.PolicyModule.Groups
import           Hails.PolicyModule.DSL
import           Hails.HttpServer.Types
import           Hails.HttpServer.Auth

import           Hails.Database.Structured hiding (findAll, findAllP)

import           Commenter.Models
import		 LBH.MP

import LIO.TCB

data CommentPolicy = CommentPolicyTCB DCPriv deriving Typeable

instance PolicyModule CommentPolicy where
   initPolicyModule priv = do
     setPolicy priv $ do
       database $ do
         readers ==> anybody
         writers ==> anybody
         admins  ==> this
       collection "comments" $ do
         access $ do
           readers ==> anybody
           writers ==> anybody
         clearance $ do
           secrecy   ==> this
           integrity ==> anybody
         document $ \doc -> do
           --let (Just u) = fromDocument doc
           readers ==> anybody
           writers ==> anybody
           --writers ==> (T.unpack $ userName u) \/ root \/ this
     return $ CommentPolicyTCB priv
       where this = privDesc priv
             root = principal "root"

instance DCLabeledRecord CommentPolicy Comment where
  endorseInstance _ = CommentPolicyTCB noPriv

withCommentPolicy :: DBAction a -> DC a
withCommentPolicy act = withPolicyModule $
  \(CommentPolicyTCB noPrivs) -> act

{-
instance Groups CommentPolicy where
  groupsInstanceEndorse = CommentPolicyTCB noPriv
  groups _ p pgroup = case () of
    _ | group == "#comment_viewers" -> do
      comments <- findAllP p $ select [] "comments"
      return $ map (toPrincipal . commentAuthor) comments
    where group = principalName pgroup

-- | Same as 'findWhereP', but uses groups when retrieving document.
findWhereWithGroupP :: (DCRecord a, MonadDB m) => DCPriv -> Query -> m (Maybe a)
findWhereWithGroupP p query  = liftDB $ do
  mldoc <- findOneP p query
  c <- getClearance
  case mldoc of
    Just ldoc' -> do ldoc <- labelRewrite (undefined :: LambdaChairPolicy) ldoc'
                     if canFlowToP p (labelOf ldoc) c 
                       then fromDocument `liftM` (liftLIO $ unlabelP p ldoc)
                       else return Nothing
    _ -> return Nothing

-- | Same as Hails\' 'findAll', but uses groups
findAll :: (DCRecord a, MonadDB m)
        => Query -> m [a]
findAll = findAllP noPriv

-- | Same as Hails\' 'findAllP', but uses groups
findAllP :: (DCRecord a, MonadDB m)
         => DCPriv -> Query -> m [a]
findAllP p query = liftDB $ do
  cursor <- findP p query
  cursorToRecords cursor []
  where cursorToRecords cur docs = do
          mldoc <- nextP p cur
          case mldoc of
            Just ldoc' -> do
              ldoc <- labelRewrite (undefined :: LambdaChairPolicy) ldoc'
              c <- getClearance
              if canFlowTo (labelOf ldoc) c
                then do md <- fromDocument `liftM` (liftLIO $ unlabelP p ldoc)
                        cursorToRecords cur $ maybe docs (:docs) md
                 else cursorToRecords cur docs
            _ -> return $ reverse docs
-}
