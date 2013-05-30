{-# LANGUAGE OverloadedStrings
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , DeriveDataTypeable
           , OverloadedStrings #-}
module Commenter.Policy ( CommentPolicy
                          , personaLoginEmailToUid
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

-- **IF THERE'S AN ERROR WITH PERSONA, IT'S PROBABLY BECAUSE THIS IS COMMENTED OUT**

{-
-- | Requests are labeled by email addreses, relabel to id's.
personaLoginEmailToUid :: Middleware
personaLoginEmailToUid app conf lreq = do
  meu <- liftLIO . withPolicyModule $ \(LBHPolicyTCB privs) -> do
    let i     = dcIntegrity $ requestLabel conf
        s     = dcSecrecy   $ requestLabel conf
        ps    = toList i
        email = head . head $ ps
    -- Label must have format <_, principal>
    musr <- if i == dcFalse || length ps /= 1 || length (head ps) /= 1
              then return Nothing
              else do mu <- findByP privs "users" "email" $
                              T.decodeUtf8 . principalName $ email
                      return $ (principal . T.encodeUtf8 . userId) `liftM` mu
    return $ do { u <- musr ; return $ mkXfms email u }
  case meu of
    Nothing -> app conf lreq
    Just (e2u, u2e) -> do
         let conf' = conf { browserLabel = dcLabel
                              (e2u $ dcSecrecy $ browserLabel conf) dcTrue
                          , requestLabel = dcLabel
                               dcTrue (e2u $ dcIntegrity $ requestLabel conf)
                          }
         setClearanceP allPrivTCB $ browserLabel conf'
         lreq' <- relabelLabeledP allPrivTCB (requestLabel conf') lreq
         resp <- app conf' lreq'
         curl <- getLabel
         curc <- getClearance
         let curc' = dcLabel (u2e $ dcSecrecy curc) (u2e $ dcIntegrity curc)
             curl' = dcLabel (u2e $ dcSecrecy curl) (u2e $ dcIntegrity curl)
         -- raise current clearance, considering the current label
         setClearanceP allPrivTCB $ curc' `upperBound` curl
         -- change current label
         setLabelP allPrivTCB curl'
         -- lower current clearance
         setClearanceP allPrivTCB $ curc'
         return resp
    where mkXfms e u = ( \cmp -> xfm (\x -> if x == e then u else x) cmp
                       , \cmp -> xfm (\x -> if x == u then e else x) cmp )
          xfm f cmp | cmp == dcTrue || cmp == dcFalse = cmp
          xfm f cmp = let cs = unDCFormula cmp
                      in dcFormula $
                           Set.map (\c -> Clause $ Set.map f $ unClause c) cs
-}


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
