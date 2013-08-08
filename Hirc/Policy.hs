{-# LANGUAGE OverloadedStrings
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , DeriveDataTypeable
           , OverloadedStrings #-}
module Hirc.Policy ( HircPolicy
                   , withHircPolicy
                   ) where

import           Prelude hiding (lookup)

import           Data.Maybe
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import           Data.Typeable
import qualified Data.Set as Set
import qualified Data.List as List
import           Text.Regex.Posix

import           Control.Monad

import           LIO
import           LIO.DCLabel
--import           LIO.DCLabel.Core
import           LIO.TCB

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

import           Hirc.Models


data HircPolicy = HircPolicyTCB DCPriv deriving Typeable

instance PolicyModule HircPolicy where
   initPolicyModule priv = do
     setPolicy priv $ do
       database $ do
         readers ==> unrestricted
         writers ==> unrestricted
         admins  ==> this
       collection "channels" $ do
         access $ do
           readers ==> unrestricted
           writers ==> unrestricted
         clearance $ do
           secrecy   ==> this
           integrity ==> unrestricted
         document $ \doc -> do
           readers ==> unrestricted
           writers ==> unrestricted
         field "listed" key
       collection "chats" $ do
         access $ do
           readers ==> unrestricted
           writers ==> unrestricted
         clearance $ do
           secrecy   ==> this
           integrity ==> unrestricted
         document $ \doc -> do
           readers ==> unrestricted
           writers ==> unrestricted
       collection "users" $ do
         access $ do
           readers ==> unrestricted
           writers ==> unrestricted
         clearance $ do
           secrecy   ==> this
           integrity ==> unrestricted
         document $ \doc -> do
           readers ==> unrestricted
           writers ==> unrestricted
         field "name" key
     return $ HircPolicyTCB priv
       where this = privDesc priv
             root = principal "root"

instance DCLabeledRecord HircPolicy Chat where
  endorseInstance _ = HircPolicyTCB $ PrivTCB $ toCNF True

instance DCLabeledRecord HircPolicy Channel where
  endorseInstance _ = HircPolicyTCB $ PrivTCB $ toCNF True

withHircPolicy :: DBAction a -> DC a
withHircPolicy act = withPolicyModule (\(_ :: HircPolicy) -> act)

--withHircPolicy act = withPolicyModule $
  -- \(HircPolicyTCB noPrivs) -> act

