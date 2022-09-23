{-# OPTIONS_GHC -Wno-orphans #-}
module Slacklinker.UUID (module Data.UUID) where

import Data.UUID (UUID, toASCIIBytes, fromASCIIBytes, fromText, toText)
import Slacklinker.Import
import Database.Persist
import Database.Persist.Postgresql (PersistFieldSql(..))
import Web.PathPieces (PathPiece (..))

instance PersistField UUID where
  toPersistValue = PersistLiteralEscaped . toASCIIBytes
  fromPersistValue pv =
    case pv of
      PersistLiteral_ _ uuid ->
        case fromASCIIBytes uuid of
          Nothing -> Left $ "Slacklinker/UUID.hs: Failed to deserialize a UUID; received: " <> pack (show uuid)
          -- Prevent a thunk leak by ensuring that the UUID returned from
          -- 'fromASCIIBytes' is in WHNF.
          Just uuid' -> Right $! uuid'
      PersistText txt
        | Just uuid <- fromText txt -> Right $! uuid
      _ ->
        Left $ "Slacklinker/UUID.hs: When trying to deserialize a UUID: expected PersistLiteral, received: " <> tshow pv

instance PersistFieldSql UUID where
  sqlType _ = SqlOther "uuid"

instance PathPiece UUID where
  toPathPiece = toText
  fromPathPiece = fromText
