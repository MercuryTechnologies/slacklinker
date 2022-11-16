module Slacklinker.UpdateReply.Sql where

import Database.Esqueleto.Experimental
import Slacklinker.Import
import Slacklinker.Models
import Slacklinker.PersistImport ()

linkedMessagesInThread ::
  RepliedThreadId ->
  SqlQuery (SqlExpr (Entity LinkedMessage), SqlExpr (Entity ChannelMetadata))
linkedMessagesInThread repliedThreadId = do
  (lm :& cm) <-
    from
      $ table @LinkedMessage
        `innerJoin` table @ChannelMetadata
      `on` (\(lm :& cm) -> lm.channelMetadataId ==. cm.id)
  where_ $ lm.repliedThreadId ==. val repliedThreadId
  pure (lm, cm)

workspaceByRepliedThreadId :: RepliedThreadId -> SqlQuery (SqlExpr (Entity Workspace))
workspaceByRepliedThreadId repliedThreadId = do
  (rt :& ws) <-
    from $ table @RepliedThread
      `innerJoin` table @Workspace
      `on` (\(rt :& ws) -> rt.workspaceId ==. ws.id)
  where_ $ rt.id ==. val repliedThreadId
  pure ws
