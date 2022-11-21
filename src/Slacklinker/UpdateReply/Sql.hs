module Slacklinker.UpdateReply.Sql (
  linkedMessagesInThread,
  workspaceByRepliedThreadId,
) where

import Database.Esqueleto.Experimental
import Slacklinker.Import
import Slacklinker.Models

linkedMessagesInThread ::
  RepliedThreadId ->
  SqlQuery (SqlExpr (Entity LinkedMessage), SqlExpr (Entity JoinedChannel))
linkedMessagesInThread repliedThreadId = do
  (lm :& jc) <-
    from
      $ table @LinkedMessage
        `innerJoin` table @JoinedChannel
      `on` (\(lm :& jc) -> lm.joinedChannelId ==. jc.id)
  where_ $ lm.repliedThreadId ==. val repliedThreadId
  orderBy [desc lm.messageTs]
  pure (lm, jc)

workspaceByRepliedThreadId :: RepliedThreadId -> SqlQuery (SqlExpr (Entity Workspace))
workspaceByRepliedThreadId repliedThreadId = do
  (rt :& ws) <-
    from
      $ table @RepliedThread
        `innerJoin` table @Workspace
      `on` (\(rt :& ws) -> rt.workspaceId ==. ws.id)
  where_ $ rt.id ==. val repliedThreadId
  pure ws
