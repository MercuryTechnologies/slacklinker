module Slacklinker.UpdateReply.Sql (
  linkedMessagesInThread,
  workspaceByRepliedThreadId,
) where

import Database.Esqueleto.Experimental
import Slacklinker.Import
import Slacklinker.Models

linkedMessagesInThread ::
  RepliedThreadId ->
  SqlQuery
    ( SqlExpr (Entity LinkedMessage)
    , SqlExpr (Maybe (Entity User))
    , SqlExpr (Entity JoinedChannel)
    )
linkedMessagesInThread repliedThreadId = do
  (lm :& mU :& jc) <- from do
    table @LinkedMessage
      `leftJoin` table @User
        `on` (\(lm :& u) -> lm.userId ==. u.id)
      `innerJoin` table @JoinedChannel
        `on` (\(lm :& _ :& jc) -> lm.joinedChannelId ==. jc.id)
  where_ $ lm.repliedThreadId ==. val repliedThreadId
  orderBy [desc lm.messageTs]
  pure (lm, mU, jc)

workspaceByRepliedThreadId :: RepliedThreadId -> SqlQuery (SqlExpr (Entity Workspace))
workspaceByRepliedThreadId repliedThreadId = do
  (rt :& ws) <-
    from
      $ table @RepliedThread
        `innerJoin` table @Workspace
      `on` (\(rt :& ws) -> rt.workspaceId ==. ws.id)
  where_ $ rt.id ==. val repliedThreadId
  pure ws
