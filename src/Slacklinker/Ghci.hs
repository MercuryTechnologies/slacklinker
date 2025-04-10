-- | Utilities for using GHCi more effectively in Slacklinker.
module Slacklinker.Ghci where

import Data.Maybe (fromJust)
import Data.UUID qualified as UUID
import Database.Persist (FieldDef (..), LiteralType (..), PersistEntity (..), PersistValue (..), SqlType (..))
import Slacklinker.App
import Slacklinker.Prelude

-- | Run an AppM action.
ghciAppM :: AppM b -> IO b
ghciAppM act = do
  app_ <- makeApp
  bracket (appStartupNoSender app_) appShutdownNoSender $ \runtimeInfo -> do
    let app = app_ {runtimeInfo = runtimeInfo}
    runAppM app act

-- | Summon a Key of whatever Persistent type given a string.
ghciKey :: forall a. (PersistEntity a) => String -> Key a
ghciKey s =
  let
    idSqlType = fieldSqlType $ persistFieldDef (persistIdField @a)
    value =
      case idSqlType of
        SqlOther "Composite Key" ->
          error "We can't generate composite keys automatically right now; at least, it's not immediately obvious how to do it. Try assembling a fake ID by hand using the FooKey constructor, where Foo is the name of your model."
        SqlOther "uuid" -> PersistLiteral_ Escaped (UUID.toASCIIBytes . fromJust . UUID.fromString $ s)
        SqlString -> PersistText (pack s)
        SqlInt64 -> PersistInt64 . fromJust . readMay $ s
        t -> error $ "ghciKey doesn't handle " <> show t
   in
    case keyFromValues [value] of
      Left e -> error $ cs e
      Right x -> x
