module JSONGolden (oneGoldenTest) where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (stripEnd)
import Data.Text.IO qualified as T
import Test.Hspec.Core.Spec (SpecM)
import TestImport
import Text.Pretty.Simple (pShowNoColor)
import Type.Reflection

filename :: Text -> Text -> FilePath
filename tycon name = "test/golden" </> unpack tycon </> unpack name

typeName :: forall a. Typeable a => Text
typeName = pack . tyConName . typeRepTyCon $ typeRep @a

goldenTest :: forall a. (FromJSON a, Show a, Typeable a) => Text -> LByteString -> Golden Text
goldenTest name rawInput = do
  let output = either error id $ eitherDecode @a rawInput
      theTypeName = typeName @a
   in Golden
        { output = toStrict . pShowNoColor $ output
        , encodePretty = unpack
        , writeToFile = T.writeFile
        , -- deal with vim related EOF
          readFromFile = \fname -> stripEnd <$> T.readFile fname
        , goldenFile = filename theTypeName name ++ ".golden"
        , actualFile = Just $ filename theTypeName name ++ ".actual"
        , failFirstTime = True
        }

oneGoldenTest :: forall a. (FromJSON a, Show a, Typeable a) => Text -> SpecM () ()
oneGoldenTest name = do
  input <- runIO . LBS.readFile $ filename (typeName @a) name <> ".json"
  it (unpack name) $ goldenTest @a name input
