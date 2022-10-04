{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Slacklinker.Persist.DeriveEnum
  ( PostgresEnum,
    allEnumValues,
    derivePostgresEnumForSumType,
    derivePostgresEnumForSumTypeStripCommonPrefix,
  )
where

import Control.Monad (zipWithM)
import Control.Monad.Fail (fail)
import Data.ByteString.Char8 qualified as S8
import Data.Either.Combinators (maybeToRight)
import Data.Text qualified as T
import Database.Persist (PersistField (..), PersistValue (..))
import Database.Persist.Sql (PersistFieldSql, SqlType (..), sqlType)
import Language.Haskell.TH.Datatype (ConstructorInfo (..), ConstructorVariant (..), DatatypeInfo (..), reifyDatatype)
import Language.Haskell.TH.Lib (conT, varE)
import Language.Haskell.TH.Syntax
import Slacklinker.Prelude hiding (lift)

class (Bounded a, Enum a, Ord a, PersistField a, PersistFieldSql a, Typeable a) => PostgresEnum a

allEnumValues :: (Enum a, Bounded a) => [a]
allEnumValues = [minBound .. maxBound]

{- | Derives PersistField and PersistFieldSql for a sum type, using Postgres enums

 Unlike derivePostgresEnum, this version does not use Show/Read instances, but instead the exact Haskell name
 By not using Read (or converting from ByteString to String), parsing speed is dramatically increased
 If necessary, we could probably make this also work for Show instances as well, though.

 Example usage: $(derivePostgresEnumForSumType ''MySumType "my_sum_type")
-}
derivePostgresEnumForSumType :: Name -> String -> Q [Dec]
derivePostgresEnumForSumType = derivePostgresEnumForSumTypeWith pure

{- | Derives PersistField and PersistFieldSql for a sum type using Postgres enums
 stripping off the common prefix shared by constructors.
 Will fail if there is no common prefix (i.e. the common prefix is empty)

 Example usage: $(derivePostgresEnumForSumTypeStripCommonPrefix ''MySumType "my_sum_type")
-}
derivePostgresEnumForSumTypeStripCommonPrefix :: Name -> String -> Q [Dec]
derivePostgresEnumForSumTypeStripCommonPrefix = derivePostgresEnumForSumTypeWith tryStripCommonPrefixes

{- | Derives PersistField and PersistFieldSql for a sum type, using Postgres enums

 Unlike derivePostgresEnum, this version does not use Show/Read instances, but instead the exact Haskell name
 By not using Read (or converting from ByteString to String), parsing speed is dramatically increased
 If necessary, we could probably make this also work for Show instances as well, though.
-}
derivePostgresEnumForSumTypeWith :: ([String] -> Either String [String]) -> Name -> String -> Q [Dec]
derivePostgresEnumForSumTypeWith tryTransformNames typeName postgresType = do
  datatypeInfo <- reifyDatatype typeName
  constructorNames <- mapM getNormalConstructorName (datatypeCons datatypeInfo)

  -- Apply the name transformation function. If it fails, we will fail in Q
  transformedNameStrings <- either fail pure . tryTransformNames $ map nameToOccNameString constructorNames

  toPersistValueMatches <- zipWithM constructorNameToMatch constructorNames transformedNameStrings
  let toPersistValueExp = LamCaseE toPersistValueMatches

  fromPersistValueGoodMatches <- zipWithM constructorNameFromMatch constructorNames transformedNameStrings
  fromPersistValueBadMatches <- fromPersistValueBadMatch typeName
  let fromPersistValueExp = LamCaseE (fromPersistValueGoodMatches <> [fromPersistValueBadMatches])

  [d|
    instance PersistField $(conT typeName) where
      toPersistValue = $(pure toPersistValueExp)
      fromPersistValue = $(pure fromPersistValueExp)

    instance PersistFieldSql $(conT typeName) where
      sqlType _ = SqlOther $ T.pack $(lift postgresType)

    instance PostgresEnum $(conT typeName)
    |]

-- | Given a constructor "Foo", generates @Foo -> PersistLiteralEscaped "Foo"@
constructorNameToMatch :: Name -> String -> Q Match
constructorNameToMatch name nameStr = do
  anExp <- [|PersistLiteralEscaped $ S8.pack $(lift nameStr)|]

  pure $ Match (ConP name [] []) (NormalB anExp) []

-- | Given a constructor "Foo", generates @"Foo" -> Right Foo@
constructorNameFromMatch :: Name -> String -> Q Match
constructorNameFromMatch name nameStr = do
  let pat = ConP 'PersistLiteralEscaped [] [LitP (StringL nameStr)]

  (rightConstructor :: Exp) <- [|Right|]
  let result = rightConstructor `AppE` ConE name
  pure $ Match pat (NormalB result) []

{- | Generates a fallthrough pattern match for the given type name:

 @unexpectedPersistValue -> Left "fromPersistField: When trying to deserialize a value of type MyEnum, got Baz"@
-}
fromPersistValueBadMatch :: Name -> Q Match
fromPersistValueBadMatch typeName = do
  let s = nameToOccNameString typeName
  unexpectedPersistValueName <- newName "unexpectedPersistValue"
  errorCase <- [|Left $ T.pack $ "fromPersistField: When trying to deserialize a value of type " <> $(lift s) <> ", got: " <> show $(varE unexpectedPersistValueName)|]
  pure $ Match (VarP unexpectedPersistValueName) (NormalB errorCase) []

getNormalConstructorName :: ConstructorInfo -> Q Name
getNormalConstructorName constructorInfo = case constructorVariant constructorInfo of
  NormalConstructor -> case constructorFields constructorInfo of
    [] -> pure $ constructorName constructorInfo
    bad -> fail $ "derivePostgresEnumForSumType only works for sum types with no arguments (e.g. data X = A | B is OK, data X = A Int | B is not). Got: " <> show bad
  bad -> fail $ "derivePostgresEnumForSumType expects normal (non-record, non-infix) constructors, got: " <> show bad

nameToOccNameString :: Name -> String
nameToOccNameString (Name (OccName s) _) = s

tryStripCommonPrefixes :: [String] -> Either String [String]
tryStripCommonPrefixes [] = Left "Type has no constructors"
tryStripCommonPrefixes strs@(s : ss) = do
  prefix <- foldM tryGetCommonPrefix s ss
  mapM (maybeToRight "BUG: Calculated common prefix was not common" . stripPrefix prefix) strs
  where
    tryGetCommonPrefix :: String -> String -> Either String String
    tryGetCommonPrefix prefix str = case collectPrefix [] prefix str of
      [] -> Left $ "Could not find a common prefix from prefix '" <> prefix <> "' and name '" <> str <> "'"
      commonPrefix -> Right commonPrefix

    collectPrefix :: String -> String -> String -> String
    collectPrefix acc (l : ls) (r : rs)
      | l == r = collectPrefix (l : acc) ls rs
      | otherwise = reverse acc
    collectPrefix acc _ _ = reverse acc
