module Issues (testAllIssues) where

import Ability (Ability (..))
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import RawPokemon (Pokemon (..))
import Test.Tasty
import Test.Tasty.HUnit
import Utils (fromIdCsvWithoutId)

testAllIssues :: TestTree
testAllIssues =
  testGroup
    "Unit tests for issues @ penelopeysm/apripsql"
    [testIssue7, testIssue14, testIssue15]

-- * Issue 7

testIssue7 :: TestTree
testIssue7 =
  testGroup
    "Issue 7: unique_names conform to [a-zA-Z0-9 ]+"
    [testCase "All names" t7]

t7Conforms :: Text -> Bool
t7Conforms = T.all (`elem` ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789- " :: String))

t7 :: Assertion
t7 = do
  allRawPokemon <- fromIdCsvWithoutId "csv/pokemon-raw.csv"
  let allNames = map uniqueName allRawPokemon
  assertBool "All names conform to [a-zA-Z0-9 ]+" $ all t7Conforms allNames

-- * Issue 14

testIssue14 :: TestTree
testIssue14 =
  testGroup
    "Issue 14: Libero description"
    [ testCase "Libero" libero,
      testCase "Protean" protean
    ]

libero :: Assertion
libero = do
  allAbilities <- fromIdCsvWithoutId "csv/abilities.csv"
  case find (\a -> abilityName a == "Libero") allAbilities of
    Nothing -> assertFailure "Could not find Libero"
    Just x ->
      assertEqual
        "Libero description is accurate"
        "Changes the Pokémon’s type to the type of the move it’s about to use. This works only once each time the Pokémon enters battle."
        (abilityFlavorText x)

protean :: Assertion
protean = do
  allAbilities <- fromIdCsvWithoutId "csv/abilities.csv"
  case find (\a -> abilityName a == "Protean") allAbilities of
    Nothing -> assertFailure "Could not find Protean"
    Just x ->
      assertEqual
        "Protean description is accurate"
        "Changes the Pokémon’s type to the type of the move it’s about to use. This works only once each time the Pokémon enters battle."
        (abilityFlavorText x)

-- * Issue 15

testIssue15 :: TestTree
testIssue15 =
  testGroup
    "Issue 15: Tatsugiri data missing"
    [ testCase "Droopy" (t15 "Droopy Form"),
      testCase "Stretchy" (t15 "Stretchy Form")
    ]

t15 :: Text -> Assertion
t15 formName = do
  allRawPokemon <- fromIdCsvWithoutId "csv/pokemon-raw.csv"
  let curly = find (\p -> name p == "Tatsugiri" && form p == Just "Curly Form") allRawPokemon
  let tatsu = find (\p -> name p == "Tatsugiri" && form p == Just formName) allRawPokemon
  assertBool "Tatsugiri: Curly Form not found" (isJust tatsu)
  assertBool ("Tatsugiri: " <> T.unpack formName <> " not found") (isJust tatsu)
  assertBool "ability1 differs" (fmap ability1 curly == fmap ability1 tatsu)
  assertBool "ability2 differs" (fmap ability2 curly == fmap ability2 tatsu)
  assertBool "hiddenAbility differs" (fmap hiddenAbility curly == fmap hiddenAbility tatsu)
  assertBool "eggGroup1 differs" (fmap eggGroup1 curly == fmap eggGroup1 tatsu)
  assertBool "eggGroup2 differs" (fmap eggGroup2 curly == fmap eggGroup2 tatsu)
