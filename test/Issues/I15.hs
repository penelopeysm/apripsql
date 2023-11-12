module Issues.I15 (testIssue15) where

import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import RawPokemon (Pokemon (..))
import Test.Tasty
import Test.Tasty.HUnit
import Utils (fromIdCsvWithoutId)

testIssue15 :: TestTree
testIssue15 =
  testGroup
    "Issue 15: Tatsugiri data missing"
    [ testCase "Droopy" (t "Droopy Form"),
      testCase "Stretchy" (t "Stretchy Form")
    ]

t :: Text -> Assertion
t formName = do
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
