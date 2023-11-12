module Issues.I14 (testIssue14) where

import Ability (Ability (..))
import Data.List (find)
import Test.Tasty
import Test.Tasty.HUnit
import Utils (fromIdCsvWithoutId)

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
