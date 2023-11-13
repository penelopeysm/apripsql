module Issues (testAllIssues) where

import Ability (Ability (..))
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Legality (Legality (..))
import Pokemon (PokemonFinal (..))
import RawPokemon (Pokemon (..))
import Test.Tasty
import Test.Tasty.HUnit
import Utils (fromCsv, fromIdCsvWithId, fromIdCsvWithoutId, makeMapFromWithIds)

getPokemonIdFromUniqueName :: Text -> IO Int
getPokemonIdFromUniqueName uniqueName = do
  allPokemon <- M.mapKeys Pokemon.uniqueName . makeMapFromWithIds <$> fromIdCsvWithId "csv/pokemon.csv"
  case M.lookup uniqueName allPokemon of
    Nothing -> error $ "Pokemon with unique name " ++ T.unpack uniqueName ++ " not found"
    Just p -> return p

testAllIssues :: TestTree
testAllIssues =
  testGroup
    "Unit tests for issues @ penelopeysm/apripsql"
    [ testIssue1,
      testIssue2,
      testIssue4,
      testIssue7,
      testIssue12,
      testIssue13,
      testIssue14,
      testIssue15
    ]

-- * Issue 1

testIssue1 :: TestTree
testIssue1 = testCase "Issue 1: No Mega evolutions" $ do
  allRawPokemon <- fromCsv "csv/pokemon-raw.csv"
  let hasMegas =
        any
          ( \p -> case RawPokemon.form p of
              Nothing -> False
              Just f -> "Mega" `T.isInfixOf` f
          )
          allRawPokemon
  assertEqual "Mega evolutions found" False hasMegas

-- * Issue 2

testIssue2 :: TestTree
testIssue2 = testCase "Issue 2: No Partner Pikachu/Eevee, Ash Greninja, Eternamax" $ do
  allRawPokemon <- fromCsv "csv/pokemon-raw.csv"
  assertBool "Partner Pikachu found" (isNothing $ find (\p -> RawPokemon.form p == Just "Partner Pikachu") allRawPokemon)
  assertBool "Partner Eevee found" (isNothing $ find (\p -> RawPokemon.form p == Just "Partner Eevee") allRawPokemon)
  assertBool "Ash-Greninja found" (isNothing $ find (\p -> RawPokemon.form p == Just "Ash-Greninja") allRawPokemon)
  assertBool "Eternamax found" (isNothing $ find (\p -> RawPokemon.form p == Just "Eternamax") allRawPokemon)

-- * Issue 4

testIssue4 :: TestTree
testIssue4 = testCase "Issue 4: Updated abilities in SV" $ do
  allRawPokemon <- fromCsv "csv/pokemon-raw.csv"
  assertEqual "Piplup HA" (Just "Competitive") (find (\p -> RawPokemon.name p == "Piplup") allRawPokemon >>= hiddenAbility)
  assertEqual "Prinplup HA" (Just "Competitive") (find (\p -> RawPokemon.name p == "Prinplup") allRawPokemon >>= hiddenAbility)
  assertEqual "Empoleon HA" (Just "Competitive") (find (\p -> RawPokemon.name p == "Empoleon") allRawPokemon >>= hiddenAbility)
  assertEqual "Gallade ability 2" (Just "Sharpness") (find (\p -> RawPokemon.name p == "Gallade") allRawPokemon >>= ability2)
  assertEqual "Shiftry ability 1" (Just "Wind Rider") (ability1 <$> find (\p -> RawPokemon.name p == "Shiftry") allRawPokemon)

-- * Issue 7

testIssue7 :: TestTree
testIssue7 =
  testCase "Issue 7: unique_names conform to [a-zA-Z0-9 ]+" $ do
    let t7Conforms :: Text -> Bool
        t7Conforms = T.all (`elem` ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789- " :: String))
    allRawPokemon <- fromCsv "csv/pokemon-raw.csv"
    let allNames = map RawPokemon.uniqueName allRawPokemon
    assertBool "All names conform to [a-zA-Z0-9 ]+" $ all t7Conforms allNames

-- * Issue 12

testIssue12 :: TestTree
testIssue12 = testCase "Issue 12: Safari/Sport Tauros-Combat are available in G9" $ do
  allLegalities <- fromCsv "csv/legalities.csv"
  gothitaId <- getPokemonIdFromUniqueName "tauros-combat"
  let gothitaLegality = find (\l -> pkmnId l == gothitaId) allLegalities
  case gothitaLegality of
    Nothing -> assertFailure "tauros-combat not found in legalities.csv"
    Just l -> do
      assertEqual "Safari Tauros-Combat is available in G7" 1 (homeSafari l)
      assertEqual "Sport Tauros-Combat is available in G7" 1 (homeSport l)

-- * Issue 13

testIssue13 :: TestTree
testIssue13 = testCase "Issue 13: Dream Gothita is available in G7" $ do
  allLegalities <- fromCsv "csv/legalities.csv"
  gothitaId <- getPokemonIdFromUniqueName "gothita"
  let gothitaLegality = find (\l -> pkmnId l == gothitaId) allLegalities
  case gothitaLegality of
    Nothing -> assertFailure "gothita not found in legalities.csv"
    Just l -> assertEqual "Dream Gothita is available in G7" 1 (bankDream l)

-- * Issue 14

testIssue14 :: TestTree
testIssue14 =
  testGroup
    "Issue 14: Libero description"
    [ testCase "Libero" (t14 "Libero"),
      testCase "Protean" (t14 "Protean")
    ]

t14 :: Text -> Assertion
t14 name = do
  allAbilities <- fromIdCsvWithoutId "csv/abilities.csv"
  case find (\a -> abilityName a == name) allAbilities of
    Nothing -> assertFailure $ "Could not find " <> T.unpack name
    Just x ->
      assertEqual
        (T.unpack name <> " description is accurate")
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
  allRawPokemon <- fromCsv "csv/pokemon-raw.csv"
  let curly = find (\p -> RawPokemon.name p == "Tatsugiri" && RawPokemon.form p == Just "Curly Form") allRawPokemon
  let tatsu = find (\p -> RawPokemon.name p == "Tatsugiri" && RawPokemon.form p == Just formName) allRawPokemon
  assertBool "Tatsugiri: Curly Form not found" (isJust tatsu)
  assertBool ("Tatsugiri: " <> T.unpack formName <> " not found") (isJust tatsu)
  assertBool "ability1 differs" (fmap ability1 curly == fmap ability1 tatsu)
  assertBool "ability2 differs" (fmap ability2 curly == fmap ability2 tatsu)
  assertBool "hiddenAbility differs" (fmap hiddenAbility curly == fmap hiddenAbility tatsu)
  assertBool "eggGroup1 differs" (fmap eggGroup1 curly == fmap eggGroup1 tatsu)
  assertBool "eggGroup2 differs" (fmap eggGroup2 curly == fmap eggGroup2 tatsu)
