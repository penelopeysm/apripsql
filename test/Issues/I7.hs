module Issues.I7 (testIssue7) where

import Data.Text (Text)
import qualified Data.Text as T
import RawPokemon (Pokemon (..))
import Test.Tasty
import Test.Tasty.HUnit
import Utils (fromIdCsvWithoutId)

testIssue7 :: TestTree
testIssue7 =
  testGroup
    "Issue 7: unique_names conform to [a-zA-Z0-9 ]+"
    [testCase "All names" t]

conforms :: Text -> Bool
conforms = T.all (`elem` ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789- " :: String))

t :: Assertion
t = do
  allRawPokemon <- fromIdCsvWithoutId "csv/pokemon-raw.csv"
  let allNames = map uniqueName allRawPokemon
  assertBool "All names conform to [a-zA-Z0-9 ]+" $ all conforms allNames
