module Setup.Move (MoveFinal (..), setupMoves) where

import qualified Data.Csv as Csv
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Setup.MoveCategory (MoveCategory (..))
import Setup.Type (Type (..))
import qualified Setup.RawMove as Raw
import Utils (fromCsv, fromIdCsvWithId, makeMapFromWithIds, toIdCsv, (?!))

data MoveFinal = MoveFinal
  { name :: Text,
    typeId :: Int,
    categoryId :: Int,
    flavorText :: Text,
    basePower :: Maybe Int,
    accuracy :: Maybe Int,
    pp :: Maybe Int
  }
  deriving (Show, Eq, Ord)

makeMoveFinal ::
  Map Type Int ->
  Map MoveCategory Int ->
  Raw.Move ->
  MoveFinal
makeMoveFinal typeMap catMap rm =
  MoveFinal
    { name = Raw.moveName rm,
      typeId = typeMap ?! Raw.moveType rm,
      categoryId = catMap ?! Raw.moveCategory rm,
      flavorText = Raw.moveFlavorText rm,
      basePower = Raw.moveBasePower rm,
      accuracy = Raw.moveAccuracy rm,
      pp = Raw.movePP rm
    }

instance Csv.ToNamedRecord MoveFinal where
  toNamedRecord (MoveFinal name typeId categoryId flavorText basePower accuracy pp) =
    Csv.namedRecord
      [ "name" Csv..= name,
        "type_id" Csv..= typeId,
        "category_id" Csv..= categoryId,
        "flavor_text" Csv..= flavorText,
        "base_power" Csv..= basePower,
        "accuracy" Csv..= accuracy,
        "pp" Csv..= pp
      ]

instance Csv.FromNamedRecord MoveFinal where
  parseNamedRecord m =
    MoveFinal
      <$> m Csv..: "name"
      <*> m Csv..: "type_id"
      <*> m Csv..: "category_id"
      <*> m Csv..: "flavor_text"
      <*> m Csv..: "base_power"
      <*> m Csv..: "accuracy"
      <*> m Csv..: "pp"

instance Csv.DefaultOrdered MoveFinal where
  headerOrder = const $ Csv.header ["name", "type_id", "category_id", "flavor_text", "base_power", "accuracy", "pp"]

setupMoves :: IO ()
setupMoves = do
  rawMoves <- fromCsv "csv/moves-raw.csv"
  typeMap <- makeMapFromWithIds <$> fromIdCsvWithId "csv/types.csv"
  catMap <- makeMapFromWithIds <$> fromIdCsvWithId "csv/move-categories.csv"
  let moves = map (makeMoveFinal typeMap catMap) rawMoves
  toIdCsv "csv/moves.csv" moves
