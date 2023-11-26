module Setup.MoveCategory (MoveCategory (..), setupMoveCategories) where

import qualified Data.Csv as Csv
import Utils (bsToString, toIdCsv)

data MoveCategory
  = Physical
  | Special
  | Status
  deriving (Eq, Ord, Show, Bounded, Enum)

toString :: MoveCategory -> String
toString = show

fromString :: String -> Maybe MoveCategory
fromString "Physical" = Just Physical
fromString "Special" = Just Special
fromString "Status" = Just Status
fromString _ = Nothing

instance Csv.ToField MoveCategory where
  toField = Csv.toField . toString

instance Csv.FromField MoveCategory where
  parseField bs = do
    let s = bsToString bs
    case fromString s of
      Just x -> return x
      Nothing -> fail $ "Invalid move category: " ++ s

instance Csv.ToNamedRecord MoveCategory where
  toNamedRecord x = Csv.namedRecord ["name" Csv..= toString x]

instance Csv.FromNamedRecord MoveCategory where
  parseNamedRecord m = do
    nameString <- m Csv..: "name"
    case fromString nameString of
      Just x -> return x
      Nothing -> fail $ "Invalid move category: " ++ nameString

instance Csv.DefaultOrdered MoveCategory where
  headerOrder = const $ Csv.header ["name"]

setupMoveCategories :: IO ()
setupMoveCategories = toIdCsv "csv/move-categories.csv" ([minBound ..] :: [MoveCategory])
