module LearnMethod (LearnMethod (..), setupLearnMethods) where

import qualified Data.Csv as Csv
import Utils (toIdCsv)

data LearnMethod
  = LevelUp
  | Evolution
  | Tutor
  | Egg
  | TM
  | Reminder
  deriving (Eq, Ord, Show, Bounded, Enum)

toString :: LearnMethod -> String
toString LevelUp = "Level up"
toString Evolution = "Evolution"
toString Tutor = "Tutor"
toString Egg = "Egg"
toString TM = "TM"
toString Reminder = "Reminder"

fromString :: String -> Maybe LearnMethod
fromString "Level up" = Just LevelUp
fromString "Evolution" = Just Evolution
fromString "Tutor" = Just Tutor
fromString "Egg" = Just Egg
fromString "TM" = Just TM
fromString "Reminder" = Just Reminder
fromString _ = Nothing

instance Csv.ToNamedRecord LearnMethod where
  toNamedRecord x = Csv.namedRecord ["name" Csv..= toString x]

instance Csv.FromNamedRecord LearnMethod where
  parseNamedRecord m = do
    nameString <- m Csv..: "name"
    case fromString nameString of
      Just x -> return x
      Nothing -> fail $ "Invalid move learn method: " ++ nameString

instance Csv.DefaultOrdered LearnMethod where
  headerOrder = const $ Csv.header ["name"]

setupLearnMethods :: IO ()
setupLearnMethods = toIdCsv "csv/learn-methods.csv" ([minBound ..] :: [LearnMethod])
