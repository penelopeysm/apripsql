module GenderRatio (GenderRatio (..), setupGenderRatios) where

import qualified Data.Csv as Csv
import Utils (bsToString, toIdCsv)

data GenderRatio
  = Genderless
  | FemaleOnly
  | Female71
  | Female31
  | Equal
  | Male31
  | Male71
  | MaleOnly
  deriving (Eq, Ord, Show, Bounded, Enum)

toString :: GenderRatio -> String
toString Genderless = "Genderless"
toString FemaleOnly = "Female only"
toString Female71 = "Female 7:1"
toString Female31 = "Female 3:1"
toString Equal = "Equal"
toString Male31 = "Male 3:1"
toString Male71 = "Male 7:1"
toString MaleOnly = "Male only"

fromString :: String -> Maybe GenderRatio
fromString "Genderless" = Just Genderless
fromString "Female only" = Just FemaleOnly
fromString "Female 7:1" = Just Female71
fromString "Female 3:1" = Just Female31
fromString "Equal" = Just Equal
fromString "Male 3:1" = Just Male31
fromString "Male 7:1" = Just Male71
fromString "Male only" = Just MaleOnly
fromString _ = Nothing

instance Csv.ToField GenderRatio where
  toField = Csv.toField . toString

instance Csv.FromField GenderRatio where
  parseField bs = do
    let s = bsToString bs
    case fromString s of
      Just x -> return x
      Nothing -> fail $ "Invalid gender ratio: " ++ s

instance Csv.ToNamedRecord GenderRatio where
  toNamedRecord x = Csv.namedRecord ["name" Csv..= toString x]

instance Csv.FromNamedRecord GenderRatio where
  parseNamedRecord m = do
    nameString <- m Csv..: "name"
    case fromString nameString of
      Just x -> return x
      Nothing -> fail $ "Invalid gender ratio: " ++ nameString

instance Csv.DefaultOrdered GenderRatio where
  headerOrder = const $ Csv.header ["name"]

setupGenderRatios :: IO ()
setupGenderRatios = toIdCsv "csv/gender-ratios.csv" ([minBound ..] :: [GenderRatio])
