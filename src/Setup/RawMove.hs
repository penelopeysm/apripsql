module Setup.RawMove (Move (..), setupRawMoves) where

import Control.Applicative (empty)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Csv as Csv
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Setup.MoveCategory (MoveCategory (..))
import Setup.Type (Type (..), fromString)
import qualified Setup.Type
import Text.HTML.Scalpel
import Utils (readMaybeInt, toCsv)

data Move = Move
  { moveName :: Text,
    moveType :: Type,
    moveCategory :: MoveCategory,
    moveFlavorText :: Text,
    moveBasePower :: Maybe Int,
    moveAccuracy :: Maybe Int,
    movePP :: Maybe Int -- Only Dynamax moves don't have PP
  }
  deriving (Eq, Ord, Show)

instance Csv.ToNamedRecord Move where
  toNamedRecord (Move name type_ category flavorText bp acc pp) =
    Csv.namedRecord
      [ "name" Csv..= name,
        "type" Csv..= type_,
        "category" Csv..= category,
        "flavor_text" Csv..= flavorText,
        "base_power" Csv..= bp,
        "accuracy" Csv..= acc,
        "pp" Csv..= pp
      ]

instance Csv.FromNamedRecord Move where
  parseNamedRecord m =
    Move
      <$> m Csv..: "name"
      <*> m Csv..: "type"
      <*> m Csv..: "category"
      <*> m Csv..: "flavor_text"
      <*> m Csv..: "base_power"
      <*> m Csv..: "accuracy"
      <*> m Csv..: "pp"

instance Csv.DefaultOrdered Move where
  headerOrder = const $ Csv.header ["name", "type", "category", "flavor_text", "base_power", "accuracy", "pp"]

getAllMoves :: IO [Move]
getAllMoves = do
  let url = "https://pokemondb.net/move/all"
  let moveScraper :: ScraperT Text IO [Move]
      moveScraper = chroots ("table" @: ["id" @= "moves"] // "tr") $ do
        name <- text ("a" @: [hasClass "ent-name"])
        url <-
          ("https://pokemondb.net" <>)
            <$> attr "href" ("a" @: [hasClass "ent-name"])
        inGameFlavorText <- liftIO (getMoveFlavorText url)
        liftIO $ T.putStrLn $ "Scraping: " <> url
        flavorText <- case inGameFlavorText of
          Just ft -> pure ft
          Nothing -> case name of
            "Blazing Torque" -> pure "Signature move of the Schedar Starmobile."
            "Combat Torque" -> pure "Signature move of the Caph Starmobile."
            "Magical Torque" -> pure "Signature move of the Ruchbah Starmobile."
            "Noxious Torque" -> pure "Signature move of the Navi Starmobile."
            "Wicked Torque" -> pure "Signature move of the Segin Starmobile."
            _ -> text ("td" @: [hasClass "cell-long-text"])
        type_ <- text ("a" @: [hasClass "type-icon"])
        category <- attr "title" "img"
        (bpStr : accStr : ppStr : _) <- texts ("td" @: [hasClass "cell-num"])
        pure $
          Move
            name
            ( case fromString (T.unpack type_) of
                Just t -> t
                Nothing -> error "Invalid move type"
            )
            ( case category of
                "Physical" -> Physical
                "Special" -> Special
                "Status" -> Status
                _ -> error "Invalid move category"
            )
            flavorText
            ( case name of
                -- Gen 9 update
                "Grassy Glide" -> Just 55
                _ -> readMaybeInt bpStr
            )
            (readMaybeInt accStr)
            (readMaybeInt ppStr)
  tags <- fetchTags (T.unpack url)
  fromJust <$> scrapeT moveScraper tags

getMoveFlavorText :: Text -> IO (Maybe Text)
getMoveFlavorText url = do
  scrapeURL (T.unpack url) $
    chroot ("div" @: [hasClass "grid-col"]) $
      inSerial $ do
        -- Find the first h2 that contains "Game descriptions"
        seekNext $ do
          h2 <- text "h2"
          guard $ "Game descriptions" `T.isInfixOf` h2
        -- Pick out the next table
        seekNext $ chroot "table" $ do
          -- Get the last td (corresponding to the latest game)
          tds <- texts "td"
          case filter (not . ("recommended that this move is forgotten" `T.isInfixOf`)) tds of
            [] -> empty
            flavorTexts -> pure $ last flavorTexts

patch :: [Move] -> IO [Move]
patch moves = do
  let tealMaskMoves =
        [ Move
            "Syrup Bomb"
            Grass
            Special
            "The user sets off an explosion of sticky candy syrup, which coats the target and causes the target's Speed stat to drop each turn for three turns."
            (Just 60)
            (Just 85)
            (Just 10),
          Move
            "Ivy Cudgel"
            Grass
            Physical
            "The user strikes with an ivy-wrapped cudgel. This move's type changes depending on the mask worn by the user, and it has a heightened chance of landing a critical hit."
            (Just 100)
            (Just 100)
            (Just 10),
          Move
            "Matcha Gotcha"
            Grass
            Special
            "The user fires a blast of tea that it mixed. The user's HP is restored by up to half the damage taken by the target. This may also leave the target with a burn."
            (Just 80)
            (Just 90)
            (Just 15),
          Move
            "Blood Moon"
            Normal
            Special
            "The user unleashes the full brunt of its spirit from a full moon that shines as red as blood. This move can't be used twice in a row."
            (Just 140)
            (Just 100)
            (Just 5)
        ]
  pure $ sort $ moves <> tealMaskMoves

setupRawMoves :: IO ()
setupRawMoves = getAllMoves >>= patch >>= toCsv "csv/moves-raw.csv"
