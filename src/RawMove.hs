module RawMove (setupRawMoves) where

import Control.Applicative (empty)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Csv as Csv
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import MoveCategory (MoveCategory (..))
import Text.HTML.Scalpel
import Utils (toCsv)

data Move = Move
  { moveName :: Text,
    moveType :: Text,
    moveCategory :: MoveCategory,
    moveFlavorText :: Text
  }
  deriving (Show)

instance Csv.ToNamedRecord Move where
  toNamedRecord (Move name type_ category flavorText) =
    Csv.namedRecord
      [ "name" Csv..= name,
        "type" Csv..= type_,
        "category" Csv..= category,
        "flavor_text" Csv..= flavorText
      ]

instance Csv.FromNamedRecord Move where
  parseNamedRecord m =
    Move
      <$> m Csv..: "name"
      <*> m Csv..: "type"
      <*> m Csv..: "category"
      <*> m Csv..: "flavor_text"

instance Csv.DefaultOrdered Move where
  headerOrder = const $ Csv.header ["name", "type", "category", "flavor_text"]

getAllMoves :: IO [Move]
getAllMoves = do
  let url = "https://pokemondb.net/move/all"
  let moveScraper :: ScraperT Text IO [Move]
      moveScraper = chroots ("table" @: ["id" @= "moves"] // "tr") $ do
        name <- text ("a" @: [hasClass "ent-name"])
        url <-
          ("https://pokemondb.net" <>)
            <$> attr "href" ("a" @: [hasClass "ent-name"])
        liftIO $ print url
        inGameFlavorText <- liftIO (getMoveFlavorText url)
        flavorText <- case inGameFlavorText of
          Just ft -> pure ft
          Nothing -> text ("td" @: [hasClass "cell-long-text"])
        type_ <- text ("a" @: [hasClass "type-icon"])
        category <- attr "title" "img"
        pure $
          Move
            name
            type_
            ( case category of
                "Physical" -> Physical
                "Special" -> Special
                "Status" -> Status
                _ -> error "Invalid move category"
            )
            flavorText
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
        [ Move "Syrup Bomb" "Grass" Special "The user sets off an explosion of sticky candy syrup, which coats the target and causes the target's Speed stat to drop each turn for three turns.",
          Move "Ivy Cudgel" "Grass" Physical "The user strikes with an ivy-wrapped cudgel. This move's type changes depending on the mask worn by the user, and it has a heightened chance of landing a critical hit.",
          Move "Matcha Gotcha" "Grass" Special "The user fires a blast of tea that it mixed. The user's HP is restored by up to half the damage taken by the target. This may also leave the target with a burn.",
          Move "Blood Moon" "Normal" Special "The user unleashes the full brunt of its spirit from a full moon that shines as red as blood. This move can't be used twice in a row."
        ]
  pure $ moves <> tealMaskMoves

setupRawMoves :: IO ()
setupRawMoves = do
  moves <- getAllMoves >>= patch
  toCsv "csv/moves.csv" moves
