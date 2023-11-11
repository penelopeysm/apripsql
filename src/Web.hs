module Web where

import Control.Applicative
import Control.Exception (throwIO, try)
import Control.Monad (forM_, guard, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (groupBy, nub)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Read (decimal)
import Debug.Trace
import Text.HTML.Scalpel
import Types

-- | Scrapes the ability flavor text from PokemonDB.
getAbilityFlavorText :: Text -> IO Text
getAbilityFlavorText abil = do
  let url =
        "https://pokemondb.net/ability/"
          <> (T.toLower . T.replace " " "-" . T.replace "'" "" $ abil)
  let flavorTextScraper :: Scraper Text Text
      flavorTextScraper = chroot ("div" @: [hasClass "grid-col"]) $ inSerial $ do
        -- Find the first h2 that contains "Game descriptions"
        seekNext $ do
          h2 <- text "h2"
          guard $ "Game descriptions" `T.isInfixOf` h2
        -- Pick out the next table
        seekNext $ chroot "table" $ do
          -- Get the last td (corresponding to the latest game)
          tds <- texts "td"
          case tds of
            [] -> empty
            _ -> pure $ last tds
  flavorText <- scrapeURL (T.unpack url) flavorTextScraper
  case flavorText of
    Nothing -> throwIO $ userError $ "Could not scrape " <> T.unpack url
    Just ft -> pure ft

getAllAbilities :: IO [Text]
getAllAbilities = do
  let url = "https://pokemondb.net/ability"
  let abilityScraper :: Scraper Text [Text]
      abilityScraper = chroot ("table" @: ["id" @= "abilities"]) $ do
        texts $ "a" @: [hasClass "ent-name"]
  abilities <- scrapeURL (T.unpack url) abilityScraper
  case abilities of
    Nothing -> throwIO $ userError $ "Could not scrape " <> T.unpack url
    Just abils -> pure abils

makeGenderRatioText :: GenderRatio -> Text
makeGenderRatioText g = case g of
  Genderless -> "Genderless"
  FemaleOnly -> "Female only"
  Female71 -> "Female 7:1"
  Female31 -> "Female 3:1"
  Equal -> "Equal"
  Male31 -> "Male 3:1"
  Male71 -> "Male 7:1"
  MaleOnly -> "Male only"

-- | Converts a Text to an Int, throwing an error if it fails.
readInt :: Text -> Int
readInt s = fst . fromRightPartial . decimal $ s
  where
    fromRightPartial (Right b) = b
    fromRightPartial (Left _) = error $ T.unpack $ "fromRightPartial: Left when decoding " <> s

getAllPokemonPartial :: IO [PokemonPartial]
getAllPokemonPartial = do
  let url = "https://pokemondb.net/pokedex/all"
  let partialPokemonScraper :: Scraper Text [PokemonPartial]
      partialPokemonScraper = chroots ("table" @: ["id" @= "pokedex"] // "tr") $ do
        ndex <-
          readInt
            <$> text ("span" @: [hasClass "infocard-cell-data"])
        species <- text ("a" @: [hasClass "ent-name"])
        monUrl <- ("https://pokemondb.net" <>) <$> attr "href" ("a" @: [hasClass "ent-name"])
        form <- optional $ text ("small" @: [hasClass "text-muted"])
        types <- texts ("a" @: [hasClass "type-icon"])
        let (t1, t2) = case types of
              [t1] -> (t1, Nothing)
              [t1, t2] -> (t1, Just t2)
              _ -> error "Pokemon had invalid number of types"
        -- Temporarily set formNum to 0
        pure $ PokemonPartial species form 0 ndex t1 t2 monUrl
  partialMons <- fromJust <$> scrapeURL (T.unpack url) partialPokemonScraper
  -- Add in formNums
  let groupedMons = groupBy (\a b -> pname a == pname b) partialMons
  pure $ concatMap (\ms -> zipWith (\m i -> m {pformNum = i}) ms [0 ..]) groupedMons

errorPkmn :: PokemonPartial -> Text -> a
errorPkmn ppkmn msg =
  error $
    T.unpack $
      "Error scraping "
        <> pname ppkmn
        <> maybe "" ("-" <>) (pform ppkmn)
        <> ": "
        <> msg

getPokemon :: PokemonPartial -> IO Pokemon
getPokemon ppkmn = do
  T.putStrLn $ "Scraping " <> pname ppkmn <> maybe "" ("-" <>) (pform ppkmn)
  let haScraper :: Scraper Text Pokemon
      haScraper = do
        -- Check that we are looking at the correct form
        forms <- texts ("a" @: [hasClass "sv-tabs-tab"])
        guard $ case pform ppkmn of
          Nothing -> True
          Just form -> form == (forms !! pformNum ppkmn)
        -- If so, grab the HA
        has <- chroots ("table" @: [hasClass "vitals-table"] // "tr") $ do
          heading <- text "th"
          guard $ heading == "Abilities"
          optional $ text ("small" @: [hasClass "text-muted"] // "a")
        let ha = has !! pformNum ppkmn
        -- And the egg groups (which don't differ by form)
        eggGroups <- chroot ("table" @: [hasClass "vitals-table"] // "tr") $ do
          heading <- text "th"
          guard $ heading == "Egg Groups"
          texts "a"
        let (egg1, egg2) = case eggGroups of
              [e1] -> (e1, Nothing)
              [e1, e2] -> (e1, Just e2)
              _ -> errorPkmn ppkmn "Pokemon had invalid number of egg groups"
        -- And the gender ratio (which doesn't differ by form)
        genderRatio <- chroot ("table" @: [hasClass "vitals-table"] // "tr") $ do
          heading <- text "th"
          guard $ heading == "Gender"
          ratio <- text ("span" @: [hasClass "text-blue"]) <|> text "td"
          pure $ case ratio of
            "Genderless" -> Genderless
            "—" -> Genderless
            "0% male" -> FemaleOnly
            "12.5% male" -> Female71
            "25% male" -> Female31
            "50% male" -> Equal
            "75% male" -> Male31
            "87.5% male" -> Male71
            "100% male" -> MaleOnly
            _ -> errorPkmn ppkmn ("Could not determine gender ratio: got " <> ratio)
        pure $
          Pokemon
            { name = pname ppkmn,
              form = pform ppkmn,
              ndex = pndex ppkmn,
              type1 = ptype1 ppkmn,
              type2 = ptype2 ppkmn,
              eggGroup1 = egg1,
              eggGroup2 = egg2,
              genderRatio = genderRatio,
              hiddenAbility = ha
            }
  fromJust <$> scrapeURL (T.unpack $ pmonUrl ppkmn) haScraper

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

makeGameText :: Game -> Text
makeGameText USUM = "USUM"
makeGameText SWSH = "SwSh"
makeGameText BDSP = "BDSP"
makeGameText SV = "SV"

isRegionalNotInUSUM :: PokemonPartial -> Bool
isRegionalNotInUSUM pkmn =
  case pform pkmn of
    Just form -> any (`T.isInfixOf` form) ["Galarian", "Hisuian", "Paldean"]
    Nothing -> False

isRegionalNotInBDSP :: PokemonPartial -> Bool
isRegionalNotInBDSP pkmn =
  case pform pkmn of
    Just form -> any (`T.isInfixOf` form) ["Alolan", "Galarian", "Hisuian", "Paldean"]
    Nothing -> False

isRegionalNotInSWSH :: PokemonPartial -> Bool
isRegionalNotInSWSH pkmn =
  case pform pkmn of
    Just form -> "Paldean" `T.isInfixOf` form
    Nothing -> False

getLearnset :: PokemonPartial -> Game -> IO [MoveLearn]
getLearnset pkmn game =
  if (game == BDSP && isRegionalNotInBDSP pkmn) || (game == SWSH && isRegionalNotInSWSH pkmn) || (game == USUM && isRegionalNotInUSUM pkmn)
    then pure []
    else do
      -- Step 1: Figure out which is the tab id corresponding to the correct game
      let (gen, targetTagName) = case game of
            USUM -> (7, "Ultra Sun/Ultra Moon")
            SWSH -> (8, "Sword/Shield")
            BDSP -> (8, "Brilliant Diamond/Shining Pearl")
            SV -> (9, "Scarlet/Violet")
          url = pmonUrl pkmn <> "/moves/" <> T.pack (show gen)
      tags <- fetchTags (T.unpack url)
      -- If we get a 404, then the Pokemon doesn't exist in this game. Scalpel
      -- doesn't expose any HTTP request errors and I can't be bothered to
      -- actually parse the 404 page, so this will have to suffice
      if "Error 404: Page Not Found" `T.isInfixOf` T.pack (show tags)
        then pure []
        else do
          let tabListScraper :: Scraper Text [(Text, Text)]
              tabListScraper = chroot ("div" @: [hasClass "sv-tabs-tab-list"]) $ do
                ts <- texts ("a" @: [hasClass "sv-tabs-tab"])
                ids <- attrs "href" ("a" @: [hasClass "sv-tabs-tab"])
                pure $ zip ts (map (T.replace "#" "") ids)
          let tabTitles = fromJust $ scrape tabListScraper tags
          tabId <- case filter ((== targetTagName) . fst) tabTitles of
            [(_, elemId)] -> pure elemId
            _ -> throwIO $ userError $ T.unpack ("Could not find " <> targetTagName <> " tab for Pokemon named " <> pname pkmn)

          -- Step 2: Define convenience functions which can be reused
          let chrootSerialTabId :: SerialScraper Text a -> Scraper Text a
              chrootSerialTabId = chroot ("div" @: ["id" @= T.unpack tabId] // "div" @: [hasClass "grid-col"]) . inSerial
              findNextH3With :: [Text] -> SerialScraper Text ()
              findNextH3With snippets = seekNext $ do
                h3 <- text "h3"
                forM_ snippets $ \snip -> guard (snip `T.isInfixOf` h3)
              findNextPWith :: [Text] -> SerialScraper Text ()
              findNextPWith snippets = seekNext $ do
                p <- text "p"
                forM_ snippets $ \snip -> guard (snip `T.isInfixOf` p)
              findNextPTextSmallWith :: [Text] -> SerialScraper Text ()
              findNextPTextSmallWith snippets = seekNext $ do
                p <- text ("p" @: [hasClass "text-small"])
                forM_ snippets $ \snip -> guard (snip `T.isInfixOf` p)
              -- Fetch names of the forms and the div IDs of the corresponding tabs
              -- to search in
              getFormsAndTabIds :: SerialScraper Text [(Text, Text)]
              getFormsAndTabIds = do
                untilNext (matches "h3") $ seekNext $ chroot ("div" @: [hasClass "tabset-moves-game-form"] // "div" @: [hasClass "sv-tabs-tab-list"]) $ do
                  ts <- texts ("a" @: [hasClass "sv-tabs-tab"])
                  ids <- attrs "href" ("a" @: [hasClass "sv-tabs-tab"])
                  pure $ zip ts (map (T.replace "#" "") ids)
              -- Get a move table when no Form tabs are present
              withMoveTableNoForms :: Scraper Text a -> SerialScraper Text a
              withMoveTableNoForms scraper = do
                -- stepBack $ pure () -- Not sure if needed?
                seekNext $ chroot ("div" @: [hasClass "resp-scroll"] // "table" @: [hasClass "data-table"]) scraper

              -- Get a move table from a particular Form tab. This requires that the
              -- Form tabs are present and that the specific Form being searched for is
              -- present.
              withMoveTableFormFound :: Text -> Scraper Text a -> SerialScraper Text a
              withMoveTableFormFound tabId scraper = do
                -- Rewind first so that we can seekNext again
                stepBack $ pure ()
                seekNext $ chroot ("div" @: ["id" @= T.unpack tabId] // "table" @: [hasClass "data-table"]) scraper

              -- Get a move table searching for Forms, and run the given scraper on the
              -- table found. This generalises across all types of moves.
              runScraperWithForms :: Scraper Text [MoveLearn] -> SerialScraper Text [MoveLearn]
              runScraperWithForms scraper = do
                formsTabids <- getFormsAndTabIds <|> pure []
                case formsTabids of
                  [] -> withMoveTableNoForms scraper -- No forms, or moves are shared between forms
                  formsIds -> do
                    case filter
                      ( \(fm, _) ->
                          pform pkmn == Just fm
                            || (isNothing (pform pkmn) && pname pkmn == fm)
                      )
                      formsIds of
                      [] -> pure [] -- No moves for the given form
                      [(_, id')] -> withMoveTableFormFound id' scraper -- Form-specific moves

          -- Step 3: actual parsing
          let pkmnFullName = T.unpack $ case pform pkmn of
                Nothing -> pname pkmn
                Just fm -> pname pkmn <> " (" <> fm <> ")"

          -- Level up
          let lvlupScraper :: Scraper Text [MoveLearn]
              lvlupScraper = chrootSerialTabId $ do
                findNextH3With ["Moves learnt by level up"]
                findNextPTextSmallWith ["learns the following moves", "at the levels specified"]
                runScraperWithForms $ do
                  chroots ("tbody" // "tr") $ do
                    level <- readInt <$> text ("td" @: [hasClass "cell-num"])
                    name <- text ("td" @: [hasClass "cell-name"] // "a" @: [hasClass "ent-name"])
                    pure $ MoveLearn name (LevelUp level)
              noLvlupScraper = chrootSerialTabId $ do
                findNextH3With ["Moves learnt by level up"]
                findNextPWith ["does not learn any level up moves"]
                pure []
          lvlup <- case scrape (lvlupScraper <|> noLvlupScraper) tags of
            Just moves -> pure moves
            Nothing -> throwIO $ userError $ "Could not parse level up move section for " <> pkmnFullName

          -- Evolution
          let evolutionScraper = chrootSerialTabId $ do
                findNextH3With ["Moves learnt on evolution"]
                findNextPTextSmallWith ["learns the following moves when it evolves"]
                runScraperWithForms $ do
                  chroots ("tbody" // "tr") $ do
                    name <- text ("td" @: [hasClass "cell-name"] // "a" @: [hasClass "ent-name"])
                    pure $ MoveLearn name Evolution
              -- Just use fromMaybe [] here because not every Pokemon page has the
              -- evolution section
              evolution = fromMaybe [] $ scrape evolutionScraper tags

          -- Egg moves
          let hasEmsScraper = chrootSerialTabId $ do
                findNextH3With ["Egg moves"]
                findNextPTextSmallWith ["learns the following moves via breeding"]
                runScraperWithForms $ do
                  chroots ("tbody" // "tr") $ do
                    name <- text ("td" @: [hasClass "cell-name"] // "a" @: [hasClass "ent-name"])
                    pure $ MoveLearn name Egg
              noEmsScraper = chrootSerialTabId $ do
                findNextH3With ["Egg moves"]
                findNextPWith ["does not learn any moves by breeding"]
                pure []
          eggMoves <- case (pname pkmn, game) of
            ("Dipplin", SV) -> pure $ map (`MoveLearn` Egg) ["Defense Curl", "Rollout", "Recycle", "Sucker Punch"]
            ("Poltchageist", SV) -> pure []
            ("Sinistcha", SV) -> pure []
            ("Okidogi", SV) -> pure []
            ("Munkidori", SV) -> pure []
            ("Fezandipiti", SV) -> pure []
            ("Ogerpon", SV) -> pure []
            _ -> case scrape (hasEmsScraper <|> noEmsScraper) tags of
              Just moves -> pure moves
              Nothing -> throwIO $ userError $ "Could not parse egg move section for " <> pkmnFullName

          -- Tutor moves
          let tutorScraper = chrootSerialTabId $ do
                findNextH3With ["Move Tutor moves"]
                findNextPTextSmallWith ["can be taught these attacks", "from move tutors"]
                runScraperWithForms $ do
                  chroots ("tbody" // "tr") $ do
                    name <- text ("td" @: [hasClass "cell-name"] // "a" @: [hasClass "ent-name"])
                    pure $ MoveLearn name Tutor
              tutorMoves = fromMaybe [] $ scrape tutorScraper tags

          -- Reminder moves
          let reminderScraper = chrootSerialTabId $ do
                findNextH3With ["Moves learnt by reminder"]
                findNextPTextSmallWith ["learns the following extra moves when using the"]
                runScraperWithForms $ do
                  chroots ("tbody" // "tr") $ do
                    name <- text ("td" @: [hasClass "cell-name"] // "a" @: [hasClass "ent-name"])
                    pure $ MoveLearn name Reminder
              reminderMoves = fromMaybe [] $ scrape reminderScraper tags

          -- TM moves
          let tmScraper :: Scraper Text [MoveLearn]
              tmScraper = chrootSerialTabId $ do
                findNextH3With ["Moves learnt by TM"]
                findNextPTextSmallWith ["is compatible with these Technical Machines"]
                runScraperWithForms $ do
                  chroots ("tbody" // "tr") $ do
                    name <- text ("td" @: [hasClass "cell-name"] // "a" @: [hasClass "ent-name"])
                    pure $ MoveLearn name TM
              noTmScraper = chrootSerialTabId $ do
                findNextH3With ["Moves learnt by TM"]
                findNextPWith ["cannot be taught any TM moves"]
                pure []
          tmMoves <- case scrape (tmScraper <|> noTmScraper) tags of
            Just moves -> pure moves
            Nothing -> throwIO $ userError $ "Could not parse TM move section for " <> pkmnFullName

          -- TR moves
          let trScraper :: Scraper Text [MoveLearn]
              trScraper = chrootSerialTabId $ do
                findNextH3With ["Moves learnt by TR"]
                findNextPTextSmallWith ["is compatible with these Technical Records"]
                runScraperWithForms $ do
                  chroots ("tbody" // "tr") $ do
                    name <- text ("td" @: [hasClass "cell-name"] // "a" @: [hasClass "ent-name"])
                    pure $ MoveLearn name TM
              noTrScraper = chrootSerialTabId $ do
                findNextH3With ["Moves learnt by TR"]
                findNextPWith ["cannot be taught any TR moves"]
                pure []
          trMoves <- case scrape (trScraper <|> noTrScraper) tags of
            Just moves -> pure moves
            Nothing ->
              if game == SWSH
                then throwIO $ userError $ "Could not parse TR move section for " <> pkmnFullName
                else pure []

          -- Put it all together
          pure $ lvlup <> evolution <> eggMoves <> tutorMoves <> tmMoves <> trMoves <> reminderMoves

getEvolutionFamilies :: IO [[Text]]
getEvolutionFamilies = do
  let url = "https://pokemondb.net/evolution"
  let evoFamilyScraper :: Scraper Text [[Text]]
      evoFamilyScraper = chroots ("div" @: [hasClass "infocard-filter-block"]) $ do
        chroots ("div" @: [hasClass "infocard"]) $ do
          text ("a" @: [hasClass "ent-name"])
  results <- fromJust <$> scrapeURL url evoFamilyScraper
  let printLine = T.putStrLn . T.intercalate ", " . nub
  mapM_ printLine results
  pure results
