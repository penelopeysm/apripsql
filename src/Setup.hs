{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Setup
  ( clearDatabase,
    setupAbilities,
    setupEggGroups,
    setupTypes,
    setupGenderRatios,
    setupPokemon,
    setupMoveCategories,
    setupMoves,
    setupGames,
    setupLearnMethods,
    setupLearnsets,
    setupPosts,
    setupVotes,
    setupDiscord,
    setupTokens,
    setupEvolutionFamilies,
    setupLegality,
    setupNatures,
  )
where

import Control.Monad (forM_, unless, void)
import qualified Data.ByteString.Lazy as BL
import Data.Csv (HasHeader (..), decode, encode)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Time.Clock (UTCTime (..))
import qualified Data.Vector as V
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import Types
import Web

withConnection' :: (Connection -> IO a) -> IO a
withConnection' act = do
  flyProxyString <- fmap (T.encodeUtf8 . T.pack) <$> lookupEnv "FLY_PG_PROXY_CONN_STRING"
  conn <- case flyProxyString of
    Just fps -> do
      connectPostgreSQL fps
    _ ->
      connect $
        ConnectInfo
          { connectHost = "localhost",
            connectPort = 5432,
            connectUser = "pysm",
            connectPassword = "",
            connectDatabase = "apri"
          }
  result <- act conn
  close conn
  pure result

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs =
  let (ys, zs) = splitAt n xs
   in ys : chunk n zs

clearDatabase :: IO ()
clearDatabase = withConnection' $ \conn -> do
  execute_ conn [sql|DROP TABLE IF EXISTS learnsets|]
  execute_ conn [sql|DROP TABLE IF EXISTS pokemon|]
  execute_ conn [sql|DROP TABLE IF EXISTS moves|]
  execute_ conn [sql|DROP TABLE IF EXISTS abilities|]
  execute_ conn [sql|DROP TABLE IF EXISTS egg_groups|]
  execute_ conn [sql|DROP TABLE IF EXISTS types|]
  execute_ conn [sql|DROP TABLE IF EXISTS gender_ratios|]
  execute_ conn [sql|DROP TABLE IF EXISTS move_categories|]
  pure ()

createAbilitiesCSVIfNotExists :: IO ()
createAbilitiesCSVIfNotExists = do
  let fname = "abilities.csv"
  fileExists <- doesFileExist fname
  unless fileExists $ do
    abilities <- Web.getAllAbilities
    flavorTexts <- mapM Web.getAbilityFlavorText abilities
    let abilitiesAndTexts = zip abilities flavorTexts
    BL.writeFile fname $ encode abilitiesAndTexts

setupAbilities :: IO ()
setupAbilities = do
  createAbilitiesCSVIfNotExists
  eitherAbilitiesAndTexts <- decode NoHeader <$> BL.readFile "abilities.csv"
  case eitherAbilitiesAndTexts of
    Left err -> error err
    Right (abilitiesAndTexts :: V.Vector (Text, Text)) -> do
      withConnection' $ \conn -> do
        -- (Re)create table
        execute_ conn [sql|DROP TABLE IF EXISTS abilities|]
        execute_
          conn
          [sql|
              CREATE TABLE IF NOT EXISTS abilities (
                id SERIAL PRIMARY KEY,
                name text UNIQUE NOT NULL,
                flavor_text text NOT NULL
              )|]
        -- Populate table
        void $
          executeMany
            conn
            [sql|
                INSERT INTO abilities (name, flavor_text) VALUES (?,?)
            |]
            (V.toList abilitiesAndTexts)

setupEggGroups :: IO ()
setupEggGroups = do
  let eggGroups :: [Only Text] = map Only ["Monster", "Water 1", "Bug", "Flying", "Field", "Fairy", "Grass", "Human-Like", "Water 3", "Mineral", "Amorphous", "Water 2", "Ditto", "Dragon", "Undiscovered"]
  withConnection' $ \conn -> do
    -- (Re)create table
    execute_ conn [sql|DROP TABLE IF EXISTS egg_groups|]
    execute_
      conn
      [sql|
      CREATE TABLE IF NOT EXISTS egg_groups (
        id SERIAL PRIMARY KEY,
        name text UNIQUE NOT NULL
      )|]
    void $
      executeMany
        conn
        [sql|INSERT INTO egg_groups (name) VALUES (?)|]
        eggGroups

setupTypes :: IO ()
setupTypes = do
  let types :: [Only Text] = map Only ["Normal", "Fire", "Water", "Electric", "Grass", "Ice", "Fighting", "Poison", "Ground", "Flying", "Psychic", "Bug", "Rock", "Ghost", "Dragon", "Dark", "Steel", "Fairy"]
  withConnection' $ \conn -> do
    -- (Re)create table
    execute_ conn [sql|DROP TABLE IF EXISTS types|]
    execute_
      conn
      [sql|
      CREATE TABLE IF NOT EXISTS types (
        id SERIAL PRIMARY KEY,
        name text UNIQUE NOT NULL
      )|]
    void $
      executeMany
        conn
        [sql|INSERT INTO types (name) VALUES (?)|]
        types

setupGenderRatios :: IO ()
setupGenderRatios = do
  let genderRatios :: [Only Text] = map (Only . makeGenderRatioText) [minBound .. maxBound]
  withConnection' $ \conn -> do
    -- (Re)create table
    execute_ conn [sql|DROP TABLE IF EXISTS gender_ratios|]
    execute_
      conn
      [sql|
      CREATE TABLE IF NOT EXISTS gender_ratios (
        id SERIAL PRIMARY KEY,
        name text UNIQUE NOT NULL
      )|]
    void $
      executeMany
        conn
        [sql|INSERT INTO gender_ratios (name) VALUES (?)|]
        genderRatios

getAllAbilitiesFromDb :: IO (Map Text Int)
getAllAbilitiesFromDb = do
  list <- withConnection' $ \conn -> do
    query_ conn [sql|SELECT name, id FROM abilities|]
  pure $ M.fromList list

getAllEggGroupsFromDb :: IO (Map Text Int)
getAllEggGroupsFromDb = do
  list <- withConnection' $ \conn -> do
    query_ conn [sql|SELECT name, id FROM egg_groups|]
  pure $ M.fromList list

getAllTypesFromDb :: IO (Map Text Int)
getAllTypesFromDb = do
  list <- withConnection' $ \conn -> do
    query_ conn [sql|SELECT name, id FROM types|]
  pure $ M.fromList list

-- Too lazy to re-parse the text into our sum type
genderRatioMap :: Map GenderRatio Int
genderRatioMap =
  M.fromList $ map (\gr -> (gr, succ $ fromEnum gr)) [minBound .. maxBound]

tealMaskPokemonPartial :: [PokemonPartial]
tealMaskPokemonPartial =
  [ PokemonPartial {pname = "Dipplin", pform = Nothing, pformNum = 0, pndex = 1011, ptype1 = "Grass", ptype2 = Just "Dragon", pmonUrl = "https://pokemondb.net/pokedex/dipplin"},
    PokemonPartial {pname = "Poltchageist", pform = Nothing, pformNum = 0, pndex = 1012, ptype1 = "Grass", ptype2 = Just "Ghost", pmonUrl = "https://pokemondb.net/pokedex/poltchageist"},
    PokemonPartial {pname = "Sinistcha", pform = Nothing, pformNum = 0, pndex = 1013, ptype1 = "Grass", ptype2 = Just "Ghost", pmonUrl = "https://pokemondb.net/pokedex/sinistcha"},
    PokemonPartial {pname = "Okidogi", pform = Nothing, pformNum = 0, pndex = 1014, ptype1 = "Poison", ptype2 = Just "Fighting", pmonUrl = "https://pokemondb.net/pokedex/okidogi"},
    PokemonPartial {pname = "Munkidori", pform = Nothing, pformNum = 0, pndex = 1015, ptype1 = "Poison", ptype2 = Just "Psychic", pmonUrl = "https://pokemondb.net/pokedex/munkidori"},
    PokemonPartial {pname = "Fezandipiti", pform = Nothing, pformNum = 0, pndex = 1016, ptype1 = "Poison", ptype2 = Just "Fairy", pmonUrl = "https://pokemondb.net/pokedex/fezandipiti"},
    PokemonPartial {pname = "Ogerpon", pform = Just "Teal Mask", pformNum = 0, pndex = 1017, ptype1 = "Grass", ptype2 = Nothing, pmonUrl = "https://pokemondb.net/pokedex/ogerpon"},
    PokemonPartial {pname = "Ogerpon", pform = Just "Wellspring Mask", pformNum = 1, pndex = 1017, ptype1 = "Grass", ptype2 = Just "Water", pmonUrl = "https://pokemondb.net/pokedex/ogerpon"},
    PokemonPartial {pname = "Ogerpon", pform = Just "Hearthflame Mask", pformNum = 2, pndex = 1017, ptype1 = "Grass", ptype2 = Just "Fire", pmonUrl = "https://pokemondb.net/pokedex/ogerpon"},
    PokemonPartial {pname = "Ogerpon", pform = Just "Cornerstone Mask", pformNum = 3, pndex = 1017, ptype1 = "Grass", ptype2 = Just "Rock", pmonUrl = "https://pokemondb.net/pokedex/ogerpon"}
  ]

makeUniqueName :: Text -> Maybe Text -> Text
makeUniqueName nm' fm =
  let nm = case nm' of
        "Mr. Mime" -> "mr-mime"
        "Mr. Rime" -> "mr-rime"
        "Mime Jr." -> "mime-jr"
        "Farfetch'd" -> "farfetchd"
        "Sirfetch'd" -> "sirfetchd"
        p -> p
   in T.replace " " "-" $ T.strip $ T.toLower $ case fm of
        Nothing -> nm
        Just f
          -- Darmanitan is SPECIAL
          | f == "Standard Mode" -> nm
          | f == "Zen Mode" -> nm <> "-zen"
          | f == "Galarian Standard Mode" -> nm <> "-galar"
          | f == "Galarian Zen Mode" -> nm <> "-galar-zen"
          -- Catch-all cases
          | "Alolan " `T.isPrefixOf` f -> nm <> "-alola"
          | "Galarian " `T.isPrefixOf` f -> nm <> "-galar"
          | "Hisuian " `T.isPrefixOf` f -> nm <> "-hisui"
          | "Paldean " `T.isPrefixOf` f -> nm <> "-paldea"
          | "Hoopa " `T.isPrefixOf` f -> nm <> "-" <> T.replace "Hoopa " "" f
          | "Crowned " `T.isPrefixOf` f -> nm <> "-crowned"
          | "Mega " `T.isPrefixOf` f && "X" `T.isSuffixOf` f -> nm <> "-mega-x"
          | "Mega " `T.isPrefixOf` f && "Y" `T.isSuffixOf` f -> nm <> "-mega-y"
          | "Mega " `T.isPrefixOf` f -> nm <> "-mega"
          | "Primal " `T.isPrefixOf` f -> nm <> "-primal"
          | "Partner " `T.isPrefixOf` f -> nm <> "-partner"
          | " Forme" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Forme" "" f
          | " -Striped Form" `T.isSuffixOf` f -> nm <> "-" <> T.replace " -Striped Form" "" f
          | " Form" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Form" "" f
          | " Breed" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Breed" "" f
          | " Cloak" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Cloak" "" f
          | " Rider" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Rider" "" f
          | " Rotom" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Rotom" "" f
          | " Necrozma" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Necrozma" "" f
          | " Kyurem" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Kyurem" "" f
          | " Mask" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Mask" "" f
          | " Mode" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Mode" "" f
          | " Size" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Size" "" f
          | " Plumage" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Plumage" "" f
          | " Style" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Style" "" f
          | " Face" `T.isSuffixOf` f -> nm <> "-" <> T.replace " Face" "" f
          -- Specific stuff
          | f == "Ash-Greninja" -> "greninja-ash"
          | f == "Own Tempo Rockruff" -> "rockruff-dusk"
          | f == "Male" -> nm <> "-male"
          | f == "Hero of Many Battles" -> nm -- Z/Z base forms
          | f == "Female" -> nm <> "-female"
          | f == "Eternamax" -> nm <> "-eternamax"
          | f == "Bloodmoon" -> nm <> "-bloodmoon"
          | f == "Family of Three" -> nm <> "-family-of-three"
          | f == "Family of Four" -> nm <> "-family-of-four"
          | otherwise -> error $ "Don't know what to do with: " <> show nm <> " (" <> show f <> ")"

createPokemonCsvIfNotExists :: IO ()
createPokemonCsvIfNotExists = do
  let fname = "pokemon.csv"
  fileExists <- doesFileExist fname
  unless fileExists $ do
    -- Fetch data
    abilitiesMap <- getAllAbilitiesFromDb
    eggGroupsMap <- getAllEggGroupsFromDb
    typesMap <- getAllTypesFromDb
    ppkmnUpTo1010 <- getAllPokemonPartial
    let ppkmn = ppkmnUpTo1010 ++ tealMaskPokemonPartial
    let makePkmnSql :: Pokemon -> (Text, Maybe Text, Text, Int, Int, Maybe Int, Int, Int, Maybe Int, Maybe Int)
        -- Throws an error if any of the maps are missing
        makePkmnSql pkmn =
          ( name pkmn,
            form pkmn,
            makeUniqueName (name pkmn) (form pkmn),
            ndex pkmn,
            typesMap M.! type1 pkmn,
            case type2 pkmn of
              Nothing -> Nothing
              Just t -> Just $ typesMap M.! t,
            genderRatioMap M.! genderRatio pkmn,
            eggGroupsMap M.! eggGroup1 pkmn,
            case eggGroup2 pkmn of
              Nothing -> Nothing
              Just eg -> Just $ eggGroupsMap M.! eg,
            case hiddenAbility pkmn of
              Nothing -> Nothing
              Just ha -> Just $ abilitiesMap M.! ha
          )
    pkmnSql <- mapM (fmap makePkmnSql . Web.getPokemon) ppkmn
    BL.writeFile fname $ encode pkmnSql

setupPokemon :: IO ()
setupPokemon = do
  createPokemonCsvIfNotExists
  eitherPkmnSql <- decode NoHeader <$> BL.readFile "pokemon.csv"
  case eitherPkmnSql of
    Left err -> error err
    Right (pkmnSql :: V.Vector (Text, Maybe Text, Text, Int, Int, Maybe Int, Int, Int, Maybe Int, Maybe Int)) -> do
      -- Add to database
      withConnection' $ \conn -> do
        -- (Re)create table
        execute_ conn [sql|DROP TABLE IF EXISTS pokemon|]
        execute_
          conn
          [sql|
            CREATE TABLE IF NOT EXISTS pokemon (
              id SERIAL PRIMARY KEY,
              name text NOT NULL,
              form text,
              unique_name text UNIQUE NOT NULL,
              ndex integer NOT NULL,
              type1_id integer NOT NULL REFERENCES types(id),
              type2_id integer REFERENCES types(id),
              gr_id integer NOT NULL REFERENCES gender_ratios(id),
              eg1_id integer NOT NULL REFERENCES egg_groups(id),
              eg2_id integer REFERENCES egg_groups(id),
              ha_id integer REFERENCES abilities(id)
            )|]
        -- Populate table
        let batches = chunk 50 (V.toList pkmnSql)
        forM_ batches $ \batch -> do
          T.putStrLn "Inserting batch of 50..."
          executeMany
            conn
            [sql|BEGIN;
                   INSERT INTO pokemon
                     (name, form, unique_name, ndex, type1_id, type2_id, gr_id, eg1_id, eg2_id, ha_id)
                     VALUES (?,?,?,?,?,?,?,?,?,?);
                   COMMIT;|]
            batch

createMovesCsvIfNotExists :: IO ()
createMovesCsvIfNotExists = do
  let fname = "moves.csv"
  fileExists <- doesFileExist fname
  unless fileExists $ do
    let tealMaskMoves =
          [ Move "Syrup Bomb" "Grass" Special "The user sets off an explosion of sticky candy syrup, which coats the target and causes the target's Speed stat to drop each turn for three turns.",
            Move "Ivy Cudgel" "Grass" Physical "The user strikes with an ivy-wrapped cudgel. This move's type changes depending on the mask worn by the user, and it has a heightened chance of landing a critical hit.",
            Move "Matcha Gotcha" "Grass" Special "The user fires a blast of tea that it mixed. The user's HP is restored by up to half the damage taken by the target. This may also leave the target with a burn.",
            Move "Blood Moon" "Normal" Special "The user unleashes the full brunt of its spirit from a full moon that shines as red as blood. This move can't be used twice in a row."
          ]
    -- Fetch data
    moves <- (<> tealMaskMoves) <$> Web.getAllMoves

    typesMap <- getAllTypesFromDb
    let moveToTuple :: Move -> (Text, Int, Int, Text)
        moveToTuple move =
          ( mName move,
            typesMap M.! mType move,
            succ $ fromEnum (mCategory move),
            mFlavorText move
          )
    BL.writeFile fname $ encode (map moveToTuple moves)

setupMoveCategories :: IO ()
setupMoveCategories = do
  withConnection' $ \conn -> do
    -- (Re)create table
    execute_ conn [sql|DROP TABLE IF EXISTS move_categories|]
    execute_
      conn
      [sql|
      CREATE TABLE IF NOT EXISTS move_categories (
        id SERIAL PRIMARY KEY,
        name text UNIQUE NOT NULL
      )|]
    -- Populate table
    void $
      executeMany
        conn
        [sql|INSERT INTO move_categories (name) VALUES (?)|]
        (map (Only . show) [minBound :: MoveCategory .. maxBound])

setupMoves :: IO ()
setupMoves = do
  createMovesCsvIfNotExists
  eitherMoveSql <- decode NoHeader <$> BL.readFile "moves.csv"
  case eitherMoveSql of
    Left err -> error err
    Right (moveSql' :: V.Vector (Text, Int, Int, Text)) -> do
      let moveSql = V.toList moveSql'
      -- Add to database
      withConnection' $ \conn -> do
        -- (Re)create table
        execute_ conn [sql|DROP TABLE IF EXISTS moves|]
        execute_
          conn
          [sql|
          CREATE TABLE IF NOT EXISTS moves (
            id SERIAL PRIMARY KEY,
            name text UNIQUE NOT NULL,
            type_id integer NOT NULL REFERENCES types(id),
            category_id integer NOT NULL REFERENCES move_categories(id),
            flavor_text text NOT NULL
          )|]
        -- Populate table
        let batches = chunk 50 moveSql
        forM_ batches $ \batch -> do
          T.putStrLn "Inserting batch of 50..."
          executeMany
            conn
            [sql|BEGIN;
                 INSERT INTO moves (name, type_id, category_id, flavor_text) VALUES (?,?,?,?);
                 COMMIT;|]
            batch

getMoveIdsFromDb :: IO (Map Text Int)
getMoveIdsFromDb = do
  list <- withConnection' $ \conn -> do
    query_ conn [sql|SELECT name, id FROM moves|]
  pure $ M.fromList list

setupGames :: IO ()
setupGames = do
  let games :: [Only Text] = map (Only . makeGameText) [minBound .. maxBound]
  withConnection' $ \conn -> do
    -- (Re)create table
    execute_ conn [sql|DROP TABLE IF EXISTS games|]
    execute_
      conn
      [sql|
      CREATE TABLE IF NOT EXISTS games (
        id SERIAL PRIMARY KEY,
        name text UNIQUE NOT NULL
      )|]
    void $
      executeMany
        conn
        [sql|INSERT INTO games (name) VALUES (?)|]
        games

getGameIdsFromDb :: IO (Map Text Int)
getGameIdsFromDb = do
  list <- withConnection' $ \conn -> do
    query_ conn [sql|SELECT name, id FROM games|]
  pure $ M.fromList list

setupLearnMethods :: IO ()
setupLearnMethods = do
  let lms :: [Only Text] = map Only ["Level up", "Evolution", "Tutor", "Egg", "TM", "Reminder"]
  withConnection' $ \conn -> do
    -- (Re)create table
    execute_ conn [sql|DROP TABLE IF EXISTS learn_methods|]
    execute_
      conn
      [sql|
      CREATE TABLE IF NOT EXISTS learn_methods (
        id SERIAL PRIMARY KEY,
        name text UNIQUE NOT NULL
      )|]
    void $
      executeMany
        conn
        [sql|INSERT INTO learn_methods (name) VALUES (?)|]
        lms

-- | Need to manually code this because LearnMethod does not have a valid Enum
-- instance
learnMethodsToInt :: LearnMethod -> Int
learnMethodsToInt method = case method of
  LevelUp _ -> 1
  Evolution -> 2
  Tutor -> 3
  Egg -> 4
  TM -> 5
  Reminder -> 6

createLearnsetsCsvIfNotExists :: IO ()
createLearnsetsCsvIfNotExists = do
  let fname = "learnsets.csv"
  fileExists <- doesFileExist fname
  unless fileExists $ do
    allPokemonPartial <- (<> tealMaskPokemonPartial) <$> getAllPokemonPartial
    moveIds <- getMoveIdsFromDb
    gameIds <- getGameIdsFromDb

    let nDrop = 0
    let nTotal = length allPokemonPartial
    let pkmnPartial = drop nDrop allPokemonPartial
    -- Loop over Pokemon
    forM_ (zip [nDrop + 1 ..] pkmnPartial) $ \(n, p) -> do
      let progress = T.pack $ "(" <> show n <> "/" <> show nTotal <> ")"
      let pkmnName = case pform p of
            Nothing -> pname p
            Just f -> pname p <> " (" <> f <> ")"
      T.putStrLn $ progress <> " Fetching learnset for " <> pkmnName <> "..."
      pkmnId <- withConnection' $ \conn -> do
        case pform p of
          Nothing -> do
            [Only pkmnId] <- query conn [sql|SELECT id FROM pokemon WHERE name = ? AND form IS NULL;|] (Only $ pname p)
            pure pkmnId
          Just f -> do
            [Only pkmnId] <- query conn [sql|SELECT id FROM pokemon WHERE name = ? AND form = ?;|] (pname p, f)
            pure pkmnId
      -- Loop over games
      forM_ [USUM, SWSH, BDSP, SV] $ \game -> do
        T.putStrLn $ "   " <> makeGameText game
        learnset <- Web.getLearnset p game
        let learnsetSql :: [(Int, Int, Int, Int, Maybe Int)]
            learnsetSql =
              map
                ( \mld ->
                    ( pkmnId,
                      gameIds M.! makeGameText game,
                      moveIds M.! (case mldMoveName mld of "Vice Grip" -> "Vise Grip"; m -> m),
                      learnMethodsToInt $ mldLearnMethod mld,
                      case mldLearnMethod mld of
                        LevelUp lvl -> Just lvl
                        _ -> Nothing
                    )
                )
                learnset
        BL.appendFile fname $ encode learnsetSql

setupLearnsets :: IO ()
setupLearnsets = do
  createLearnsetsCsvIfNotExists
  eitherLearnsetsSql <- decode NoHeader <$> BL.readFile "learnsets.csv"
  case eitherLearnsetsSql of
    Left err -> error err
    Right (learnsetsSql :: V.Vector (Int, Int, Int, Int, Maybe Int)) -> do
      withConnection' $ \conn -> do
        -- (Re)create table
        execute_ conn [sql|DROP TABLE IF EXISTS learnsets|]
        void $
          execute_
            conn
            [sql|
              CREATE TABLE IF NOT EXISTS learnsets (
                pokemon_id integer NOT NULL REFERENCES pokemon(id),
                game_id integer NOT NULL REFERENCES games(id),
                move_id integer NOT NULL REFERENCES moves(id),
                learn_method_id integer NOT NULL REFERENCES learn_methods(id),
                level integer
              )|]

      -- Populate table
      withConnection' $ \conn -> do
        let batches = chunk 50 (V.toList learnsetsSql)
        forM_ batches $ \batch -> do
          T.putStrLn "Inserting batch of 50..."
          executeMany
            conn
            [sql|BEGIN;
                 INSERT INTO learnsets (pokemon_id, game_id, move_id, learn_method_id, level) VALUES (?,?,?,?,?);
                 COMMIT;|]
            batch

setupPosts :: IO ()
setupPosts = do
  eitherPostsSql <- decode HasHeader <$> BL.readFile "posts.csv"
  -- id,url,title,body,submitter,time,flair,hit,truth
  case eitherPostsSql of
    Left err -> error err
    Right (postsSql :: V.Vector (Text, Text, Text, Text, Text, Text, Maybe Text, Maybe Float, Maybe Float)) -> do
      let makePostTuple :: (Text, Text, Text, Text, Text, Text, Maybe Text, Maybe Float, Maybe Float) -> (Text, Text, Text, Text, Text, UTCTime, Maybe Text, Maybe Bool)
          makePostTuple (id', url, title, body, submitter, time, flair, hit, _) =
            (id', url, title', body', submitter, time', flair, hit')
            where
              time' = read $ T.unpack time
              hit' = case hit of
                Just f -> Just $ f > 0.5
                Nothing -> Nothing
              unEscape = T.replace "\\r" "\r" . T.replace "\\n" "\n"
              title' = unEscape title
              body' = unEscape body
      let postsSql' = map makePostTuple $ V.toList postsSql
      withConnection' $ \conn -> do
        -- (Re)create table
        execute_ conn [sql|DROP TABLE IF EXISTS posts|]
        void $
          execute_
            conn
            [sql|
              CREATE TABLE IF NOT EXISTS posts (
                id TEXT NOT NULL PRIMARY KEY,
                url TEXT NOT NULL,
                title TEXT NOT NULL,
                body TEXT NOT NULL,
                submitter TEXT NOT NULL,
                utc_time TIMESTAMP WITH TIME ZONE NOT NULL,
                flair TEXT NOT NULL,
                hit BOOLEAN
              )|]
      -- Populate table
      withConnection' $ \conn -> do
        let batches = chunk 50 postsSql'
        forM_ batches $ \batch -> do
          T.putStrLn "Inserting batch of 50..."
          executeMany
            conn
            [sql|BEGIN;
                 INSERT INTO posts (id, url, title, body, submitter, utc_time, flair, hit) VALUES (?,?,?,?,?,?,?,?);
                 COMMIT;|]
            batch

setupVotes :: IO ()
setupVotes = do
  eitherVotesSql <- decode HasHeader <$> BL.readFile "votes.csv"
  case eitherVotesSql of
    Left err -> error err
    Right (votesSql :: V.Vector (Int, Text, Text, Int)) -> do
      withConnection' $ \conn -> do
        -- (Re)create table
        execute_ conn [sql|DROP TABLE IF EXISTS votes|]
        void $
          execute_
            conn
            [sql|
              CREATE TABLE IF NOT EXISTS votes (
                id SERIAL PRIMARY KEY,
                post_id TEXT NOT NULL REFERENCES posts(id),
                username TEXT NOT NULL,
                vote BOOLEAN NOT NULL
              )|]
      let votesSql' = map (\(_, postId, username, vote) -> (postId, username, vote == 1)) $ V.toList votesSql
      -- Populate table
      withConnection' $ \conn -> do
        let batches = chunk 50 votesSql'
        forM_ batches $ \batch -> do
          T.putStrLn "Inserting batch of 50..."
          executeMany
            conn
            [sql|BEGIN;
                 INSERT INTO votes (post_id, username, vote) VALUES (?,?,?);
                 COMMIT;|]
            batch

setupDiscord :: IO ()
setupDiscord = do
  eitherDiscordSql <- decode HasHeader <$> BL.readFile "discord.csv"
  case eitherDiscordSql of
    Left err -> error err
    Right (discordSql :: V.Vector (Text, Text, Text)) -> do
      withConnection' $ \conn -> do
        -- (Re)create table
        execute_ conn [sql|DROP TABLE IF EXISTS discord|]
        void $
          execute_
            conn
            [sql|
              CREATE TABLE IF NOT EXISTS discord (
                post_id TEXT NOT NULL REFERENCES posts(id),
                channel_id TEXT NOT NULL,
                message_id TEXT NOT NULL
              )|]
      -- Populate table
      withConnection' $ \conn -> do
        let batches = chunk 50 (V.toList discordSql)
        forM_ batches $ \batch -> do
          T.putStrLn "Inserting batch of 50..."
          executeMany
            conn
            [sql|BEGIN;
                 INSERT INTO discord (post_id, channel_id, message_id) VALUES (?,?,?);
                 COMMIT;|]
            batch

setupTokens :: IO ()
setupTokens = do
  withConnection' $ \conn -> do
    -- (Re)create table
    execute_ conn [sql|DROP TABLE IF EXISTS tokens|]
    void $
      execute_
        conn
        [sql|
          CREATE TABLE IF NOT EXISTS tokens (
            id TEXT NOT NULL PRIMARY KEY,
            token TEXT NOT NULL,
            token_type TEXT NOT NULL,
            expires_at TIMESTAMP WITH TIME ZONE NOT NULL,
            scopes TEXT NOT NULL,
            refresh_token TEXT NOT NULL
          )|]

setupEvolutionFamilies :: IO ()
setupEvolutionFamilies = do
  evolutionFamilies <- Web.getEvolutionFamilies
  withConnection' $ \conn -> do
    void $
      execute_
        conn
        [sql|ALTER TABLE pokemon ADD COLUMN evolution_family_id INTEGER;|]
  forM_ (zip [1 :: Int ..] evolutionFamilies) $ \(i, family) -> do
    withConnection' $ \conn -> do
      execute
        conn
        [sql|UPDATE pokemon SET evolution_family_id = ? WHERE name IN ?;|]
        (i, In family)

setupLegality :: IO ()
setupLegality = do
  eitherVotesSql <- decode NoHeader <$> BL.readFile "legality.csv"
  case eitherVotesSql of
    Left err -> error err
    Right (legalitySql :: V.Vector (Text, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)) -> do
      withConnection' $ \conn -> do
        -- (Re)create table
        execute_ conn [sql|DROP TABLE IF EXISTS legality|]
        void $
          execute_
            conn
            [sql|
              CREATE TABLE IF NOT EXISTS legality (
                pokemon_id INTEGER NOT NULL REFERENCES pokemon(id),
                bank_beast BOOLEAN NOT NULL,
                bank_dream BOOLEAN NOT NULL,
                bank_apri BOOLEAN NOT NULL,
                bank_safari BOOLEAN NOT NULL,
                bank_sport BOOLEAN NOT NULL,
                home_beast BOOLEAN NOT NULL,
                home_dream BOOLEAN NOT NULL,
                home_apri BOOLEAN NOT NULL,
                home_safari BOOLEAN NOT NULL,
                home_sport BOOLEAN NOT NULL
              )|]
      let p = (== 1) -- Convert Int to Bool
      let legalitySql' =
            map
              ( \(pkmnName, a, b, c, d, e, f, g, h, i, j) ->
                  (pkmnName, (p a, p b, p c, p d, p e, p f, p g, p h, p i, p j, pkmnName))
              )
              $ V.toList legalitySql
      -- Populate table
      withConnection' $ \conn -> do
        forM_ legalitySql' $ \(name, tup) -> do
          -- Check that an ID exists
          n <- query conn [sql|SELECT id FROM pokemon WHERE unique_name = ?;|] (Only name)
          case n of
            [Only (_ :: Int)] ->
              execute
                conn
                [sql|INSERT INTO legality (pokemon_id, bank_beast, bank_dream, bank_apri, bank_safari, bank_sport, home_beast, home_dream, home_apri, home_safari, home_sport)
                           SELECT p.id, ? as bank_beast, ? as bank_dream, ? as bank_apri, ? as bank_safari,
                                  ? as bank_sport, ? as home_beast, ? as home_dream, ? as home_apri,
                                  ? as home_safari, ? as home_sport
                           FROM pokemon p WHERE p.unique_name = ?;|]
                tup
            _ -> error $ show tup

setupNatures :: IO ()
setupNatures = do
  eitherNaturesSql <- decode HasHeader <$> BL.readFile "natures.csv"
  case eitherNaturesSql of
    Left err -> error err
    Right (naturesSql :: V.Vector (Text, Text, Text, Text, Text)) -> do
      withConnection' $ \conn -> do
        -- (Re)create table
        execute_ conn [sql|DROP TABLE IF EXISTS natures|]
        void $
          execute_
            conn
            [sql|
            CREATE TABLE natures (
                pokemon_id INTEGER NOT NULL REFERENCES pokemon(id),
                penny TEXT,
                jemma_swsh TEXT,
                jemma_bdsp TEXT,
                jemma_g7 TEXT
            );|]
      let p t = if T.null t then Nothing else Just t
      let naturesSql' =
            map
              ( \(pkmnName, a, b, c, d) ->
                  (pkmnName, (p a, p b, p c, p d, pkmnName))
              )
              $ V.toList naturesSql
      -- Populate table
      withConnection' $ \conn -> do
        forM_ naturesSql' $ \(name, tup) -> do
          -- Check that an ID exists
          n <- query conn [sql|SELECT id FROM pokemon WHERE unique_name = ?;|] (Only name)
          case n of
            [Only (_ :: Int)] ->
              execute
                conn
                [sql|INSERT INTO natures (pokemon_id, penny, jemma_swsh, jemma_bdsp, jemma_g7)
                           SELECT p.id, ? as penny, ? as jemma_swsh, ? as jemma_bdsp, ? as jemma_g7
                           FROM pokemon p WHERE p.unique_name = ?;|]
                tup
            _ -> error $ T.unpack name
