{-# LANGUAGE QuasiQuotes #-}

module Setup.Database (setupDatabase) where

import Control.Monad (forM_, unless, void)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Copy
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Types (Identifier (..))

emptyDatabase :: Connection -> IO ()
emptyDatabase conn = do
  let allTables =
        [ "abilities" :: Text,
          "egg_groups",
          "evolutions",
          "games",
          "gender_ratios",
          "learn_methods",
          "learnsets",
          "legality",
          "move_categories",
          "moves",
          "natures",
          "pokemon",
          "types"
        ]
  forM_ allTables $ \table -> do
    execute conn "DROP TABLE IF EXISTS ? CASCADE" (Only $ Identifier table)

setupTable :: FilePath -> String -> Query -> Connection -> IO ()
setupTable path tableName createTableQuery conn = do
  let tableParam = Only $ Identifier $ T.pack tableName
  putStr $ "Loading " ++ path ++ " into " ++ tableName ++ "... "
  execute conn createTableQuery tableParam
  copy conn "COPY ? FROM STDIN WITH (FORMAT csv, HEADER)" tableParam
  BS.readFile path >>= putCopyData conn
  n <- putCopyEnd conn
  putStrLn $ "Loaded " ++ show n ++ " rows."

setupDatabase :: Connection -> IO ()
setupDatabase conn = do
  -- Clean up.
  emptyDatabase conn
  n :: [Only Text] <- query_ conn "SELECT tablename FROM pg_tables WHERE schemaname = 'public'"
  unless (null n) $ error "Database not empty"
  -- Create tables
  void $ do
    setupTable
      "csv/abilities.csv"
      "abilities"
      [sql|
      CREATE TABLE ? (
        id INTEGER NOT NULL PRIMARY KEY,
        name TEXT NOT NULL UNIQUE,
        flavor_text TEXT NOT NULL
      )|]
      conn
    setupTable
      "csv/egg-groups.csv"
      "egg_groups"
      [sql|
      CREATE TABLE ? (
        id INTEGER NOT NULL PRIMARY KEY,
        name TEXT NOT NULL UNIQUE
      )|]
      conn
    setupTable
      "csv/games.csv"
      "games"
      [sql|
      CREATE TABLE ? (
        id INTEGER NOT NULL PRIMARY KEY,
        name TEXT NOT NULL UNIQUE
      )|]
      conn
    setupTable
      "csv/gender-ratios.csv"
      "gender_ratios"
      [sql|
      CREATE TABLE ? (
        id INTEGER NOT NULL PRIMARY KEY,
        name TEXT NOT NULL UNIQUE
      )|]
      conn
    setupTable
      "csv/move-categories.csv"
      "move_categories"
      [sql|
      CREATE TABLE ? (
        id INTEGER NOT NULL PRIMARY KEY,
        name TEXT NOT NULL UNIQUE
      )|]
      conn
    setupTable
      "csv/learn-methods.csv"
      "learn_methods"
      [sql|
      CREATE TABLE ? (
        id INTEGER NOT NULL PRIMARY KEY,
        name TEXT NOT NULL UNIQUE
      )|]
      conn
    setupTable
      "csv/types.csv"
      "types"
      [sql|
      CREATE TABLE ? (
        id INTEGER NOT NULL PRIMARY KEY,
        name TEXT NOT NULL UNIQUE
      )|]
      conn
    setupTable
      "csv/moves.csv"
      "moves"
      [sql|
      CREATE TABLE ? (
        id INTEGER NOT NULL PRIMARY KEY,
        name TEXT NOT NULL UNIQUE,
        type_id INTEGER NOT NULL REFERENCES types(id),
        category_id INTEGER NOT NULL REFERENCES move_categories(id),
        flavor_text TEXT NOT NULL,
        power INTEGER,
        accuracy INTEGER,
        pp INTEGER
      )|]
      conn
    setupTable
      "csv/pokemon.csv"
      "pokemon"
      [sql|
      CREATE TABLE ? (
        id INTEGER NOT NULL PRIMARY KEY,
        name TEXT NOT NULL,
        form TEXT,
        unique_name TEXT NOT NULL UNIQUE,
        ndex INTEGER NOT NULL,
        galar_dex INTEGER,
        ioa_dex INTEGER,
        ct_dex INTEGER,
        paldea_dex INTEGER,
        tm_dex INTEGER,
        type1_id INTEGER NOT NULL REFERENCES types(id),
        type2_id INTEGER REFERENCES types(id),
        hp INTEGER NOT NULL,
        atk INTEGER NOT NULL,
        def INTEGER NOT NULL,
        spa INTEGER NOT NULL,
        spd INTEGER NOT NULL,
        spe INTEGER NOT NULL,
        eg1_id INTEGER NOT NULL REFERENCES egg_groups(id),
        eg2_id INTEGER REFERENCES egg_groups(id),
        gr_id INTEGER NOT NULL REFERENCES gender_ratios(id),
        ability1_id INTEGER NOT NULL REFERENCES abilities(id),
        ability2_id INTEGER REFERENCES abilities(id),
        ha_id INTEGER REFERENCES abilities(id),
        egg_cycles INTEGER NOT NULL
      )|]
      conn
    setupTable
      "csv/evolutions.csv"
      "evolutions"
      [sql|
      CREATE TABLE ? (
        prevo_id INTEGER NOT NULL REFERENCES pokemon(id),
        evo_id INTEGER NOT NULL REFERENCES pokemon(id),
        method TEXT NOT NULL,
        PRIMARY KEY (prevo_id, evo_id)
      )|]
      conn
    setupTable
      "csv/natures.csv"
      "natures"
      [sql|
      CREATE TABLE ? (
        pokemon_id INTEGER NOT NULL REFERENCES pokemon(id),
        penny TEXT,
        jemma_swsh TEXT,
        jemma_bdsp TEXT,
        jemma_g7 TEXT
      )|]
      conn
    setupTable
      "csv/legalities.csv"
      "legality"
      [sql|
      CREATE TABLE ? (
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
      conn
    setupTable
      "csv/learnsets.csv"
      "learnsets"
      [sql|
      CREATE TABLE ? (
        pokemon_id INTEGER NOT NULL REFERENCES pokemon(id),
        move_id INTEGER NOT NULL REFERENCES moves(id),
        game_id INTEGER NOT NULL REFERENCES games(id),
        learn_method_id INTEGER NOT NULL REFERENCES learn_methods(id),
        level INTEGER
      )|]
      conn
