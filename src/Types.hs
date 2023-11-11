module Types
  ( PokemonPartial (..),
    GenderRatio (..),
    Pokemon (..),
    MoveCategory (..),
    Move (..),
    LearnMethod (..),
    Game (..),
    MoveLearn (..),
  )
where

import Data.Text (Text)
import qualified Data.Text as T

data PokemonPartial = PokemonPartial
  { pname :: Text,
    pform :: Maybe Text,
    pformNum :: Int, -- 0 for base form, 1 for first alternate form, etc.
    pndex :: Int,
    ptype1 :: Text,
    ptype2 :: Maybe Text,
    pmonUrl :: Text
  }
  deriving (Show)

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

data Pokemon = Pokemon
  { -- From Pokedex page (PokemonPartial)
    name :: Text,
    form :: Maybe Text,
    ndex :: Int,
    type1 :: Text,
    type2 :: Maybe Text,
    -- From individual Pokemon page
    eggGroup1 :: Text,
    eggGroup2 :: Maybe Text,
    genderRatio :: GenderRatio,
    hiddenAbility :: Maybe Text
  }
  deriving (Show)

data MoveCategory = Physical | Special | Status deriving (Eq, Ord, Show, Bounded, Enum)

data Move = Move
  { mName :: Text,
    mType :: Text,
    mCategory :: MoveCategory,
    mFlavorText :: Text
  }
  deriving (Show)

data LearnMethod
  = LevelUp Int
  | Evolution
  | Tutor
  | Egg
  | TM
  | Reminder
  deriving (Eq, Ord, Show)

data Game = USUM | SWSH | BDSP | SV deriving (Eq, Ord, Show, Bounded, Enum)

data MoveLearn = MoveLearn
  { mldMoveName :: Text,
    mldLearnMethod :: LearnMethod
  }
  deriving (Eq, Ord, Show)
