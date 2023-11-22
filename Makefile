.PHONY: all clean

all: csv/learnsets.csv csv/natures.csv csv/legalities.csv csv/evolutions.csv

csv/egg-groups.csv: src/EggGroup.hs
	cabal run apripsql -- --command egg-groups

csv/types.csv: src/Type.hs
	cabal run apripsql -- --command types

csv/move-categories.csv: src/MoveCategory.hs
	cabal run apripsql -- --command move-categories

csv/gender-ratios.csv: src/GenderRatio.hs
	cabal run apripsql -- --command gender-ratios

csv/games.csv: src/Game.hs
	cabal run apripsql -- --command games

csv/learn-methods.csv: src/LearnMethod.hs
	cabal run apripsql -- --command learn-methods

csv/abilities.csv: src/Ability.hs
	cabal run apripsql -- --command abilities

csv/pokemon-raw.csv: src/RawPokemon.hs
	cabal run apripsql -- --command pokemon-raw

csv/pokemon.csv: src/Pokemon.hs csv/pokemon-raw.csv csv/egg-groups.csv csv/types.csv csv/gender-ratios.csv csv/abilities.csv
	cabal run apripsql -- --command pokemon

csv/moves-raw.csv: src/RawMove.hs
	cabal run apripsql -- --command moves-raw

csv/moves.csv: src/Move.hs csv/moves-raw.csv csv/types.csv csv/move-categories.csv
	cabal run apripsql -- --command moves

csv/learnsets-raw.csv: src/RawLearnset.hs
	cabal run apripsql -- --command learnsets-raw

csv/learnsets-suppl.csv: src/SupplementaryLearnset.hs csv/pokemon.csv
	cabal run apripsql -- --command learnsets-suppl

csv/learnsets.csv: src/Learnset.hs csv/learnsets-raw.csv csv/moves.csv csv/learn-methods.csv csv/pokemon.csv csv/games.csv
	cabal run apripsql -- --command learnsets

csv/natures.csv: src/Nature.hs csv/pokemon.csv static/natures-raw.csv
	cabal run apripsql -- --command natures

csv/legalities.csv: src/Legality.hs csv/pokemon.csv static/legalities-raw.csv
	cabal run apripsql -- --command legalities

csv/evolutions-raw.csv: src/RawEvolution.hs
	cabal run apripsql -- --command evolutions-raw

csv/evolutions.csv: src/Evolution.hs csv/evolutions-raw.csv csv/pokemon.csv
	cabal run apripsql -- --command evolutions

clean:
	rm -f csv/*.csv
