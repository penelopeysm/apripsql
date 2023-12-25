.PHONY: all clean

all: database

database: src/Setup/Database.hs csv/learnsets.csv csv/natures.csv csv/legalities.csv csv/evolutions.csv csv/aliases.csv
	cabal run apripsql -- --command database

csv/egg-groups.csv: src/Setup/EggGroup.hs
	cabal run apripsql -- --command egg-groups

csv/types.csv: src/Setup/Type.hs
	cabal run apripsql -- --command types

csv/move-categories.csv: src/Setup/MoveCategory.hs
	cabal run apripsql -- --command move-categories

csv/gender-ratios.csv: src/Setup/GenderRatio.hs
	cabal run apripsql -- --command gender-ratios

csv/games.csv: src/Setup/Game.hs
	cabal run apripsql -- --command games

csv/learn-methods.csv: src/Setup/LearnMethod.hs
	cabal run apripsql -- --command learn-methods

csv/abilities.csv: src/Setup/Ability.hs
	cabal run apripsql -- --command abilities

csv/pokemon-raw.csv: src/Setup/RawPokemon.hs
	cabal run apripsql -- --command pokemon-raw

csv/pokemon.csv: src/Setup/Pokemon.hs csv/pokemon-raw.csv csv/egg-groups.csv csv/types.csv csv/gender-ratios.csv csv/abilities.csv
	cabal run apripsql -- --command pokemon

csv/aliases.csv: src/Setup/Alias.hs csv/pokemon.csv static/aliases.csv
	cabal run apripsql -- --command aliases

csv/moves-raw.csv: src/Setup/RawMove.hs
	cabal run apripsql -- --command moves-raw

csv/moves.csv: src/Setup/Move.hs csv/moves-raw.csv csv/types.csv csv/move-categories.csv
	cabal run apripsql -- --command moves

csv/learnsets-raw.csv: src/Setup/RawLearnset.hs
	cabal run apripsql -- --command learnsets-raw

csv/learnsets-suppl.csv: src/Setup/SupplementaryLearnset.hs csv/pokemon.csv
	cabal run apripsql -- --command learnsets-suppl

static/learnsets-dlc2.csv: static/dlc2_serebii.py
	python static/dlc2_serebii.py

csv/learnsets.csv: src/Setup/Learnset.hs csv/learnsets-raw.csv csv/moves.csv csv/learn-methods.csv csv/pokemon.csv csv/games.csv csv/learnsets-suppl.csv static/learnsets-dlc2.csv
	cabal run apripsql -- --command learnsets

csv/natures.csv: src/Setup/Nature.hs csv/pokemon.csv static/natures-raw.csv
	cabal run apripsql -- --command natures

csv/legalities.csv: src/Setup/Legality.hs csv/pokemon.csv static/legalities-raw.csv
	cabal run apripsql -- --command legalities

csv/evolutions-raw.csv: src/Setup/RawEvolution.hs
	cabal run apripsql -- --command evolutions-raw

csv/evolutions.csv: src/Setup/Evolution.hs csv/evolutions-raw.csv csv/pokemon.csv
	cabal run apripsql -- --command evolutions

clean:
	rm -f csv/*.csv
