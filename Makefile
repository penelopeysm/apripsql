.PHONY: all clean

# TODO
all: csv/egg_groups.csv csv/types.csv

csv/egg_groups.csv: src/EggGroup.hs
	cabal run apripsql -- --command egg-groups

csv/types.csv: src/Type.hs
	cabal run apripsql -- --command types

clean:
	rm -f csv/*.csv
