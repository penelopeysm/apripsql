# apripsql

This repository hosts the code required to regenerate the PostgreSQL database used by [ApriBot](https://github.com/penelopeysm/apribot).
To be precise, the code generates CSV files in the `csv/` directory.
The code to add this into the database is not yet written.

Data are scraped from [PokémonDB](https://pokemondb.net).
Errors in the database are to be recorded as issues in this repository.
**To close an issue, a test must be written to check that the error is fixed.**
Note that errors *must* be fixed by the CSV stage: the PostgreSQL database should *not* be directly edited.

Data dependencies are handled by the repository Makefile.

## Updating the Fly.io database

```bash
fly proxy 5432 -a apripsql
# source the secrets file with FLY_PG_PROXY_CONN_STRING
make
```
