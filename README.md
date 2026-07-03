# cran-archive

This pipeline records CRAN packages that have been removed from the live CRAN
listing (archived), together with accurate dates. The live CRAN package index
does not retain removed packages, so this fills that gap using CRAN's own
archive.

It reads CRAN's archive index (`src/contrib/Meta/archive.rds`) and the current
list of available packages. A package is treated as archived when it appears in
the archive but is no longer available on CRAN. For each archived package it
records when it first appeared, when it was archived (the date of its last
archived version), and that last version. The aggregated data is written to a
SQLite database and published to the `r-observatory/cran-archive` GitHub
repository for downstream consumers.

## Output

`cran-archive.db` (published on the rolling `current` release) contains:

- `cran_archive` - one row per currently archived package: `package`,
  `first_release`, `archived_on`, `last_version`, `removal_reason`, `source`,
  `updated_at`.
- `cran_archive_events` - the per-version release timeline plus an archival
  marker for each package: `package`, `event_date`, `event_type`, `version`.

A `manifest.json` accompanies the database and carries a `changed` flag so
downstream consumers can skip unchanged rebuilds.

## Running

```sh
Rscript scripts/update.R out/            # incremental (change-gated)
Rscript scripts/update.R out/ --bootstrap  # full rebuild
Rscript tests/testthat.R                 # unit tests
```

Removal reasons are not published by CRAN in a structured form; `removal_reason`
is left empty until a curated source is available, rather than guessed.
