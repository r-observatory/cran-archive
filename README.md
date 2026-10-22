# cran-archive

This pipeline records CRAN packages that have been removed from the live CRAN
listing (archived), together with accurate dates and reasons. The live CRAN
package index does not retain removed packages, so this fills that gap using
CRAN's own archive.

It reads CRAN's archive index (`src/contrib/Meta/archive.rds`), its index of current tarballs (`src/contrib/Meta/current.rds`), the list of available packages (`src/contrib/PACKAGES.rds`), all three from cran.r-project.org, and CRAN's `PACKAGES.in` annotations. A package is treated as archived when it appears in the archive but is no longer available on CRAN. The available list is read with only the duplicates filter, so a package that declares `OS_type: windows`, or that needs a newer R than the runner, still counts as available.
For each archived package it records when it first appeared, when and why it was
archived, and its last version. The aggregated data is written to a SQLite
database and published to the `r-observatory/cran-archive` GitHub repository for
downstream consumers.

## Archived packages

The archived tarballs only carry their original release dates, so the mtime of a
package's last tarball is its last *release* date — which can predate the actual
archival by years (a stable package archived only when its maintainer's email
became undeliverable, say). The archival **date** and **reason** therefore come
from CRAN's `X-CRAN-Comment` annotation, e.g.

```
X-CRAN-Comment: Archived on 2026-07-10 as email to the maintainer is undeliverable.
```

`archived_on` is the date parsed from that comment (falling back to the last
tarball mtime only when the comment carries no date), and `removal_reason` is the
cause with the leading `Archived on <date> as` prefix stripped.

A package can be archived, restored, and archived again. CRAN records these
cycles in `X-CRAN-History`, which persists even after a package returns to CRAN,
so `cran_archive_history` retains each archival and re-listing (with its date and
reason) as durable history rather than only the current state.

Beyond the archive/relist cycle, CRAN's `X-CRAN-History` and `X-CRAN-Comment`
annotations describe a wider set of events: a package may be orphaned, have
individual versions removed, be renamed, or be replaced by a successor. These
are parsed into `cran_archive_lineage`, an ordered per-package timeline where
each row is one event with its date (when stated), a canonical action, and the
cleaned reason. The recognised actions are `archived`, `unarchived` (covering
unarchival, unorphaning, restoration and reinstatement), `orphaned`, `removed`,
`renamed`, and `replaced`. A `Replaced_by` annotation is folded in as a trailing
`replaced` event. `cran_archive_action_counts` is a small companion histogram of
how many events of each action occur across all packages.

## Source tarballs

`cran_tarballs` records the exact size in bytes and the modification time of every source tarball CRAN lists, from `Meta/current.rds` and `Meta/archive.rds`, with the MD5 sum from `PACKAGES.rds` while the file is current. A file is identified by its size and mtime within its package version. When CRAN replaces the file of a version already released, the new file becomes the next `revision` and the old row stays, marked `gone` once CRAN no longer lists it. A file that moves from the current listing to the archive keeps its revision and its MD5. The MD5 is filled in, or corrected, whenever `PACKAGES.rds` gives it, until the file's own bytes are hashed, so the seconds by which that index trails `current.rds` never create a revision. Copies CRAN keeps under a package's `Ancestry/` or `Old/` folder are not recorded.

`mtime` is the upload time. `first_seen` and `last_seen` only say when this pipeline saw the file, and the first run recorded every file with that day's date.

Each file is also downloaded once to record its `sha256`, and its `md5sum` where `PACKAGES.rds` never gave one. The files come from the CDN mirror `https://cloud.r-project.org`, four at a time, with up to three tries each and a growing wait between tries, under a User-Agent that names this repository. A current file is read from `src/contrib/` and an archived one from `src/contrib/Archive/<package>/`; a `gone` file is not downloaded, since its address now serves another file or none. Before anything is stored, the download must be exactly `size_bytes` long and, where the row has CRAN's MD5, must match it. A file that does not verify, or does not download, stores nothing, is named in the run log, and is tried again on a later run. `hashed_at` is when the hashes were stored. A file with a `sha256` is never downloaded again, and a re-upload of the same version is a new revision, hashed on its own.

A run hashes at most 12,000 files and starts no download after 15 minutes of hashing, or 20 minutes after the job started, whichever comes first, so the publish step always runs inside the 30-minute job timeout. Each batch is stored as it finishes, so a run that stops on its budget publishes what it hashed. Current files go first, then archived files of packages still on CRAN, newest release first, then the rest. When hashing stops on an unexpected error, the run fails after publishing.

The table carries forward from the previous release, hashes included. If that release cannot be read, or downloads as an empty file, nothing is published. If the current index fails or lists fewer than 15,000 files, or more than 500 new files appear for versions already recorded, the previous table is kept as it was, `tarballs_state` in `manifest.json` says why, and the run fails after publishing so that someone looks. When building the day's file list stops with an error, the error text is printed in the run log and stored as `tarballs_error` in `manifest.json`.

## Output

`cran-archive.db` (published on the rolling `current` release) contains:

- `cran_archive` - one row per currently archived package: `package`,
  `first_release`, `archived_on`, `last_version`, `removal_reason`, `source`,
  `updated_at`.
- `cran_archive_events` - the per-version release timeline plus an archival
  marker for each package: `package`, `event_date`, `event_type`, `version`.
- `cran_archive_history` - the durable archive/relist history: one row per
  archival episode with `archived_on`, `removal_reason`, and `relisted_on`
  (NULL while the package is currently archived), keyed by
  `(package, episode_seq)`.
- `cran_archive_lineage` - the full ordered event lineage parsed from the CRAN
  annotations: one row per event with `package`, `seq` (position within the
  package's timeline), `event_date` (NULL when the event is undated), `action`
  (one of `archived`, `unarchived`, `orphaned`, `removed`, `renamed`,
  `replaced`), and `reason`, keyed by `(package, seq)`.
- `cran_archive_action_counts` - a histogram of the lineage: one row per
  `action` with its total count `n` across all packages.
- `cran_tarballs` - one row per source tarball file: `package`, `version`, `revision`, `size_bytes`, `mtime` (UTC), `md5sum`, `listing` (`current`, `archive` or `gone`), `first_seen`, `last_seen`, `sha256` and `hashed_at` (UTC, NULL until the file is hashed), keyed by `(package, version, revision)`.

A `manifest.json` accompanies the database and carries a `changed` flag so
downstream consumers can skip unchanged rebuilds, plus `n_tarballs`, `tarball_revisions_new`, `tarballs_state` (`cold_start`, `updated`, `carried`, `storm`, `unreachable` or `skipped`) and `tarballs_error` (null unless building the day's file list stopped with an error). For the hashes it carries `tarballs_hashed` (this run), `tarballs_to_hash` (listed files still without a `sha256`), `tarball_hash_mismatches`, `tarball_hash_failures` and `tarball_hash_stop` (`done`, `count`, `time`, `error` or `skipped`); the counts are null when hashing was skipped.

## Running

```sh
Rscript scripts/update.R out/            # incremental (change-gated)
Rscript scripts/update.R out/ --bootstrap  # full rebuild
Rscript tests/testthat.R                 # unit tests
```

## Feedback

Found a bug, a wrong number, or a missing package? Report it at [r-observatory/feedback](https://github.com/r-observatory/feedback/issues/new/choose). All feedback about R Observatory, the site, the data, and the pipelines, is tracked in one place.
