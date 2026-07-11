# cran-archive

This pipeline records CRAN packages that have been removed from the live CRAN
listing (archived), together with accurate dates and reasons. The live CRAN
package index does not retain removed packages, so this fills that gap using
CRAN's own archive.

It reads CRAN's archive index (`src/contrib/Meta/archive.rds`), the current list
of available packages, and CRAN's `PACKAGES.in` annotations. A package is treated
as archived when it appears in the archive but is no longer available on CRAN.
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

A `manifest.json` accompanies the database and carries a `changed` flag so
downstream consumers can skip unchanged rebuilds.

## Running

```sh
Rscript scripts/update.R out/            # incremental (change-gated)
Rscript scripts/update.R out/ --bootstrap  # full rebuild
Rscript tests/testthat.R                 # unit tests
```
