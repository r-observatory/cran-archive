# scripts/config.R: constants for the cran-archive pipeline.
CRAN_ARCHIVE_URL      <- "https://cran.r-project.org/src/contrib/Meta/archive.rds"
CRAN_CURRENT_URL      <- "https://cran.r-project.org/src/contrib/Meta/current.rds"
# The server that regenerates the Meta indexes, so PACKAGES comes from the same
# batch rather than from a mirror that may lag it.
CRAN_PACKAGES_URL     <- "https://cran.r-project.org"
CRAN_PACKAGES_IN_URL  <- "https://cran.r-project.org/src/contrib/PACKAGES.in"
PUBLISH_REPO          <- "r-observatory/cran-archive"
DB_FILENAME           <- "cran-archive.db"

# Floors for the names size gate: a fetch below these is treated as partial and
# the run reuses the prior published database rather than shrinking it.
CRAN_LIVE_FLOOR    <- 15000L
CRAN_ARCHIVE_FLOOR <- 20000L

# Fetch-sanity floors: below these a fetch is presumed truncated and the run
# aborts (no write) rather than publishing a shrunken catalog. The archive
# tables are a stateless rebuild, so the next healthy run self-heals.
CURRENT_PKGS_FLOOR  <- 15000L
ARCHIVE_LIST_FLOOR  <- 5000L

# More new files than this for versions cran_tarballs already holds, in one
# run, reads as a bulk touch or a format change rather than re-uploads, and the
# prior table is kept as it was.
TARBALL_REVISION_STORM_MAX <- 500L

# Hashing each tarball once. The files come from the CDN mirror, which serves
# the same files, so the downloads stay off CRAN's own server.
TARBALL_MIRROR_URL      <- "https://cloud.r-project.org"
TARBALL_HASH_USER_AGENT <- "r-observatory/cran-archive (+https://github.com/r-observatory/cran-archive)"
# Per run: at most this many files, for at most TARBALL_HASH_MAX_SECS, and no
# new download later than TARBALL_HASH_JOB_SECS after the job started, so the
# publish step always runs inside the 30-minute job timeout.
TARBALL_HASH_MAX_FILES   <- 12000L
TARBALL_HASH_MAX_SECS    <- 900
TARBALL_HASH_JOB_SECS    <- 1200
# Downloads at once, tries per file, the first wait between tries (it doubles),
# and the seconds one try may take.
TARBALL_HASH_CONCURRENCY <- 4L
TARBALL_HASH_ATTEMPTS    <- 3L
TARBALL_HASH_BACKOFF     <- 2
TARBALL_HASH_TIMEOUT     <- 180
