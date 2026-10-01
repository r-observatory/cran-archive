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
