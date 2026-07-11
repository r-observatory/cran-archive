# scripts/config.R: constants for the cran-archive pipeline.
CRAN_ARCHIVE_URL      <- "https://cran.r-project.org/src/contrib/Meta/archive.rds"
CRAN_PACKAGES_URL     <- "https://cloud.r-project.org"
CRAN_PACKAGES_IN_URL  <- "https://cran.r-project.org/src/contrib/PACKAGES.in"
PUBLISH_REPO          <- "r-observatory/cran-archive"
DB_FILENAME           <- "cran-archive.db"

# Floors for the names size gate: a fetch below these is treated as partial and
# the run reuses the prior published database rather than shrinking it.
CRAN_LIVE_FLOOR    <- 15000L
CRAN_ARCHIVE_FLOOR <- 20000L

# Fetch-sanity floors: below these a fetch is presumed truncated and the run
# aborts (no write) rather than publishing a shrunken catalog. The pipeline is a
# stateless rebuild, so the next healthy run self-heals.
CURRENT_PKGS_FLOOR  <- 15000L
ARCHIVE_LIST_FLOOR  <- 5000L
