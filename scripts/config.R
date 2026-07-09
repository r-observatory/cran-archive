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
