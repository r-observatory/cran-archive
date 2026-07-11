# tests/testthat/test-export.R: unit tests for export_archive and write_manifest.
library(RSQLite)
library(jsonlite)

# ---------------------------------------------------------------------------
# Fixture builders
# ---------------------------------------------------------------------------

.make_archive_df <- function() {
  data.frame(
    package        = c("PkgA", "PkgB"),
    first_release  = c("2015-01-01", "2018-06-15"),
    archived_on    = c("2020-03-10", "2022-09-01"),
    last_version   = c("2.0", "1.3.0"),
    removal_reason = c(NA_character_, "Abandoned"),
    stringsAsFactors = FALSE
  )
}

.make_events_df <- function() {
  data.frame(
    package    = c("PkgA", "PkgA", "PkgA", "PkgB", "PkgB"),
    event_date = c("2015-01-01", "2019-05-01", "2020-03-10",
                   "2018-06-15", "2022-09-01"),
    event_type = c("release", "release", "archived", "release", "archived"),
    version    = c("1.0", "2.0", "2.0", "1.3.0", "1.3.0"),
    stringsAsFactors = FALSE
  )
}

.make_empty_history_df <- function() {
  data.frame(package = character(0), episode_seq = integer(0),
    archived_on = character(0), relisted_on = character(0), removal_reason = character(0),
    last_version = character(0), relist_source = character(0), archived_on_source = character(0),
    stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# export_archive
# ---------------------------------------------------------------------------

test_that("export_archive writes cran_archive with correct row count and values", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_archive(tmp, .make_archive_df(), .make_events_df(), .make_empty_history_df())

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  rows <- RSQLite::dbGetQuery(con, "SELECT * FROM cran_archive ORDER BY package")
  expect_equal(nrow(rows), 2L)

  pkga <- rows[rows$package == "PkgA", ]
  expect_equal(pkga$first_release,  "2015-01-01")
  expect_equal(pkga$archived_on,    "2020-03-10")
  expect_equal(pkga$last_version,   "2.0")
  expect_true(is.na(pkga$removal_reason))
  expect_equal(pkga$source,         "cran-archive-rds")
  expect_true(nzchar(pkga$updated_at))
})

test_that("export_archive writes removal_reason when present", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_archive(tmp, .make_archive_df(), .make_events_df(), .make_empty_history_df())

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  pkgb <- RSQLite::dbGetQuery(con, "SELECT * FROM cran_archive WHERE package = 'PkgB'")
  expect_equal(pkgb$removal_reason, "Abandoned")
})

test_that("export_archive writes cran_archive_events with correct row count", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_archive(tmp, .make_archive_df(), .make_events_df(), .make_empty_history_df())

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  rows <- RSQLite::dbGetQuery(con,
    "SELECT * FROM cran_archive_events ORDER BY package, event_date")
  expect_equal(nrow(rows), 5L)

  pkga_rows <- rows[rows$package == "PkgA", ]
  expect_equal(sum(pkga_rows$event_type == "release"),  2L)
  expect_equal(sum(pkga_rows$event_type == "archived"), 1L)
})

test_that("export_archive creates index on cran_archive_events(package)", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_archive(tmp, .make_archive_df(), .make_events_df(), .make_empty_history_df())

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  idx <- RSQLite::dbGetQuery(
    con,
    "SELECT name FROM sqlite_master WHERE type = 'index' ORDER BY name"
  )$name
  expect_true("idx_cran_archive_events_pkg" %in% idx)
})

test_that("export_archive: cran_archive has package as TEXT PRIMARY KEY", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_archive(tmp, .make_archive_df(), .make_events_df(), .make_empty_history_df())

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  info    <- RSQLite::dbGetQuery(con, "PRAGMA table_info(cran_archive)")
  pkg_col <- info[info$name == "package", ]
  expect_equal(pkg_col$pk, 1L)
})

test_that("export_archive: cran_archive has all required columns", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_archive(tmp, .make_archive_df(), .make_events_df(), .make_empty_history_df())

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  info <- RSQLite::dbGetQuery(con, "PRAGMA table_info(cran_archive)")
  expect_true(all(c("package", "first_release", "archived_on", "last_version",
                    "removal_reason", "source", "updated_at") %in% info$name))
})

test_that("export_archive: cran_archive_events has all required columns", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_archive(tmp, .make_archive_df(), .make_events_df(), .make_empty_history_df())

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  info <- RSQLite::dbGetQuery(con, "PRAGMA table_info(cran_archive_events)")
  expect_true(all(c("package", "event_date", "event_type", "version") %in% info$name))
})

test_that("export_archive overwrites an existing DB file cleanly (no double-insert)", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  export_archive(tmp, .make_archive_df(), .make_events_df(), .make_empty_history_df())

  single_arch   <- .make_archive_df()[1L, ]
  single_events <- .make_events_df()[.make_events_df()$package == "PkgA", ]
  export_archive(tmp, single_arch, single_events, .make_empty_history_df())

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  expect_equal(
    RSQLite::dbGetQuery(con, "SELECT COUNT(*) AS n FROM cran_archive")$n,
    1L
  )
  expect_equal(
    RSQLite::dbGetQuery(con, "SELECT COUNT(*) AS n FROM cran_archive_events")$n,
    3L
  )
})

test_that("export_archive: empty archive_df writes zero rows without error", {
  tmp <- tempfile(fileext = ".db")
  on.exit(unlink(tmp), add = TRUE)

  empty_arch <- data.frame(
    package = character(0), first_release = character(0),
    archived_on = character(0), last_version = character(0),
    removal_reason = character(0), stringsAsFactors = FALSE
  )
  empty_ev <- data.frame(
    package = character(0), event_date = character(0),
    event_type = character(0), version = character(0),
    stringsAsFactors = FALSE
  )
  expect_no_error(export_archive(tmp, empty_arch, empty_ev, .make_empty_history_df()))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  expect_equal(RSQLite::dbGetQuery(con, "SELECT COUNT(*) AS n FROM cran_archive")$n, 0L)
  expect_equal(RSQLite::dbGetQuery(con, "SELECT COUNT(*) AS n FROM cran_archive_events")$n, 0L)
})

test_that("export_archive: the open-episode unique index rejects two open rows", {
  tmp <- withr::local_tempdir(); db <- file.path(tmp, "x.db")
  adf <- data.frame(package=character(0), first_release=character(0), archived_on=character(0),
    last_version=character(0), removal_reason=character(0), stringsAsFactors=FALSE)
  edf <- data.frame(package=character(0), event_date=character(0), event_type=character(0),
    version=character(0), stringsAsFactors=FALSE)
  bad <- data.frame(package=c("P","P"), episode_seq=c(1L,2L),
    archived_on=c("2020-01-01","2021-01-01"), relisted_on=c(NA_character_, NA_character_),
    removal_reason=c(NA,NA), last_version=c(NA,NA),
    relist_source=c(NA,NA), archived_on_source=c("archive-rds","archive-rds"),
    stringsAsFactors=FALSE)
  expect_error(export_archive(db, adf, edf, bad))
})

# ---------------------------------------------------------------------------
# write_manifest
# ---------------------------------------------------------------------------

test_that("write_manifest writes valid JSON readable by jsonlite::read_json", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  obj <- list(pipeline = "cran-archive", n_archived = 12345L, changed = TRUE)
  write_manifest(tmp, obj)

  result <- jsonlite::read_json(tmp)
  expect_equal(result$pipeline,   "cran-archive")
  expect_equal(result$n_archived, 12345L)
  expect_true(result$changed)
})

test_that("write_manifest round-trips nested lists correctly", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)

  obj <- list(source = list(archive_fingerprint = "abc123"), changed = FALSE)
  write_manifest(tmp, obj)

  result <- jsonlite::read_json(tmp)
  expect_equal(result$source$archive_fingerprint, "abc123")
  expect_false(result$changed)
})
