# tests/testthat/test-helpers.R: unit tests for parse_archive_version,
# build_archive, and build_archive_events (no network required).

# ---------------------------------------------------------------------------
# Shared fixture builders
# ---------------------------------------------------------------------------

# Build a tarball path in the canonical CRAN archive format.
.tp <- function(pkg, ver) paste0(pkg, "/", pkg, "_", ver, ".tar.gz")

# Build a data.frame that mirrors an archive.rds element:
#   row names = tarball paths, mtime column = POSIXct dates.
.make_archive_df <- function(pkg, versions, dates) {
  paths        <- vapply(versions, function(v) .tp(pkg, v), character(1L))
  mt           <- as.POSIXct(dates, tz = "UTC")
  df           <- data.frame(mtime = mt, stringsAsFactors = FALSE)
  rownames(df) <- paths
  df
}

# Three-package fixture: PkgArchived (3 versions), PkgCurrent (in live CRAN),
# PkgSingle (one version with a dashed version string).
.make_fixture <- function() {
  list(
    PkgArchived = .make_archive_df(
      "PkgArchived",
      c("1.0", "1.1", "2.0"),
      c("2015-03-10", "2017-06-15", "2020-11-01")
    ),
    PkgCurrent = .make_archive_df(
      "PkgCurrent",
      c("0.1", "0.2"),
      c("2018-01-01", "2021-05-01")
    ),
    PkgSingle = .make_archive_df(
      "PkgSingle",
      c("1.0-1"),
      c("2010-07-04")
    )
  )
}

FIXTURE_CURRENT <- c("PkgCurrent")

# ---------------------------------------------------------------------------
# parse_archive_version
# ---------------------------------------------------------------------------

test_that("parse_archive_version: simple two-part version x.y", {
  expect_equal(parse_archive_version("ACA/ACA_1.1.tar.gz", "ACA"), "1.1")
})

test_that("parse_archive_version: multi-dot version x.y.z", {
  expect_equal(parse_archive_version("foo/foo_1.2.3.tar.gz", "foo"), "1.2.3")
})

test_that("parse_archive_version: dashed version x.y-z", {
  expect_equal(parse_archive_version("bar/bar_1.0-1.tar.gz", "bar"), "1.0-1")
})

test_that("parse_archive_version: version with multiple dashes", {
  expect_equal(parse_archive_version("pkg/pkg_2.1-0.tar.gz", "pkg"), "2.1-0")
})

test_that("parse_archive_version: returns NA_character_ for non-.tar.gz path", {
  expect_equal(parse_archive_version("baz/baz_1.0.zip", "baz"), NA_character_)
})

test_that("parse_archive_version: returns NA_character_ when package name mismatches", {
  expect_equal(parse_archive_version("ACA/other_1.0.tar.gz", "ACA"), NA_character_)
})

test_that("parse_archive_version: returns NA_character_ for empty string", {
  expect_equal(parse_archive_version("", "pkg"), NA_character_)
})

# ---------------------------------------------------------------------------
# build_archive
# ---------------------------------------------------------------------------

test_that("build_archive: excludes packages present in current_pkgs", {
  out <- build_archive(.make_fixture(), FIXTURE_CURRENT)
  expect_false("PkgCurrent" %in% out$package)
})

test_that("build_archive: includes packages absent from current_pkgs", {
  out <- build_archive(.make_fixture(), FIXTURE_CURRENT)
  expect_true("PkgArchived" %in% out$package)
  expect_true("PkgSingle"   %in% out$package)
})

test_that("build_archive: first_release is the min-mtime date (YYYY-MM-DD)", {
  out <- build_archive(.make_fixture(), FIXTURE_CURRENT)
  row <- out[out$package == "PkgArchived", ]
  expect_equal(row$first_release, "2015-03-10")
})

test_that("build_archive: archived_on is the max-mtime date (YYYY-MM-DD)", {
  out <- build_archive(.make_fixture(), FIXTURE_CURRENT)
  row <- out[out$package == "PkgArchived", ]
  expect_equal(row$archived_on, "2020-11-01")
})

test_that("build_archive: last_version is parsed from the max-mtime tarball filename", {
  out <- build_archive(.make_fixture(), FIXTURE_CURRENT)
  row <- out[out$package == "PkgArchived", ]
  expect_equal(row$last_version, "2.0")
})

test_that("build_archive: single-version package with dashed version string", {
  out <- build_archive(.make_fixture(), FIXTURE_CURRENT)
  row <- out[out$package == "PkgSingle", ]
  expect_equal(row$last_version,  "1.0-1")
  expect_equal(row$first_release, "2010-07-04")
  expect_equal(row$archived_on,   "2010-07-04")
})

test_that("build_archive: rows are sorted deterministically by package name", {
  out <- build_archive(.make_fixture(), FIXTURE_CURRENT)
  expect_equal(out$package, sort(out$package))
})

test_that("build_archive: removal_reason populated when package is in reasons map", {
  reasons <- c(PkgArchived = "Policy violation")
  out     <- build_archive(.make_fixture(), FIXTURE_CURRENT, reasons)
  expect_equal(out$removal_reason[out$package == "PkgArchived"], "Policy violation")
})

test_that("build_archive: removal_reason is NA when package absent from reasons map", {
  out <- build_archive(.make_fixture(), FIXTURE_CURRENT)
  expect_true(is.na(out$removal_reason[out$package == "PkgArchived"]))
  expect_true(is.na(out$removal_reason[out$package == "PkgSingle"]))
})

test_that("build_archive: returns zero-row frame with correct columns when all packages are current", {
  out <- build_archive(.make_fixture(), names(.make_fixture()))
  expect_equal(nrow(out), 0L)
  expect_true(all(c("package", "first_release", "archived_on",
                    "last_version", "removal_reason") %in% names(out)))
})

test_that("build_archive: empty archive_list returns zero-row frame", {
  out <- build_archive(list(), character(0))
  expect_equal(nrow(out), 0L)
})

test_that("build_archive: last_version and archived_on derive from max-mtime row, not last row", {
  # Max-mtime version ("2.0") is first, not last, guarding against tail/nrow regression.
  archive_list <- list(
    PkgOrder = .make_archive_df(
      "PkgOrder",
      c("2.0", "1.0", "1.1"),
      c("2020-11-01", "2015-03-10", "2017-06-15")
    )
  )
  out <- build_archive(archive_list, character(0))
  row <- out[out$package == "PkgOrder", ]
  expect_equal(row$last_version,  "2.0")
  expect_equal(row$archived_on,   "2020-11-01")
  expect_equal(row$first_release, "2015-03-10")
})

test_that("build_archive: 0-row entry is skipped; other packages still built", {
  empty_df           <- data.frame(mtime = as.POSIXct(character(0), tz = "UTC"),
                                   stringsAsFactors = FALSE)
  rownames(empty_df) <- character(0)
  archive_list <- list(
    EmptyPkg = empty_df,
    PkgSingle = .make_archive_df("PkgSingle", c("1.0"), c("2019-01-01"))
  )
  out <- build_archive(archive_list, character(0))
  expect_false("EmptyPkg" %in% out$package)
  expect_true("PkgSingle" %in% out$package)
  expect_equal(nrow(out), 1L)
})

# ---------------------------------------------------------------------------
# build_archive_events
# ---------------------------------------------------------------------------

test_that("build_archive_events: emits one release event per tarball version", {
  out      <- build_archive_events(.make_fixture(), FIXTURE_CURRENT)
  rel_rows <- out[out$package == "PkgArchived" & out$event_type == "release", ]
  expect_equal(nrow(rel_rows), 3L)
})

test_that("build_archive_events: emits exactly one archived event per package", {
  out      <- build_archive_events(.make_fixture(), FIXTURE_CURRENT)
  arc_rows <- out[out$package == "PkgArchived" & out$event_type == "archived", ]
  expect_equal(nrow(arc_rows), 1L)
})

test_that("build_archive_events: archived event date equals the max-mtime date", {
  out      <- build_archive_events(.make_fixture(), FIXTURE_CURRENT)
  arc_row  <- out[out$package == "PkgArchived" & out$event_type == "archived", ]
  expect_equal(arc_row$event_date, "2020-11-01")
})

test_that("build_archive_events: release event dates match mtime column values", {
  out      <- build_archive_events(.make_fixture(), FIXTURE_CURRENT)
  rel_rows <- out[out$package == "PkgArchived" & out$event_type == "release", ]
  expect_true("2015-03-10" %in% rel_rows$event_date)
  expect_true("2017-06-15" %in% rel_rows$event_date)
  expect_true("2020-11-01" %in% rel_rows$event_date)
})

test_that("build_archive_events: release event versions parsed from tarball filenames", {
  out      <- build_archive_events(.make_fixture(), FIXTURE_CURRENT)
  rel_rows <- out[out$package == "PkgArchived" & out$event_type == "release", ]
  expect_true("1.0" %in% rel_rows$version)
  expect_true("1.1" %in% rel_rows$version)
  expect_true("2.0" %in% rel_rows$version)
})

test_that("build_archive_events: excludes packages that are in current_pkgs", {
  out <- build_archive_events(.make_fixture(), FIXTURE_CURRENT)
  expect_false("PkgCurrent" %in% out$package)
})

test_that("build_archive_events: single-version package gets one release + one archived event", {
  out      <- build_archive_events(.make_fixture(), FIXTURE_CURRENT)
  pkg_rows <- out[out$package == "PkgSingle", ]
  expect_equal(sum(pkg_rows$event_type == "release"),  1L)
  expect_equal(sum(pkg_rows$event_type == "archived"), 1L)
  expect_equal(pkg_rows$event_date[pkg_rows$event_type == "archived"], "2010-07-04")
})

test_that("build_archive_events: returns zero-row frame with correct columns when all packages are current", {
  out <- build_archive_events(.make_fixture(), names(.make_fixture()))
  expect_equal(nrow(out), 0L)
  expect_true(all(c("package", "event_date", "event_type", "version") %in% names(out)))
})

test_that("build_archive_events: 0-row entry is skipped; other packages still emit events", {
  empty_df           <- data.frame(mtime = as.POSIXct(character(0), tz = "UTC"),
                                   stringsAsFactors = FALSE)
  rownames(empty_df) <- character(0)
  archive_list <- list(
    EmptyPkg  = empty_df,
    PkgSingle = .make_archive_df("PkgSingle", c("1.0"), c("2019-01-01"))
  )
  out <- build_archive_events(archive_list, character(0))
  expect_false("EmptyPkg" %in% out$package)
  expect_true("PkgSingle" %in% out$package)
})
