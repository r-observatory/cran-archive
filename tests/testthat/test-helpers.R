# tests/testthat/test-helpers.R: unit tests for parse_archive_version,
# build_archive, build_archive_events, and parse_packages_in (no network required).

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

# ---------------------------------------------------------------------------
# parse_packages_in
# ---------------------------------------------------------------------------

# Fixture covering the four required cases:
#   (1) epoxy     -- single-line X-CRAN-Comment, CRLF line endings
#   (2) multiline -- X-CRAN-Comment that continues on a second (indented) line
#   (3) histonly  -- has X-CRAN-History but NO X-CRAN-Comment  (must be excluded)
#   (4) nocomment -- only Package + Version fields              (must be excluded)
.PACKAGES_IN_FIXTURE <- paste0(
  "Package: epoxy\r\n",
  "Version: 0.6.0\r\n",
  "X-CRAN-Comment: Archived on 2026-07-02 as issues were not corrected in time.\r\n",
  "\r\n",
  "Package: multiline\n",
  "X-CRAN-Comment: Archived on 2026-07-02 at the maintainer's request.\n",
  " Additional context here.\n",
  "\n",
  "Package: histonly\n",
  "X-CRAN-History: Archived on 2024-01-01 for policy reasons.\n",
  "\n",
  "Package: nocomment\n",
  "Version: 1.0\n"
)

test_that("parse_packages_in: returns exactly the packages that have an X-CRAN-Comment", {
  out <- parse_packages_in(.PACKAGES_IN_FIXTURE)
  expect_equal(sort(names(out)), c("epoxy", "multiline"))
})

test_that("parse_packages_in: single-line X-CRAN-Comment value is stored verbatim (CRLF input)", {
  out <- parse_packages_in(.PACKAGES_IN_FIXTURE)
  expect_equal(
    out[["epoxy"]],
    "Archived on 2026-07-02 as issues were not corrected in time."
  )
})

test_that("parse_packages_in: multi-line X-CRAN-Comment continuation lines are folded with a space", {
  out <- parse_packages_in(.PACKAGES_IN_FIXTURE)
  expect_equal(
    out[["multiline"]],
    "Archived on 2026-07-02 at the maintainer's request. Additional context here."
  )
})

test_that("parse_packages_in: record with only X-CRAN-History (no comment) is excluded", {
  out <- parse_packages_in(.PACKAGES_IN_FIXTURE)
  expect_false("histonly" %in% names(out))
})

test_that("parse_packages_in: a '.' continuation line (Debian blank-line marker) is not folded literally", {
  txt <- paste(
    "Package: dotpara",
    "X-CRAN-Comment: Archived on 2026-06-30 for policy violation.",
    "  .",
    "  On Internet access.",
    "", sep = "\n"
  )
  out <- parse_packages_in(txt)
  expect_equal(out[["dotpara"]], "Archived on 2026-06-30 for policy violation. On Internet access.")
  expect_false(grepl(" [.] ", out[["dotpara"]]))
})

test_that("parse_packages_in: record with no X-CRAN-Comment field is excluded", {
  out <- parse_packages_in(.PACKAGES_IN_FIXTURE)
  expect_false("nocomment" %in% names(out))
})

test_that("parse_packages_in: returns character(0) for empty input", {
  out <- parse_packages_in("")
  expect_equal(out, character(0))
})

test_that("parse_packages_in returns a named character vector (not a list)", {
  out <- parse_packages_in(.PACKAGES_IN_FIXTURE)
  expect_true(is.character(out))
  expect_true(!is.null(names(out)))
})

# ---------------------------------------------------------------------------
# build_archive integration: reasons from parse_packages_in
# ---------------------------------------------------------------------------

test_that("build_archive integration: removal_reason populated (and cleaned) from parse_packages_in result", {
  # Use the fixture reasons map; build an archive_list that includes epoxy.
  reasons <- parse_packages_in(.PACKAGES_IN_FIXTURE)
  archive_list <- list(
    epoxy = .make_archive_df("epoxy", c("0.5.0", "0.6.0"),
                             c("2024-01-01", "2026-07-02"))
  )
  out <- build_archive(archive_list, character(0), reasons)
  # The stored reason is the cause only; the "Archived on <date> as" prefix is
  # stripped (the date is surfaced separately via archived_on).
  expect_equal(
    out$removal_reason[out$package == "epoxy"],
    "issues were not corrected in time"
  )
  expect_equal(out$archived_on[out$package == "epoxy"], "2026-07-02")
})

test_that("build_archive integration: removal_reason is NA for package absent from parse_packages_in result", {
  reasons <- parse_packages_in(.PACKAGES_IN_FIXTURE)
  archive_list <- list(
    orphan = .make_archive_df("orphan", c("1.0"), c("2023-05-01"))
  )
  out <- build_archive(archive_list, character(0), reasons)
  expect_true(is.na(out$removal_reason[out$package == "orphan"]))
})

# ---------------------------------------------------------------------------
# parse_history_episodes / parse_packages_history
# ---------------------------------------------------------------------------

test_that("parse_history_episodes: single closed cycle with reason", {
  eps <- parse_history_episodes(
    "Archived on 2016-01-30 as check problems were not corrected. Unarchived on 2016-03-01.")
  expect_equal(length(eps), 1L)
  expect_equal(eps[[1]]$archived_on, "2016-01-30")
  expect_equal(eps[[1]]$relisted_on, "2016-03-01")
  expect_equal(eps[[1]]$removal_reason, "check problems were not corrected")
})

test_that("parse_history_episodes: multiple cycles preserved in order", {
  eps <- parse_history_episodes(paste(
    "Archived on 2023-08-14 as issues were not corrected despite repeated reminders.",
    "Unarchived on 2023-08-21.",
    "Archived on 2025-12-11 as issues were not corrected in multiple re-submissions.",
    "Unarchived on 2025-12-12."))
  expect_equal(length(eps), 2L)
  expect_equal(eps[[1]]$archived_on, "2023-08-14")
  expect_equal(eps[[1]]$relisted_on, "2023-08-21")
  expect_equal(eps[[2]]$archived_on, "2025-12-11")
  expect_equal(eps[[2]]$relisted_on, "2025-12-12")
})

test_that("parse_history_episodes: NA input returns an empty list", {
  expect_equal(length(parse_history_episodes(NA_character_)), 0L)
})

test_that("parse_history_episodes: undated / alternate-verb events are skipped", {
  expect_equal(length(parse_history_episodes(
    "Versions 1.0.6 to 1.1.2 were removed for licence violations.")), 0L)
  expect_equal(length(parse_history_episodes(
    "Orphaned on 2023-02-05 after the maintainer became inactive.")), 0L)
})

test_that("parse_packages_history: maps package -> episodes, folds continuation lines", {
  text <- paste(
    "Package: Foo",
    "X-CRAN-Comment: Archived on 2026-01-01 as broken.",
    "X-CRAN-History: Archived on 2020-01-01 as check errors",
    "  were not corrected. Unarchived on 2020-02-01.",
    "",
    "Package: Bar",
    "Maintainer: Someone",
    sep = "\n")
  m <- parse_packages_history(text)
  expect_true("Foo" %in% names(m))
  expect_false("Bar" %in% names(m))
  expect_equal(m[["Foo"]][[1]]$archived_on, "2020-01-01")
  expect_equal(m[["Foo"]][[1]]$relisted_on, "2020-02-01")
  expect_equal(m[["Foo"]][[1]]$removal_reason, "check errors were not corrected")
})

# ---------------------------------------------------------------------------
# build_archive_history
# ---------------------------------------------------------------------------

test_that("build_archive_history: open episode from archive_df has NA relisted_on", {
  adf <- data.frame(package="PkgOpen", first_release="2015-01-01",
    archived_on="2026-07-01", last_version="1.2", removal_reason="broken",
    stringsAsFactors=FALSE)
  h <- build_archive_history(adf, list())
  expect_equal(nrow(h), 1L)
  expect_equal(h$episode_seq, 1L)
  expect_true(is.na(h$relisted_on))
  expect_equal(h$archived_on_source, "archive-rds")
})

test_that("build_archive_history: closed episodes from history, ordered, seq assigned", {
  adf <- data.frame(package=character(0), first_release=character(0),
    archived_on=character(0), last_version=character(0), removal_reason=character(0),
    stringsAsFactors=FALSE)
  hm <- list(Cyc = list(
    list(archived_on="2025-12-11", removal_reason="second", relisted_on="2025-12-12"),
    list(archived_on="2023-08-14", removal_reason="first",  relisted_on="2023-08-21")))
  h <- build_archive_history(adf, hm)
  expect_equal(nrow(h), 2L)
  expect_equal(h$archived_on, c("2023-08-14", "2025-12-11"))  # sorted ascending
  expect_equal(h$episode_seq, c(1L, 2L))
  expect_equal(h$relist_source, c("x-cran-history", "x-cran-history"))
})

test_that("build_archive_history: currently-archived package keeps closed history + open", {
  adf <- data.frame(package="Both", first_release="2015-01-01",
    archived_on="2026-07-01", last_version="3.0", removal_reason="now",
    stringsAsFactors=FALSE)
  hm <- list(Both = list(
    list(archived_on="2020-01-01", removal_reason="then", relisted_on="2020-02-01")))
  h <- build_archive_history(adf, hm)
  expect_equal(nrow(h), 2L)
  expect_equal(h$relisted_on, c("2020-02-01", NA_character_))  # closed then open
  expect_equal(sum(is.na(h$relisted_on)), 1L)                  # exactly one open
})

test_that("build_archive_history: drops unclosed/negative history episodes", {
  adf <- data.frame(package=character(0), first_release=character(0),
    archived_on=character(0), last_version=character(0), removal_reason=character(0),
    stringsAsFactors=FALSE)
  hm <- list(Bad = list(
    list(archived_on="2020-01-01", removal_reason=NA_character_, relisted_on=NA_character_),      # unclosed
    list(archived_on="2021-05-05", removal_reason="x", relisted_on="2021-01-01")))                # negative
  expect_equal(nrow(build_archive_history(adf, hm)), 0L)
})

test_that("build_archive_history: empty inputs -> zero-row frame with correct columns", {
  adf <- data.frame(package=character(0), first_release=character(0),
    archived_on=character(0), last_version=character(0), removal_reason=character(0),
    stringsAsFactors=FALSE)
  h <- build_archive_history(adf, list())
  expect_equal(nrow(h), 0L)
  expect_true(all(c("package","episode_seq","archived_on","relisted_on","removal_reason",
    "last_version","relist_source","archived_on_source") %in% names(h)))
})

test_that("build_archive_history: an archive_df row with NA archived_on yields no open episode and no error", {
  adf <- data.frame(package="P", first_release="2015-01-01", archived_on=NA_character_,
    last_version="1.0", removal_reason="x", stringsAsFactors=FALSE)
  expect_equal(nrow(build_archive_history(adf, list())), 0L)
})

test_that("build_archive_history is deterministic across repeated calls", {
  adf <- data.frame(package="P", first_release="2015-01-01", archived_on="2026-01-01",
    last_version="1.0", removal_reason="x", stringsAsFactors=FALSE)
  hm <- list(P = list(list(archived_on="2020-01-01", removal_reason="y", relisted_on="2020-02-01")))
  expect_identical(build_archive_history(adf, hm), build_archive_history(adf, hm))
})

test_that("build_archive_history: closed episode colliding with the open archived_on is dropped", {
  adf <- data.frame(package="P", first_release="2015-01-01", archived_on="2026-07-01",
    last_version="2.0", removal_reason="now", stringsAsFactors=FALSE)
  hm <- list(P = list(list(archived_on="2026-07-01", removal_reason="dup", relisted_on="2026-07-05")))
  h <- build_archive_history(adf, hm)
  expect_equal(nrow(h), 1L)          # only the open episode survives
  expect_true(is.na(h$relisted_on))  # and it is the open one
})

# ---------------------------------------------------------------------------
# archived_on comes from the X-CRAN-Comment date, not the tarball mtime
# ---------------------------------------------------------------------------

test_that("comment_archived_on extracts the first ISO date from an X-CRAN-Comment", {
  expect_equal(comment_archived_on("Archived on 2026-07-10 as email undeliverable."), "2026-07-10")
  expect_equal(comment_archived_on("Removed on 2024-11-25 for misrepresentation."), "2024-11-25")
  expect_true(is.na(comment_archived_on("Orphaned; maintainer unreachable.")))
  expect_true(is.na(comment_archived_on(NA_character_)))
  expect_true(is.na(comment_archived_on("")))
})

test_that("build_archive: archived_on is the X-CRAN-Comment date, not the tarball mtime", {
  al <- list(tmcn = .make_archive_df("tmcn", c("0.1", "0.2-13"), c("2015-01-01", "2019-08-08")))
  reasons <- c(tmcn = "Archived on 2026-07-10 as email to the maintainer is undeliverable.")
  adf <- build_archive(al, current_pkgs = character(0), reasons = reasons)
  expect_equal(adf$archived_on[adf$package == "tmcn"], "2026-07-10")
  expect_equal(adf$first_release[adf$package == "tmcn"], "2015-01-01")
  expect_equal(adf$last_version[adf$package == "tmcn"], "0.2-13")
})

test_that("build_archive: falls back to the max tarball mtime when the comment has no date", {
  al <- list(nodate = .make_archive_df("nodate", c("1.0", "1.1"), c("2016-02-02", "2018-03-03")))
  reasons <- c(nodate = "Orphaned; maintainer unreachable.")
  adf <- build_archive(al, current_pkgs = character(0), reasons = reasons)
  expect_equal(adf$archived_on[adf$package == "nodate"], "2018-03-03")
})

# ---------------------------------------------------------------------------
# clean_comment_reason strips the "<verb> on <date> [connector]" prefix
# ---------------------------------------------------------------------------

test_that("clean_comment_reason strips the leading status/date prefix + connector", {
  expect_equal(clean_comment_reason("Archived on 2026-07-10 as email to the maintainer is undeliverable."),
               "email to the maintainer is undeliverable")
  expect_equal(clean_comment_reason("Removed on 2024-11-25 for misrepresentation of authorship."),
               "misrepresentation of authorship")
  expect_equal(clean_comment_reason("Removed on 2025-09-04 at the maintainer's request."),
               "the maintainer's request")
  expect_true(is.na(clean_comment_reason(NA_character_)))
  expect_true(is.na(clean_comment_reason("")))
})

test_that("clean_comment_reason keeps a dateless comment as-is", {
  expect_equal(clean_comment_reason("Orphaned; maintainer unreachable"),
               "Orphaned; maintainer unreachable")
})

test_that("build_archive stores the cleaned reason alongside the comment date", {
  al <- list(tmcn = .make_archive_df("tmcn", c("0.1", "0.2-13"), c("2015-01-01", "2019-08-08")))
  reasons <- c(tmcn = "Archived on 2026-07-10 as email to the maintainer is undeliverable.")
  adf <- build_archive(al, current_pkgs = character(0), reasons = reasons)
  expect_equal(adf$archived_on[adf$package == "tmcn"], "2026-07-10")
  expect_equal(adf$removal_reason[adf$package == "tmcn"], "email to the maintainer is undeliverable")
})

test_that("parse_history_episodes: strips for/at connectors in the reason, not only 'as'", {
  eps <- parse_history_episodes(
    "Archived on 2021-12-31 for repeated policy violation. Unarchived on 2022-02-01.")
  expect_equal(eps[[1]]$removal_reason, "repeated policy violation")
  eps2 <- parse_history_episodes(
    "Archived on 2020-05-05 at the maintainer's request. Unarchived on 2020-06-06.")
  expect_equal(eps2[[1]]$removal_reason, "the maintainer's request")
})
