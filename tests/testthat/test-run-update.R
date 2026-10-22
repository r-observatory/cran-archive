# tests/testthat/test-run-update.R: integration tests for run_update with a
# fake io (no network required).
library(RSQLite)
library(jsonlite)

# Source update.R if it has not been loaded already. When test_dir() runs it
# changes cwd to tests/testthat; when called from the project root cwd stays.
if (!exists("run_update", mode = "function")) {
  .candidates <- c(
    file.path(getwd(), "scripts", "update.R"),
    file.path(getwd(), "..", "..", "scripts", "update.R")
  )
  .upd <- .candidates[file.exists(.candidates)]
  if (length(.upd)) source(normalizePath(.upd[1L]))
}

# ---------------------------------------------------------------------------
# Fixture archive list
# ---------------------------------------------------------------------------

.tp2 <- function(pkg, ver) paste0(pkg, "/", pkg, "_", ver, ".tar.gz")

.make_adf <- function(pkg, versions, dates, sizes = rep(1000, length(versions))) {
  paths        <- vapply(versions, function(v) .tp2(pkg, v), character(1L))
  mt           <- as.POSIXct(dates, tz = "UTC")
  df           <- data.frame(size = as.numeric(sizes), mtime = mt, stringsAsFactors = FALSE)
  rownames(df) <- paths
  df
}

# PkgArchived: 2 versions, archived
# PkgCurrent:  in the live CRAN listing (must be excluded)
# PkgSingle:   1 version, archived, dashed version string
FIXTURE_ARCHIVE <- list(
  PkgArchived = .make_adf("PkgArchived", c("1.0", "1.1"), c("2016-01-01", "2019-03-15")),
  PkgCurrent  = .make_adf("PkgCurrent",  c("0.5", "0.6"), c("2017-05-01", "2021-08-10")),
  PkgSingle   = .make_adf("PkgSingle",   c("3.0-1"),      c("2013-07-20"))
)
FIXTURE_CURRENT_PKGS <- c("PkgCurrent")

# Meta/current.rds and PACKAGES MD5 rows for the one live package.
FIXTURE_CURRENT_RDS <- data.frame(size = 2048, mtime = as.POSIXct("2024-02-01", tz = "UTC"),
                                  row.names = "PkgCurrent_0.7.tar.gz")
FIXTURE_MD5 <- data.frame(package = "PkgCurrent", version = "0.7",
                          md5sum = "5d41402abc4b2a76b9719d911017c592", stringsAsFactors = FALSE)

.empty_names_df <- function() {
  data.frame(name_lower = character(0), canonical_name = character(0),
             identity_state = character(0), first_seen = character(0),
             last_seen = character(0), stringsAsFactors = FALSE)
}

# A small read.dcf-shaped PACKAGES.in matrix for the lineage path.
.stub_dcf <- function() {
  read.dcf(textConnection(paste(
    "Package: PkgArchived",
    "X-CRAN-History: Archived on 2018-01-01 as checks failed.",
    "  Unarchived on 2018-02-01.",
    "",
    "Package: PkgSingle",
    "X-CRAN-Comment: Archived on 2013-07-21 as email bounced.",
    sep = "\n")))
}

make_stub_io <- function(archive = FIXTURE_ARCHIVE,
                         current = FIXTURE_CURRENT_PKGS,
                         reasons = character(0),
                         history = list(),
                         prev_names = .empty_names_df(),
                         packages_dcf = .stub_dcf(),
                         current_rds = FIXTURE_CURRENT_RDS,
                         packages_md5 = FIXTURE_MD5,
                         prev_tarballs = empty_tarballs()) {
  list(
    archive_rds      = function() archive,
    current_packages = function() current,
    removal_reasons  = function() reasons,
    removal_history  = function() history,
    prev_names       = function() prev_names,
    packages_dcf     = function() packages_dcf,
    current_rds      = function() current_rds,
    packages_md5     = function() packages_md5,
    prev_tarballs    = function() prev_tarballs
  )
}

# ---------------------------------------------------------------------------
# Output files
# ---------------------------------------------------------------------------

test_that("run_update writes cran-archive.db and manifest.json", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  run_update(make_stub_io(), out, force_full = TRUE)

  expect_true(file.exists(file.path(out, "cran-archive.db")))
  expect_true(file.exists(file.path(out, "manifest.json")))
})

# ---------------------------------------------------------------------------
# DB content
# ---------------------------------------------------------------------------

test_that("run_update: cran_archive excludes current packages", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  run_update(make_stub_io(), out, force_full = TRUE)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "cran-archive.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  pkgs <- RSQLite::dbGetQuery(con, "SELECT package FROM cran_archive ORDER BY package")
  expect_equal(pkgs$package, c("PkgArchived", "PkgSingle"))
  expect_false("PkgCurrent" %in% pkgs$package)
})

test_that("run_update: archived_on, first_release, and last_version correct in DB", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  run_update(make_stub_io(), out, force_full = TRUE)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "cran-archive.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  row <- RSQLite::dbGetQuery(con,
    "SELECT * FROM cran_archive WHERE package = 'PkgArchived'")
  expect_equal(row$first_release, "2016-01-01")
  expect_equal(row$archived_on,   "2019-03-15")
  expect_equal(row$last_version,  "1.1")
})

test_that("run_update: cran_archive_events has release + archived events per package", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  run_update(make_stub_io(), out, force_full = TRUE)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "cran-archive.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  ev <- RSQLite::dbGetQuery(con,
    "SELECT package, event_type, COUNT(*) AS n
     FROM cran_archive_events
     GROUP BY package, event_type
     ORDER BY package, event_type")

  pkga_rel  <- ev[ev$package == "PkgArchived" & ev$event_type == "release",  "n"]
  pkga_arch <- ev[ev$package == "PkgArchived" & ev$event_type == "archived", "n"]
  expect_equal(pkga_rel,  2L)
  expect_equal(pkga_arch, 1L)

  sing_rel  <- ev[ev$package == "PkgSingle" & ev$event_type == "release",  "n"]
  sing_arch <- ev[ev$package == "PkgSingle" & ev$event_type == "archived", "n"]
  expect_equal(sing_rel,  1L)
  expect_equal(sing_arch, 1L)
})

test_that("run_update: removal_reason populated from injected reasons map", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  reasons <- c(PkgArchived = "Policy violation")
  run_update(make_stub_io(reasons = reasons), out, force_full = TRUE)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "cran-archive.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  row <- RSQLite::dbGetQuery(con,
    "SELECT removal_reason FROM cran_archive WHERE package = 'PkgArchived'")
  expect_equal(row$removal_reason, "Policy violation")
})

test_that("run_update: removal_reason is NA when package absent from reasons map", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  run_update(make_stub_io(), out, force_full = TRUE)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "cran-archive.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  row <- RSQLite::dbGetQuery(con,
    "SELECT removal_reason FROM cran_archive WHERE package = 'PkgSingle'")
  expect_true(is.na(row$removal_reason))
})

# ---------------------------------------------------------------------------
# cran_archive_history
# ---------------------------------------------------------------------------

test_that("run_update: cran_archive_history has one open episode per archived pkg", {
  tmp <- withr::local_tempdir(); out <- file.path(tmp, "out")
  run_update(make_stub_io(), out, force_full = TRUE)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "cran-archive.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  open <- RSQLite::dbGetQuery(con,
    "SELECT package FROM cran_archive_history WHERE relisted_on IS NULL ORDER BY package")
  expect_equal(open$package, c("PkgArchived", "PkgSingle"))
})

test_that("run_update: cran_archive_history records closed cycles from history map", {
  tmp <- withr::local_tempdir(); out <- file.path(tmp, "out")
  hist <- list(PkgArchived = list(
    list(archived_on = "2018-01-01", removal_reason = "old", relisted_on = "2018-02-01")))
  run_update(make_stub_io(history = hist), out, force_full = TRUE)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "cran-archive.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  rows <- RSQLite::dbGetQuery(con,
    "SELECT episode_seq, archived_on, relisted_on FROM cran_archive_history
     WHERE package='PkgArchived' ORDER BY episode_seq")
  expect_equal(nrow(rows), 2L)                    # one closed (2018) + one open (2019)
  expect_equal(rows$relisted_on, c("2018-02-01", NA_character_))
})

# ---------------------------------------------------------------------------
# Manifest
# ---------------------------------------------------------------------------

test_that("run_update: manifest carries n_archived and n_events counts", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  res <- run_update(make_stub_io(), out, force_full = TRUE)
  expect_equal(res$manifest$n_archived, 2L)   # PkgArchived + PkgSingle
  # PkgArchived: 2 release + 1 archived = 3; PkgSingle: 1 + 1 = 2; total = 5
  expect_equal(res$manifest$n_events, 5L)
})

test_that("run_update: manifest has generated_at and archive_fingerprint", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  res <- run_update(make_stub_io(), out, force_full = TRUE)
  expect_true(nzchar(res$manifest$generated_at))
  expect_true(nzchar(res$manifest$source$archive_fingerprint))
})

# ---------------------------------------------------------------------------
# Change detection: fingerprint gate
# ---------------------------------------------------------------------------

test_that("run_update: changed=TRUE on first run (no prior manifest)", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  res <- run_update(make_stub_io(), out, force_full = FALSE, min_current = 0L, min_archive = 0L)
  expect_true(res$changed)
})

test_that("run_update: changed=FALSE on second run with identical input", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  run_update(make_stub_io(), out, force_full = FALSE, min_current = 0L, min_archive = 0L)
  res2 <- run_update(make_stub_io(), out, force_full = FALSE, min_current = 0L, min_archive = 0L)
  expect_false(res2$changed)
})

test_that("run_update: force_full=TRUE forces changed=TRUE even when fingerprint matches", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  run_update(make_stub_io(), out, force_full = FALSE, min_current = 0L, min_archive = 0L)
  res2 <- run_update(make_stub_io(), out, force_full = TRUE)
  expect_true(res2$changed)
})

test_that("run_update: changed=TRUE when archived set differs from prior manifest", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  # First run with the fixture
  run_update(make_stub_io(), out, force_full = FALSE, min_current = 0L, min_archive = 0L)

  # Second run with an extra archived package
  extra_archive <- c(
    FIXTURE_ARCHIVE,
    list(PkgNew = .make_adf("PkgNew", c("0.1"), c("2022-01-01")))
  )
  res2 <- run_update(make_stub_io(archive = extra_archive), out, force_full = FALSE,
                     min_current = 0L, min_archive = 0L)
  expect_true(res2$changed)
})

test_that("run_update: manifest.json on disk matches returned manifest", {
  tmp <- withr::local_tempdir()
  out <- file.path(tmp, "out")

  res <- run_update(make_stub_io(), out, force_full = TRUE)

  from_disk <- jsonlite::read_json(file.path(out, "manifest.json"))
  expect_equal(from_disk$n_archived, res$manifest$n_archived)
  expect_equal(from_disk$source$archive_fingerprint,
               res$manifest$source$archive_fingerprint)
})

# ---------------------------------------------------------------------------
# Integrity / completeness core
# ---------------------------------------------------------------------------

test_that("run_update: manifest carries the integrity core over the finalized DB", {
  tmp <- withr::local_tempdir(); out <- file.path(tmp, "out")

  # Low floors so the names write runs: the DB is the full, non-partial set.
  res <- run_update(make_stub_io(), out, force_full = TRUE,
                    live_floor = 0L, archive_floor = 0L)
  m  <- res$manifest
  db <- file.path(out, "cran-archive.db")

  expect_equal(m$db_filename, "cran-archive.db")
  expect_type(m$db_bytes, "double")
  expect_equal(m$db_bytes, file.size(db))
  expect_match(m$db_sha256, "^[0-9a-f]{64}$")
  # The hash is over the finalized file bytes: recomputing now (all connections
  # closed) reproduces it exactly.
  expect_equal(m$db_sha256, file_sha256(db))
  # tables enumerates the real user tables and their counts.
  expect_equal(m$tables$cran_archive, m$n_archived)
  expect_true("cran_names_all" %in% names(m$tables))
})

test_that("run_update: complete=TRUE when the full DB (incl. names) is written", {
  tmp <- withr::local_tempdir(); out <- file.path(tmp, "out")
  res <- run_update(make_stub_io(), out, force_full = TRUE,
                    live_floor = 0L, archive_floor = 0L)
  expect_false(is.na(res$manifest$n_names))
  expect_true(res$manifest$complete)
})

test_that("run_update: complete=FALSE and no names table when prior is unreachable", {
  tmp <- withr::local_tempdir(); out <- file.path(tmp, "out")
  io <- make_stub_io()
  io$prev_names <- function() stop("prior release unreachable")

  res <- run_update(io, out, force_full = TRUE, live_floor = 0L, archive_floor = 0L)
  m <- res$manifest

  expect_false(m$names_healthy)
  expect_true(is.na(m$n_names))
  expect_false(m$complete)                            # partial DB, honestly flagged
  expect_false("cran_names_all" %in% names(m$tables)) # names table genuinely absent
})

# ---------------------------------------------------------------------------
# Fetch-sanity guard
# ---------------------------------------------------------------------------

test_that("run_update: aborts on an implausibly small current-packages fetch", {
  tmp <- withr::local_tempdir(); out <- file.path(tmp, "out")
  # current list of length 1 is far below the floor; a truncated fetch must abort.
  expect_error(run_update(make_stub_io(current = c("PkgCurrent")), out, force_full = FALSE),
               "current_packages")
  expect_false(file.exists(file.path(out, "cran-archive.db")))
})

test_that("run_update: force_full bypasses the fetch-sanity floors", {
  tmp <- withr::local_tempdir(); out <- file.path(tmp, "out")
  expect_silent(run_update(make_stub_io(), out, force_full = TRUE))
})

# ---------------------------------------------------------------------------
# cran_tarballs
# ---------------------------------------------------------------------------

.read_tarballs <- function(out) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out, "cran-archive.db"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  if (!RSQLite::dbExistsTable(con, "cran_tarballs")) return(NULL)
  RSQLite::dbGetQuery(con, "SELECT * FROM cran_tarballs ORDER BY package, version, revision")
}

test_that("run_update: a first run records every archived and current file", {
  tmp <- withr::local_tempdir(); out <- file.path(tmp, "out")
  res <- run_update(make_stub_io(), out, force_full = TRUE, min_current = 0L, min_archive = 0L)
  t <- .read_tarballs(out)
  expect_equal(nrow(t), 6L)   # 5 archived files + PkgCurrent 0.7
  cur <- t[t$listing == "current", ]
  expect_equal(cur$package, "PkgCurrent")
  expect_equal(cur$size_bytes, 2048L)
  expect_equal(cur$md5sum, "5d41402abc4b2a76b9719d911017c592")
  expect_equal(res$manifest$tarballs_state, "cold_start")
  expect_equal(res$manifest$n_tarballs, 6L)
  expect_equal(res$manifest$tarball_revisions_new, 6L)
  expect_equal(res$manifest$tables$cran_tarballs, 6L)
})

test_that("run_update: a second run on the published table only moves last_seen", {
  tmp <- withr::local_tempdir(); out <- file.path(tmp, "out")
  run_update(make_stub_io(), out, force_full = TRUE, min_current = 0L, min_archive = 0L)
  prior <- .read_tarballs(out)
  prior$first_seen <- "2026-09-01"; prior$last_seen <- "2026-09-01"
  res <- run_update(make_stub_io(prev_tarballs = prior), out, force_full = TRUE,
                    min_current = 0L, min_archive = 0L)
  t <- .read_tarballs(out)
  expect_equal(res$manifest$tarballs_state, "updated")
  expect_equal(res$manifest$tarball_revisions_new, 0L)
  expect_true(all(t$first_seen == "2026-09-01"))
  expect_true(all(t$last_seen > "2026-09-01"))
})

test_that("run_update: a failed current index writes the prior table back unchanged", {
  tmp <- withr::local_tempdir(); out <- file.path(tmp, "out")
  run_update(make_stub_io(), out, force_full = TRUE, min_current = 0L, min_archive = 0L)
  prior <- .read_tarballs(out)
  io <- make_stub_io(prev_tarballs = prior)
  io$current_rds <- function() stop("HTTP 503")
  res <- suppressMessages(
    run_update(io, out, force_full = TRUE, min_current = 0L, min_archive = 0L))
  expect_equal(res$manifest$tarballs_state, "carried")
  expect_equal(res$manifest$tarball_revisions_new, 0L)
  expect_equal(.read_tarballs(out), prior)
})

test_that("run_update: a snapshot that fails shows its error in the run output and the manifest", {
  tmp <- withr::local_tempdir(); out <- file.path(tmp, "out")
  io <- make_stub_io()
  io$current_rds <- function() stop("HTTP 503")
  expect_message(
    res <- run_update(io, out, force_full = TRUE, min_current = 0L, min_archive = 0L),
    "cran_tarballs snapshot failed: HTTP 503", fixed = TRUE)
  expect_equal(res$manifest$tarballs_state, "carried")
  expect_equal(res$manifest$tarballs_error, "HTTP 503")
  on_disk <- jsonlite::read_json(file.path(out, "manifest.json"))
  expect_equal(on_disk$tarballs_state, "carried")
  expect_equal(on_disk$tarballs_error, "HTTP 503")
})

test_that("run_update: a snapshot that builds has no tarballs_error, whether merged or too small", {
  tmp <- withr::local_tempdir(); out <- file.path(tmp, "out")
  res <- run_update(make_stub_io(), out, force_full = TRUE, min_current = 0L, min_archive = 0L)
  expect_equal(res$manifest$tarballs_state, "cold_start")
  expect_true(is.na(res$manifest$tarballs_error))
  on_disk <- jsonlite::read_json(file.path(out, "manifest.json"))
  expect_true("tarballs_error" %in% names(on_disk))
  expect_null(on_disk$tarballs_error)

  # One current file is below a floor of two: carried, but nothing failed.
  expect_no_message(
    small <- run_update(make_stub_io(), out, force_full = TRUE, min_current = 2L, min_archive = 0L))
  expect_equal(small$manifest$tarballs_state, "carried")
  expect_true(is.na(small$manifest$tarballs_error))
  expect_null(jsonlite::read_json(file.path(out, "manifest.json"))$tarballs_error)
})

test_that("run_update: an unreadable prior table writes none and marks the database partial", {
  tmp <- withr::local_tempdir(); out <- file.path(tmp, "out")
  io <- make_stub_io()
  io$prev_tarballs <- function() stop("database disk image is malformed")
  res <- run_update(io, out, force_full = TRUE, min_current = 0L, min_archive = 0L,
                    live_floor = 0L, archive_floor = 0L)
  expect_null(.read_tarballs(out))
  expect_equal(res$manifest$tarballs_state, "unreachable")
  expect_true(res$manifest$names_healthy)
  expect_false(res$manifest$complete)
  on_disk <- jsonlite::read_json(file.path(out, "manifest.json"))
  expect_null(on_disk$n_tarballs)
  expect_null(on_disk$tarballs_error)
})

test_that("run_update: an unreachable prior database skips cran_tarballs as well", {
  tmp <- withr::local_tempdir(); out <- file.path(tmp, "out")
  io <- make_stub_io()
  io$prev_names <- function() stop("prior release unreachable")
  res <- run_update(io, out, force_full = TRUE, min_current = 0L, min_archive = 0L)
  expect_null(.read_tarballs(out))
  expect_equal(res$manifest$tarballs_state, "skipped")
  expect_null(jsonlite::read_json(file.path(out, "manifest.json"))$tarballs_error)
})

# ---------------------------------------------------------------------------
# cran_tarballs hashes
# ---------------------------------------------------------------------------

# The stub's one current file is the 5 bytes "hello", whose MD5 FIXTURE_MD5 gives.
.CURRENT_HELLO <- data.frame(size = 5, mtime = as.POSIXct("2024-02-01", tz = "UTC"),
                             row.names = "PkgCurrent_0.7.tar.gz")
.HELLO_SHA256 <- "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"

# Every file the stub lists, laid out as the mirror serves them: each archived
# file is 1000 bytes, as FIXTURE_ARCHIVE records.
.stub_mirror <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  put <- function(path, text) {
    f <- file.path(root, path)
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    writeBin(charToRaw(text), f)
  }
  for (pkg in names(FIXTURE_ARCHIVE)) {
    for (p in rownames(FIXTURE_ARCHIVE[[pkg]])) {
      put(file.path("src", "contrib", "Archive", p), formatC(p, width = -1000))
    }
  }
  put("src/contrib/PkgCurrent_0.7.tar.gz", "hello")
  root
}

.stub_fetch <- function(root) {
  log <- new.env()
  log$urls <- character(0)
  log$fetch <- function(urls, destfiles) {
    log$urls <- c(log$urls, urls)
    src <- file.path(root, substring(urls, nchar(TARBALL_MIRROR_URL) + 2L))
    ok <- file.exists(src)
    file.copy(src[ok], destfiles[ok], overwrite = TRUE)
    ok
  }
  log
}

.hashing_io <- function(fetch, ...) {
  io <- make_stub_io(current_rds = .CURRENT_HELLO, ...)
  io$fetch_tarballs <- fetch
  io
}

test_that("run_update: a first run hashes every listed file, current first, and reports the counts", {
  tmp <- withr::local_tempdir(); out <- file.path(tmp, "out")
  f <- .stub_fetch(.stub_mirror())
  res <- suppressMessages(run_update(.hashing_io(f$fetch), out, force_full = TRUE,
                                     min_current = 0L, min_archive = 0L))
  t <- .read_tarballs(out)
  expect_equal(nrow(t), 6L)
  expect_false(anyNA(t$sha256))
  expect_false(anyNA(t$md5sum))
  expect_false(anyNA(t$hashed_at))
  expect_equal(t$sha256[t$package == "PkgCurrent" & t$listing == "current"], .HELLO_SHA256)
  expect_equal(f$urls[1], paste0(TARBALL_MIRROR_URL, "/src/contrib/PkgCurrent_0.7.tar.gz"))
  expect_length(f$urls, 6L)

  m <- res$manifest
  expect_equal(m$tarballs_hashed, 6L)
  expect_equal(m$tarballs_to_hash, 0L)
  expect_equal(m$tarball_hash_mismatches, 0L)
  expect_equal(m$tarball_hash_failures, 0L)
  expect_equal(m$tarball_hash_stop, "done")
  on_disk <- jsonlite::read_json(file.path(out, "manifest.json"))
  expect_equal(on_disk$tarballs_hashed, 6L)
  expect_equal(on_disk$tarballs_to_hash, 0L)
  expect_equal(on_disk$tarball_hash_mismatches, 0L)
  # The integrity core covers the file as it is after the hashes went in.
  expect_equal(m$db_sha256, file_sha256(file.path(out, "cran-archive.db")))
})

test_that("run_update: a run stopped by its budget publishes what it hashed, and the next run goes on", {
  tmp <- withr::local_tempdir(); out <- file.path(tmp, "out")
  mirror <- .stub_mirror()
  f1 <- .stub_fetch(mirror)
  res1 <- suppressMessages(run_update(.hashing_io(f1$fetch), out, force_full = TRUE,
                                      min_current = 0L, min_archive = 0L, hash_max_files = 2L))
  expect_equal(res1$manifest$tarballs_hashed, 2L)
  expect_equal(res1$manifest$tarballs_to_hash, 4L)
  expect_equal(res1$manifest$tarball_hash_stop, "count")
  t1 <- .read_tarballs(out)
  expect_equal(sum(!is.na(t1$sha256)), 2L)

  f2 <- .stub_fetch(mirror)
  res2 <- suppressMessages(run_update(.hashing_io(f2$fetch, prev_tarballs = t1), out,
                                      force_full = TRUE, min_current = 0L, min_archive = 0L))
  expect_length(f2$urls, 4L)
  expect_false(any(f2$urls %in% f1$urls))
  expect_equal(res2$manifest$tarballs_hashed, 4L)
  expect_equal(res2$manifest$tarballs_to_hash, 0L)
  t2 <- .read_tarballs(out)
  expect_false(anyNA(t2$sha256))
  # The first run's hashes carried over as they were.
  kept <- !is.na(t1$sha256)
  expect_equal(t2$sha256[kept], t1$sha256[kept])
  expect_equal(t2$hashed_at[kept], t1$hashed_at[kept])
})

test_that("run_update: a deadline already past hashes nothing and still publishes the table", {
  tmp <- withr::local_tempdir(); out <- file.path(tmp, "out")
  f <- .stub_fetch(.stub_mirror())
  res <- suppressMessages(run_update(.hashing_io(f$fetch), out, force_full = TRUE,
                                     min_current = 0L, min_archive = 0L,
                                     hash_deadline = Sys.time() - 1))
  expect_length(f$urls, 0L)
  expect_equal(res$manifest$tarballs_state, "cold_start")
  expect_equal(res$manifest$tarballs_hashed, 0L)
  expect_equal(res$manifest$tarballs_to_hash, 6L)
  expect_equal(res$manifest$tarball_hash_stop, "time")
})

test_that("run_update: nothing is hashed when the prior table could not be read", {
  tmp <- withr::local_tempdir(); out <- file.path(tmp, "out")
  f <- .stub_fetch(.stub_mirror())
  io <- .hashing_io(f$fetch)
  io$prev_tarballs <- function() stop("database disk image is malformed")
  res <- suppressMessages(run_update(io, out, force_full = TRUE, min_current = 0L, min_archive = 0L))
  expect_length(f$urls, 0L)
  expect_equal(res$manifest$tarball_hash_stop, "skipped")
  on_disk <- jsonlite::read_json(file.path(out, "manifest.json"))
  expect_null(on_disk$tarballs_hashed)
  expect_null(on_disk$tarballs_to_hash)
  expect_equal(on_disk$tarball_hash_stop, "skipped")
})

test_that("run_update: an io without a fetch leaves the hashes alone", {
  tmp <- withr::local_tempdir(); out <- file.path(tmp, "out")
  res <- run_update(make_stub_io(), out, force_full = TRUE, min_current = 0L, min_archive = 0L)
  expect_equal(res$manifest$tarball_hash_stop, "skipped")
  expect_true(all(is.na(.read_tarballs(out)$sha256)))
})
