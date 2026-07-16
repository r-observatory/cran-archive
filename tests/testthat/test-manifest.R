# tests/testthat/test-manifest.R: integrity / completeness core carried in the
# release manifest (file_sha256, db_integrity_core) and its top-level merge into
# the manifest JSON. helpers.R is sourced by tests/testthat.R before this runs.

# Build a tiny, real SQLite file with a couple of user tables. The TEXT PRIMARY
# KEY creates an implicit sqlite_autoindex_* object, so the enumeration also has
# to exclude internal sqlite_% objects.
build_fixture_db <- function(n_archive = 3L, n_events = 5L) {
  tmp <- tempfile(fileext = ".db")
  con <- RSQLite::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  RSQLite::dbExecute(con,
    "CREATE TABLE cran_archive (package TEXT PRIMARY KEY, archived_on TEXT)")
  RSQLite::dbExecute(con,
    "CREATE TABLE cran_archive_events (package TEXT, event_type TEXT)")
  RSQLite::dbWriteTable(con, "cran_archive", data.frame(
    package     = paste0("pkg", seq_len(n_archive)),
    archived_on = rep("2020-01-01", n_archive),
    stringsAsFactors = FALSE), append = TRUE)
  RSQLite::dbWriteTable(con, "cran_archive_events", data.frame(
    package    = paste0("pkg", seq_len(n_events)),
    event_type = rep("archived", n_events),
    stringsAsFactors = FALSE), append = TRUE)
  RSQLite::dbExecute(con, "VACUUM")
  tmp
}

test_that("file_sha256 returns lowercase 64-char hex", {
  db <- build_fixture_db(1L, 1L)
  on.exit(unlink(db))
  h <- file_sha256(db)
  expect_match(h, "^[0-9a-f]{64}$")
})

test_that("db_integrity_core reports filename, bytes, sha256, tables, complete", {
  db <- build_fixture_db(3L, 5L)
  on.exit(unlink(db))

  core <- db_integrity_core(db, complete = TRUE)

  expect_equal(core$db_filename, basename(db))
  # db_bytes is a double (not cast to integer) so a file >= ~2 GiB does not
  # overflow to NA; compare against the uncast file.size() directly.
  expect_type(core$db_bytes, "double")
  expect_equal(core$db_bytes, file.size(db))
  # sha256 is the lowercase 64-char hex of the exact file bytes
  expect_match(core$db_sha256, "^[0-9a-f]{64}$")
  # tables maps every user table (internal sqlite_% objects excluded) to a count
  expect_named(core$tables, c("cran_archive", "cran_archive_events"))
  expect_equal(core$tables$cran_archive, 3L)
  expect_equal(core$tables$cran_archive_events, 5L)
  expect_true(core$complete)
})

test_that("db_integrity_core sha256 matches an independent system digest", {
  # Cross-check against an external CLI, independent of file_sha256()'s own
  # preferred backend (digest/openssl), so this genuinely verifies the hash
  # rather than re-running the same library. Skip ONLY if neither tool is on
  # PATH (both are expected on CI); do not gate on any R package being present.
  sha256sum_bin <- Sys.which("sha256sum")
  shasum_bin    <- Sys.which("shasum")
  if (!nzchar(sha256sum_bin) && !nzchar(shasum_bin)) {
    skip("neither sha256sum nor shasum is on PATH")
  }

  db <- build_fixture_db(2L, 2L)
  on.exit(unlink(db))

  core <- db_integrity_core(db)

  if (nzchar(sha256sum_bin)) {
    out <- system2(sha256sum_bin, shQuote(db), stdout = TRUE)
  } else {
    out <- system2(shasum_bin, c("-a", "256", shQuote(db)), stdout = TRUE)
  }
  independent <- tolower(sub("\\s.*$", "", out[1]))

  expect_equal(core$db_sha256, independent)
})

test_that("db_integrity_core passes complete through honestly", {
  db <- build_fixture_db(1L, 1L)
  on.exit(unlink(db))
  expect_false(db_integrity_core(db, complete = FALSE)$complete)
})

test_that("the integrity core serializes as top-level manifest fields", {
  db <- build_fixture_db(4L, 6L)
  on.exit(unlink(db), add = TRUE)
  core <- db_integrity_core(db, complete = TRUE)

  # Mirror how run_update() merges the core into the manifest: c(base, core)
  # attaches the core's members as TOP-LEVEL fields, not nested.
  obj <- c(
    list(
      release      = "v20260715-000000",
      generated_at = "2026-07-15T00:00:00Z",
      n_archived   = 4L,
      names_healthy = TRUE,
      source       = list(archive_fingerprint = "deadbeef")
    ),
    core
  )

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  write_manifest(tmp, obj)

  parsed <- jsonlite::read_json(tmp, simplifyVector = FALSE)
  # existing fields preserved
  expect_equal(parsed$release, "v20260715-000000")
  expect_equal(parsed$source$archive_fingerprint, "deadbeef")
  # new top-level integrity / completeness core
  expect_equal(parsed$db_filename, basename(db))
  expect_true(is.numeric(parsed$db_bytes))
  expect_equal(parsed$db_bytes, file.size(db))
  expect_match(parsed$db_sha256, "^[0-9a-f]{64}$")
  expect_equal(parsed$tables$cran_archive, 4L)
  expect_equal(parsed$tables$cran_archive_events, 6L)
  expect_true(parsed$complete)

  # db_bytes is emitted as a bare JSON number, never the string "NA".
  raw <- readLines(tmp, warn = FALSE)
  expect_true(any(grepl('"db_bytes"\\s*:\\s*[0-9]', raw)))
  expect_false(any(grepl('"db_bytes"\\s*:\\s*"', raw)))
})
