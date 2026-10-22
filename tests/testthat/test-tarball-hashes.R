# Hashing every cran_tarballs file once: MD5 and SHA-256 from the downloaded
# bytes, verified against the recorded size and CRAN's MD5 before anything is
# stored. Nothing here touches the network: the mirror is a temporary folder
# and the fetch copies from it. The file contents are the standard digest test
# strings, so the expected hashes are the published values.

ABC    <- list(text = "abc", md5 = "900150983cd24fb0d6963f7d28e17f72",
               sha256 = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
DIGEST <- list(text = "message digest", md5 = "f96b697d7cb7938d525a2f31aaf161d0",
               sha256 = "f7846f55cf23e14eebeab5b4e1550cad5b509e3348fbc4efa3a1413d393cb650")
FOX    <- list(text = "The quick brown fox jumps over the lazy dog",
               md5 = "9e107d9d372bb6826bd81d3542a419d6",
               sha256 = "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592")

# A cran_tarballs frame, one row per file.
.rows <- function(package, version, revision, size, mtime, md5, listing,
                  sha256 = NA_character_, hashed_at = NA_character_) {
  n <- length(package)
  data.frame(package = package, version = version, revision = as.integer(revision),
             size_bytes = as.integer(size), mtime = mtime, md5sum = as.character(md5),
             listing = listing, first_seen = rep("2026-10-01", n),
             last_seen = rep("2026-10-01", n), sha256 = rep_len(as.character(sha256), n),
             hashed_at = rep_len(as.character(hashed_at), n), stringsAsFactors = FALSE)
}

# cli 3.6.6 is current with CRAN's MD5. lmeInfo 0.3.2 was uploaded twice: the
# first file is in the archive without an MD5, the second is current without one.
.three <- function() .rows(
  package  = c("cli", "lmeInfo", "lmeInfo"),
  version  = c("3.6.6", "0.3.2", "0.3.2"),
  revision = c(1, 1, 2),
  size     = c(3, 43, 14),
  mtime    = c("2026-04-09T09:50:29Z", "2023-04-17T08:30:07Z", "2026-09-27T15:46:29Z"),
  md5      = c(ABC$md5, NA, NA),
  listing  = c("current", "archive", "current"))

.three_files <- function() list(
  "src/contrib/cli_3.6.6.tar.gz"                    = ABC$text,
  "src/contrib/Archive/lmeInfo/lmeInfo_0.3.2.tar.gz" = FOX$text,
  "src/contrib/lmeInfo_0.3.2.tar.gz"                = DIGEST$text)

# A mirror on disk: path under the mirror root -> file content.
.local_mirror <- function(files, env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  for (p in names(files)) {
    f <- file.path(root, p)
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    writeBin(charToRaw(files[[p]]), f)
  }
  root
}

# A fetch that copies from the local mirror, records each URL it is asked for
# and, when tick is set, moves the clock on by that many seconds per call.
.copy_fetch <- function(root, clock = NULL, tick = 0) {
  log <- new.env()
  log$urls <- character(0)
  log$fetch <- function(urls, destfiles) {
    log$urls <- c(log$urls, urls)
    if (!is.null(clock)) clock$now <- clock$now + tick
    src <- file.path(root, substring(urls, nchar(TARBALL_MIRROR_URL) + 2L))
    ok <- file.exists(src)
    file.copy(src[ok], destfiles[ok], overwrite = TRUE)
    ok
  }
  log
}

.fake_clock <- function(start = "2026-10-23 05:10:00") {
  clock <- new.env()
  clock$now <- as.POSIXct(start, tz = "UTC")
  clock$read <- function() clock$now
  clock
}

.no_pause <- function() {
  p <- new.env()
  p$waits <- numeric(0)
  p$pause <- function(secs) p$waits <- c(p$waits, secs)
  p
}

.hash_db <- function(rows, env = parent.frame()) {
  db <- withr::local_tempfile(fileext = ".db", .local_envir = env)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  export_tarballs(con, rows)
  RSQLite::dbDisconnect(con)
  db
}

.table <- function(db) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  RSQLite::dbGetQuery(con, "SELECT * FROM cran_tarballs ORDER BY package, version, revision")
}

.hash <- function(db, fetch, clock = .fake_clock(), deadline = clock$now + 3600,
                  pause = .no_pause()$pause, ...) {
  hash_tarballs(db, fetch, deadline = deadline, clock = clock$read, pause = pause, ...)
}

# --- configuration -------------------------------------------------------------

test_that("files come from the CDN mirror, named by a user agent with the repository", {
  expect_equal(TARBALL_MIRROR_URL, "https://cloud.r-project.org")
  expect_match(TARBALL_HASH_USER_AGENT, "r-observatory/cran-archive", fixed = TRUE)
  expect_match(TARBALL_HASH_USER_AGENT, "https://github.com/r-observatory/cran-archive", fixed = TRUE)
  expect_lte(TARBALL_HASH_CONCURRENCY, 4L)
  expect_gt(TARBALL_HASH_ATTEMPTS, 1L)
})

test_that("a current file is read from src/contrib and an archived one from Archive/<pkg>", {
  got <- tarball_url(c("cli", "lmeInfo"), c("3.6.6", "0.3.2"), c("current", "archive"),
                     "https://cloud.r-project.org")
  expect_equal(got, c("https://cloud.r-project.org/src/contrib/cli_3.6.6.tar.gz",
                      "https://cloud.r-project.org/src/contrib/Archive/lmeInfo/lmeInfo_0.3.2.tar.gz"))
})

# --- an old state gains the columns --------------------------------------------

.old_schema_db <- function(env = parent.frame()) {
  db <- withr::local_tempfile(fileext = ".db", .local_envir = env)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  RSQLite::dbExecute(con, "
    CREATE TABLE cran_tarballs (
      package TEXT NOT NULL, version TEXT NOT NULL, revision INTEGER NOT NULL,
      size_bytes INTEGER NOT NULL, mtime TEXT NOT NULL, md5sum TEXT,
      listing TEXT NOT NULL, first_seen TEXT NOT NULL, last_seen TEXT NOT NULL,
      PRIMARY KEY (package, version, revision),
      CHECK (last_seen >= first_seen)) WITHOUT ROWID")
  RSQLite::dbExecute(con, "INSERT INTO cran_tarballs VALUES
    ('cli', '3.6.6', 1, 644134, '2026-04-09T09:50:29Z', 'eedc08ba864ae6cd640b05eff1a607ee',
     'current', '2026-10-01', '2026-10-22'),
    ('lmeInfo', '0.3.2', 1, 65124, '2023-04-17T08:30:07Z', NULL, 'archive',
     '2026-10-01', '2026-10-22')")
  RSQLite::dbDisconnect(con)
  db
}

test_that("a state written before the hash columns gains them by ALTER TABLE, rows untouched", {
  db  <- .old_schema_db()
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  before <- RSQLite::dbGetQuery(con, "SELECT rootpage, sql FROM sqlite_master WHERE name = 'cran_tarballs'")
  RSQLite::dbDisconnect(con)

  got <- read_prev_tarballs(db)
  expect_equal(names(got), TARBALL_COLS)
  expect_equal(nrow(got), 2L)
  expect_type(got$sha256, "character")
  expect_type(got$hashed_at, "character")
  expect_true(all(is.na(got$sha256) & is.na(got$hashed_at)))
  expect_equal(got$md5sum, c("eedc08ba864ae6cd640b05eff1a607ee", NA))
  expect_equal(got$last_seen, c("2026-10-22", "2026-10-22"))

  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  info <- RSQLite::dbGetQuery(con, "PRAGMA table_info(cran_tarballs)")
  expect_equal(info$name, TARBALL_COLS)
  expect_equal(info$type[info$name %in% c("sha256", "hashed_at")], c("TEXT", "TEXT"))
  expect_equal(info$notnull[info$name %in% c("sha256", "hashed_at")], c(0L, 0L))
  after <- RSQLite::dbGetQuery(con, "SELECT rootpage, sql FROM sqlite_master WHERE name = 'cran_tarballs'")
  # The same table, not a rebuilt copy, still keyed WITHOUT ROWID.
  expect_equal(after$rootpage, before$rootpage)
  expect_match(after$sql, "WITHOUT ROWID", fixed = TRUE)
})

test_that("an empty or missing prior download is unreachable, never an empty start", {
  db <- .old_schema_db()
  expect_true(prior_download_ok(0L, db))
  expect_false(prior_download_ok(1L, db))
  expect_false(prior_download_ok(0L, file.path(withr::local_tempdir(), "missing.db")))
  empty <- withr::local_tempfile(fileext = ".db")
  file.create(empty)
  expect_false(prior_download_ok(0L, empty))
})

test_that("reading a state that already has the hash columns changes nothing", {
  db <- .hash_db(.three())
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  sql <- RSQLite::dbGetQuery(con, "SELECT sql FROM sqlite_master WHERE name = 'cran_tarballs'")$sql
  RSQLite::dbDisconnect(con)
  got <- read_prev_tarballs(db)
  got2 <- read_prev_tarballs(db)
  expect_identical(got, got2)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  expect_identical(RSQLite::dbGetQuery(con, "SELECT sql FROM sqlite_master WHERE name = 'cran_tarballs'")$sql, sql)
})

test_that("a fresh table and a migrated one have the same columns in the same order", {
  fresh <- .hash_db(.three())
  old   <- .old_schema_db()
  read_prev_tarballs(old)
  cols <- function(db) {
    con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
    on.exit(RSQLite::dbDisconnect(con), add = TRUE)
    RSQLite::dbGetQuery(con, "PRAGMA table_info(cran_tarballs)")[, c("name", "type", "notnull", "pk")]
  }
  expect_equal(cols(fresh), cols(old))
})

# --- carrying hashes from run to run -------------------------------------------

test_that("a hashed file keeps its hashes, and a re-upload of its version is a new row to hash", {
  day1 <- .rows("lmeInfo", "0.3.2", 1, 43, "2023-04-17T08:30:07Z", FOX$md5, "current",
                sha256 = FOX$sha256, hashed_at = "2026-10-20T05:12:00Z")
  snap <- data.frame(
    package = c("lmeInfo", "lmeInfo"), version = c("0.3.2", "0.3.2"),
    size_bytes = c(43L, 14L), mtime = c("2023-04-17T08:30:07Z", "2026-09-27T15:46:29Z"),
    md5sum = c(NA, NA), listing = c("archive", "current"), stringsAsFactors = FALSE)
  got <- merge_tarballs(day1, snap, "2026-10-21", storm_max = 500L)$table
  expect_equal(got$revision, c(1L, 2L))
  expect_equal(got$sha256, c(FOX$sha256, NA))
  expect_equal(got$hashed_at, c("2026-10-20T05:12:00Z", NA))
  expect_equal(got$md5sum, c(FOX$md5, NA))
})

test_that("PACKAGES no longer changes the MD5 of a file whose bytes were hashed", {
  day1 <- .rows("cli", "3.6.6", 1, 3, "2026-04-09T09:50:29Z", ABC$md5, "current",
                sha256 = ABC$sha256, hashed_at = "2026-10-20T05:12:00Z")
  snap <- data.frame(package = "cli", version = "3.6.6", size_bytes = 3L,
                     mtime = "2026-04-09T09:50:29Z", md5sum = DIGEST$md5,
                     listing = "current", stringsAsFactors = FALSE)
  got <- merge_tarballs(day1, snap, "2026-10-21", storm_max = 500L)$table
  expect_equal(got$md5sum, ABC$md5)
  expect_equal(got$last_seen, "2026-10-21")
})

# --- the order a run takes files in --------------------------------------------

test_that("current files come first, then archived files of packages on CRAN, then the rest, newest first", {
  df <- .rows(
    package  = c("pkgA", "pkgB", "pkgA", "pkgA", "gonepkg", "gonepkg", "pkgB", "pkgB", "pkgC", "pkgC"),
    version  = c("2.0", "1.0", "1.0", "1.5", "0.9", "0.8", "0.9", "0.5", "3.0", "2.0"),
    revision = rep(1, 10),
    size     = rep(10, 10),
    mtime    = c("2026-01-01T00:00:00Z", "2026-05-01T00:00:00Z", "2020-01-01T00:00:00Z",
                 "2024-01-01T00:00:00Z", "2025-01-01T00:00:00Z", "2019-01-01T00:00:00Z",
                 "2022-01-01T00:00:00Z", "2021-01-01T00:00:00Z", "2026-02-01T00:00:00Z",
                 "2018-01-01T00:00:00Z"),
    md5      = rep(NA, 10),
    listing  = c("current", "current", "archive", "archive", "archive", "archive",
                 "gone", "archive", "current", "archive"),
    sha256   = c(NA, NA, NA, NA, NA, NA, NA, "ab", "cd", NA))
  q <- tarball_hash_queue(df)
  # pkgB 0.9 is gone and pkgB 0.5 is hashed, so neither is queued. pkgC's
  # current file is hashed already, and pkgC is still on CRAN.
  expect_equal(paste(q$package, q$version),
               c("pkgB 1.0", "pkgA 2.0", "pkgA 1.5", "pkgA 1.0", "pkgC 2.0",
                 "gonepkg 0.9", "gonepkg 0.8"))
})

test_that("an empty table gives an empty queue", {
  expect_equal(nrow(tarball_hash_queue(.rows(character(0), character(0), integer(0),
    integer(0), character(0), character(0), character(0)))), 0L)
})

# --- hashing ---------------------------------------------------------------------

test_that("each file is downloaded once and its verified MD5 and SHA-256 are stored", {
  db <- .hash_db(.three())
  f  <- .copy_fetch(.local_mirror(.three_files()))
  clock <- .fake_clock("2026-10-23 05:10:00")
  res <- suppressMessages(.hash(db, f$fetch, clock = clock))

  t <- .table(db)
  expect_equal(t$sha256, c(ABC$sha256, FOX$sha256, DIGEST$sha256))
  # CRAN's MD5 stays; an empty one is filled from the bytes.
  expect_equal(t$md5sum, c(ABC$md5, FOX$md5, DIGEST$md5))
  expect_equal(t$hashed_at, rep("2026-10-23T05:10:00Z", 3))
  expect_equal(res[c("hashed", "to_hash", "mismatches", "failures", "stop")],
               list(hashed = 3L, to_hash = 0L, mismatches = 0L, failures = 0L, stop = "done"))
  expect_equal(sort(f$urls), sort(paste0(TARBALL_MIRROR_URL, "/", names(.three_files()))))
})

test_that("the run log gives the counts", {
  db <- .hash_db(.three())
  f  <- .copy_fetch(.local_mirror(.three_files()))
  expect_message(.hash(db, f$fetch),
                 "3 hashed this run, 0 still to hash, 0 mismatches, 0 failed downloads", fixed = TRUE)
})

test_that("files are fetched in queue order, a few at a time", {
  db <- .hash_db(.three())
  f  <- .copy_fetch(.local_mirror(.three_files()))
  calls <- new.env()
  calls$sizes <- integer(0)
  fetch <- function(urls, destfiles) {
    calls$sizes <- c(calls$sizes, length(urls))
    f$fetch(urls, destfiles)
  }
  suppressMessages(.hash(db, fetch, batch = 2L))
  expect_equal(calls$sizes, c(2L, 1L))
  # Current files first, newest first: lmeInfo's second file, then cli, then the archive.
  expect_equal(f$urls, paste0(TARBALL_MIRROR_URL, "/", c(
    "src/contrib/lmeInfo_0.3.2.tar.gz", "src/contrib/cli_3.6.6.tar.gz",
    "src/contrib/Archive/lmeInfo/lmeInfo_0.3.2.tar.gz")))
})

test_that("a file whose size differs from the record stores nothing and is logged", {
  rows <- .three()
  rows$size_bytes[2] <- 44L
  db <- .hash_db(rows)
  f  <- .copy_fetch(.local_mirror(.three_files()))
  expect_message(res <- .hash(db, f$fetch), "lmeInfo 0.3.2 revision 1.*43 bytes, 44 expected")
  t <- .table(db)
  expect_true(is.na(t$sha256[2]))
  expect_true(is.na(t$md5sum[2]))
  expect_true(is.na(t$hashed_at[2]))
  expect_equal(t$sha256[c(1, 3)], c(ABC$sha256, DIGEST$sha256))
  expect_equal(res$mismatches, 1L)
  expect_equal(res$hashed, 2L)
  expect_equal(res$to_hash, 1L)
})

test_that("a file whose MD5 differs from CRAN's stores nothing and keeps CRAN's MD5", {
  rows <- .three()
  rows$md5sum[1] <- DIGEST$md5
  db <- .hash_db(rows)
  f  <- .copy_fetch(.local_mirror(.three_files()))
  expect_message(res <- .hash(db, f$fetch),
                 sprintf("cli 3.6.6 revision 1.*MD5 %s, %s expected", ABC$md5, DIGEST$md5))
  t <- .table(db)
  expect_true(is.na(t$sha256[1]))
  expect_equal(t$md5sum[1], DIGEST$md5)
  expect_equal(res$mismatches, 1L)
  expect_equal(res$hashed, 2L)
})

test_that("a failed download is retried with growing waits, then stores nothing", {
  files <- .three_files()
  files[["src/contrib/cli_3.6.6.tar.gz"]] <- NULL
  db <- .hash_db(.three())
  f  <- .copy_fetch(.local_mirror(files))
  p  <- .no_pause()
  expect_message(res <- .hash(db, f$fetch, pause = p$pause, attempts = 3L),
                 "download failed, nothing stored: cli 3.6.6 revision 1")
  expect_equal(sum(f$urls == paste0(TARBALL_MIRROR_URL, "/src/contrib/cli_3.6.6.tar.gz")), 3L)
  expect_length(p$waits, 2L)
  expect_gt(p$waits[2], p$waits[1])
  expect_true(is.na(.table(db)$sha256[1]))
  expect_equal(res$failures, 1L)
  expect_equal(res$mismatches, 0L)
  expect_equal(res$hashed, 2L)
})

test_that("a download that succeeds on a retry is stored", {
  db <- .hash_db(.three())
  f  <- .copy_fetch(.local_mirror(.three_files()))
  tries <- new.env()
  tries$n <- 0L
  flaky <- function(urls, destfiles) {
    tries$n <- tries$n + 1L
    if (tries$n == 1L) stop("Timeout was reached")
    f$fetch(urls, destfiles)
  }
  res <- suppressMessages(.hash(db, flaky))
  expect_equal(res$hashed, 3L)
  expect_equal(res$failures, 0L)
  expect_false(anyNA(.table(db)$sha256))
})

test_that("a fetch that says it succeeded but left no file counts as a failure", {
  db <- .hash_db(.three()[1, ])
  res <- suppressMessages(.hash(db, function(urls, destfiles) rep(TRUE, length(urls))))
  expect_equal(res$failures, 1L)
  expect_equal(res$hashed, 0L)
})

test_that("gone files are never fetched", {
  rows <- .three()
  rows$listing[2] <- "gone"
  db <- .hash_db(rows)
  f  <- .copy_fetch(.local_mirror(.three_files()))
  res <- suppressMessages(.hash(db, f$fetch))
  expect_false(any(grepl("Archive", f$urls)))
  expect_equal(res$hashed, 2L)
  expect_equal(res$to_hash, 0L)
})

test_that("a file is never hashed twice, and a re-upload is hashed on its own", {
  rows <- .three()[2, ]
  db <- .hash_db(rows)
  mirror <- .local_mirror(.three_files())
  f1 <- .copy_fetch(mirror)
  suppressMessages(.hash(db, f1$fetch))
  expect_length(f1$urls, 1L)

  f2 <- .copy_fetch(mirror)
  res <- suppressMessages(.hash(db, f2$fetch))
  expect_length(f2$urls, 0L)
  expect_equal(res[c("hashed", "to_hash", "stop")], list(hashed = 0L, to_hash = 0L, stop = "done"))

  # lmeInfo 0.3.2 is uploaded again: the hashed file moves to the archive and
  # the new one is revision 2.
  snap <- data.frame(
    package = c("lmeInfo", "lmeInfo"), version = c("0.3.2", "0.3.2"),
    size_bytes = c(43L, 14L), mtime = c("2023-04-17T08:30:07Z", "2026-09-27T15:46:29Z"),
    md5sum = c(NA, NA), listing = c("archive", "current"), stringsAsFactors = FALSE)
  merged <- merge_tarballs(read_prev_tarballs(db), snap, "2026-10-24", storm_max = 500L)$table
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  export_tarballs(con, merged)
  RSQLite::dbDisconnect(con)

  f3 <- .copy_fetch(mirror)
  res3 <- suppressMessages(.hash(db, f3$fetch))
  expect_equal(f3$urls, paste0(TARBALL_MIRROR_URL, "/src/contrib/lmeInfo_0.3.2.tar.gz"))
  expect_equal(res3$hashed, 1L)
  expect_equal(.table(db)$sha256, c(FOX$sha256, DIGEST$sha256))
})

# --- the budget ------------------------------------------------------------------

test_that("the count budget stops the run after that many files and keeps what it hashed", {
  db <- .hash_db(.three())
  f  <- .copy_fetch(.local_mirror(.three_files()))
  res <- suppressMessages(.hash(db, f$fetch, max_files = 2L, batch = 1L))
  expect_length(f$urls, 2L)
  expect_equal(res[c("hashed", "to_hash", "stop")], list(hashed = 2L, to_hash = 1L, stop = "count"))
  t <- .table(db)
  expect_equal(sum(!is.na(t$sha256)), 2L)
  # The one left is the archived file, last in the queue.
  expect_true(is.na(t$sha256[t$listing == "archive"]))
})

test_that("the time budget stops the run cleanly and keeps what it hashed", {
  db <- .hash_db(.three())
  clock <- .fake_clock("2026-10-23 05:00:00")
  f  <- .copy_fetch(.local_mirror(.three_files()), clock = clock, tick = 100)
  res <- suppressMessages(.hash(db, f$fetch, clock = clock, deadline = clock$now + 150, batch = 1L))
  # Two downloads start before the deadline; the third would start after it.
  expect_length(f$urls, 2L)
  expect_equal(res[c("hashed", "to_hash", "stop")], list(hashed = 2L, to_hash = 1L, stop = "time"))
  t <- .table(db)
  expect_equal(sum(!is.na(t$sha256)), 2L)
  expect_equal(sort(t$hashed_at[!is.na(t$hashed_at)]),
               c("2026-10-23T05:01:40Z", "2026-10-23T05:03:20Z"))
})

test_that("no retry starts after the deadline", {
  files <- .three_files()[1]
  db <- .hash_db(.three()[2, ])
  clock <- .fake_clock()
  f  <- .copy_fetch(.local_mirror(files), clock = clock, tick = 100)
  p  <- .no_pause()
  res <- suppressMessages(.hash(db, f$fetch, clock = clock, deadline = clock$now + 50,
                                pause = p$pause, attempts = 3L))
  expect_length(f$urls, 1L)
  expect_length(p$waits, 0L)
  expect_equal(res$failures, 1L)
})

test_that("a run that starts past its deadline fetches nothing", {
  db <- .hash_db(.three())
  clock <- .fake_clock()
  f  <- .copy_fetch(.local_mirror(.three_files()))
  res <- suppressMessages(.hash(db, f$fetch, clock = clock, deadline = clock$now - 1))
  expect_length(f$urls, 0L)
  expect_equal(res[c("hashed", "to_hash", "stop")], list(hashed = 0L, to_hash = 3L, stop = "time"))
})

test_that("a database error stops hashing and keeps the batches already stored", {
  db <- .hash_db(.three())
  f  <- .copy_fetch(.local_mirror(.three_files()))
  calls <- new.env()
  calls$n <- 0L
  breaking <- function(urls, destfiles) {
    calls$n <- calls$n + 1L
    if (calls$n == 2L) {
      con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
      RSQLite::dbExecute(con, "DROP TABLE cran_tarballs")
      RSQLite::dbDisconnect(con)
    }
    f$fetch(urls, destfiles)
  }
  expect_message(res <- .hash(db, breaking, batch = 1L), "hashing stopped")
  expect_equal(res$stop, "error")
  expect_equal(res$hashed, 1L)
})

test_that("the deadline is the time budget, or sooner when the job started long ago", {
  start <- as.POSIXct("2026-10-23 05:10:00", tz = "UTC")
  expect_equal(tarball_hash_deadline(start, "", max_secs = 900, job_secs = 1200), start + 900)
  # The job started 1000 s before hashing began: 200 s are left of its 1200.
  expect_equal(tarball_hash_deadline(start, as.character(as.numeric(start) - 1000),
                                     max_secs = 900, job_secs = 1200), start + 200)
  expect_equal(tarball_hash_deadline(start, as.character(as.numeric(start) - 60),
                                     max_secs = 900, job_secs = 1200), start + 900)
  expect_equal(tarball_hash_deadline(start, "not a time", max_secs = 900, job_secs = 1200),
               start + 900)
})

# --- the real download, against local files ------------------------------------

test_that("download_tarballs fetches several files at once and reports each one", {
  root <- .local_mirror(list("src/contrib/a_1.0.tar.gz" = ABC$text,
                             "src/contrib/b_1.0.tar.gz" = FOX$text))
  out  <- withr::local_tempdir()
  urls <- paste0("file://", root, "/src/contrib/", c("a_1.0.tar.gz", "missing_1.0.tar.gz", "b_1.0.tar.gz"))
  dest <- file.path(out, c("1", "2", "3"))
  ua <- getOption("HTTPUserAgent")
  got <- download_tarballs(urls, dest)
  expect_equal(got, c(TRUE, FALSE, TRUE))
  expect_equal(unname(tools::md5sum(dest[c(1, 3)])), c(ABC$md5, FOX$md5))
  expect_identical(getOption("HTTPUserAgent"), ua)
  # A single file that fails is FALSE, not an error.
  expect_false(download_tarballs(urls[2], dest[2]))
})
