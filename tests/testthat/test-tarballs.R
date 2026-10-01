# cran_tarballs: one row per CRAN source tarball file, identified by its exact
# size and mtime within its (package, version), with the MD5 PACKAGES gives
# while the file is current. Fixture rows are real, from CRAN's Meta indexes
# and PACKAGES.rds of 2026-09-30.

.finfo <- function(paths, sizes, mtimes) {
  data.frame(size = as.numeric(sizes), mtime = as.POSIXct(mtimes, tz = "UTC"),
             row.names = paths)
}

.archive_fixture <- function() list(
  lmeInfo  = .finfo(c("lmeInfo/lmeInfo_0.3.1.tar.gz", "lmeInfo/lmeInfo_0.3.2.tar.gz"),
                    c(65117, 65124),
                    c("2023-02-15 15:00:04.025", "2023-04-17 08:30:07.235")),
  calibFit = .finfo(c("calibFit/Ancestry/calib_2.0.1.tar.gz", "calibFit/calibFit_2.1.0.tar.gz"),
                    c(370298, 438790),
                    c("2009-07-18 15:40:32", "2011-11-03 21:30:47.039")),
  relax    = .finfo(c("relax/Old/relax_1.00.tar.gz", "relax/relax_1.3.15.tar.gz"),
                    c(2026010, 2192562),
                    c("2005-06-24 15:56:12", "2014-03-10 16:23:37.343")))

.current_fixture <- function() .finfo(
  c("cli_3.6.6.tar.gz", "lmeInfo_0.3.2.tar.gz"), c(644134, 65532),
  c("2026-04-09 09:50:29.324", "2026-09-27 15:46:29.823"))

.md5_fixture <- function() data.frame(
  package = c("cli", "lmeInfo"), version = c("3.6.6", "0.3.2"),
  md5sum  = c("eedc08ba864ae6cd640b05eff1a607ee", "2f85a1705379f8a2bb36d119a9ad90d5"),
  stringsAsFactors = FALSE)

.snap <- function() build_tarball_snapshot(.archive_fixture(), .current_fixture(), .md5_fixture())

test_that("the current index is read from the same server as PACKAGES", {
  expect_equal(CRAN_CURRENT_URL, "https://cran.r-project.org/src/contrib/Meta/current.rds")
  expect_equal(TARBALL_REVISION_STORM_MAX, 500L)
})

test_that("only <pkg>/<pkg>_<ver>.tar.gz archive paths count; Ancestry and Old copies do not", {
  s <- .snap()
  expect_equal(nrow(s), 6L)
  expect_equal(sort(unique(s$package)), c("calibFit", "cli", "lmeInfo", "relax"))
  expect_equal(s$version[s$package == "relax"], "1.3.15")
  expect_equal(s$version[s$package == "calibFit"], "2.1.0")
})

test_that("sizes are whole bytes and mtimes are whole UTC seconds", {
  s <- .snap()
  expect_type(s$size_bytes, "integer")
  a <- s[s$package == "lmeInfo" & s$version == "0.3.2" & s$listing == "archive", ]
  expect_equal(a$size_bytes, 65124L)
  expect_equal(a$mtime, "2023-04-17T08:30:07Z")
  cur <- s[s$package == "lmeInfo" & s$listing == "current", ]
  expect_equal(cur$mtime, "2026-09-27T15:46:29Z")
})

test_that("a same-version re-upload gives two files: the archived one and the current one", {
  s <- .snap()
  pair <- s[s$package == "lmeInfo" & s$version == "0.3.2", ]
  expect_equal(pair$listing, c("archive", "current"))
  expect_equal(pair$size_bytes, c(65124L, 65532L))
})

test_that("the MD5 attaches to current files only", {
  s <- .snap()
  expect_equal(s$md5sum[s$package == "cli"], "eedc08ba864ae6cd640b05eff1a607ee")
  expect_equal(s$md5sum[s$package == "lmeInfo" & s$listing == "current"],
               "2f85a1705379f8a2bb36d119a9ad90d5")
  expect_true(all(is.na(s$md5sum[s$listing == "archive"])))
})

test_that("a missing PACKAGES read leaves every MD5 NULL instead of failing", {
  s <- build_tarball_snapshot(.archive_fixture(), .current_fixture(), NULL)
  expect_equal(nrow(s), 6L)
  expect_true(all(is.na(s$md5sum)))
})

test_that("a file in both indexes at once is one row, listed as current", {
  cur <- .finfo(c("cli_3.6.6.tar.gz", "lmeInfo_0.3.2.tar.gz"), c(644134, 65124),
                c("2026-04-09 09:50:29.324", "2023-04-17 08:30:07.235"))
  s <- build_tarball_snapshot(.archive_fixture(), cur, .md5_fixture())
  one <- s[s$package == "lmeInfo" & s$version == "0.3.2", ]
  expect_equal(nrow(one), 1L)
  expect_equal(one$listing, "current")
})

test_that("a size R prints as 6e+05 is stored as the integer 600000", {
  # datasauRus 0.1.2 is exactly 600000 bytes; as.character() gives "6e+05".
  arch <- list(datasauRus = .finfo("datasauRus/datasauRus_0.1.2.tar.gz", 600000,
                                   "2017-05-08 22:35:14"))
  s <- build_tarball_snapshot(arch, .finfo(character(0), numeric(0), character(0)), NULL)
  expect_identical(s$size_bytes, 600000L)
  expect_equal(s$mtime, "2017-05-08T22:35:14Z")
})

test_that("a snapshot is healthy only with enough current files and archived packages", {
  s <- .snap()
  expect_true(tarball_snapshot_healthy(s, min_current = 2L, min_archive = 3L))
  expect_false(tarball_snapshot_healthy(s, min_current = 3L, min_archive = 3L))
  expect_false(tarball_snapshot_healthy(s, min_current = 2L, min_archive = 4L))
  expect_false(tarball_snapshot_healthy(NULL, min_current = 0L, min_archive = 0L))
})

# --- revisions across runs ---------------------------------------------------

.one <- function(package, version, size, mtime, listing, md5 = NA_character_) {
  data.frame(package = package, version = version, size_bytes = as.integer(size),
             mtime = mtime, md5sum = md5, listing = listing, stringsAsFactors = FALSE)
}

test_that("a first run numbers each version's files by mtime", {
  got <- merge_tarballs(empty_tarballs(), .snap(), "2026-10-01", storm_max = 500L)
  expect_equal(got$state, "cold_start")
  expect_equal(got$new_revisions, 6L)
  pair <- got$table[got$table$package == "lmeInfo" & got$table$version == "0.3.2", ]
  expect_equal(pair$revision, c(1L, 2L))
  expect_equal(pair$mtime, c("2023-04-17T08:30:07Z", "2026-09-27T15:46:29Z"))
  expect_equal(pair$listing, c("archive", "current"))
  expect_true(all(got$table$first_seen == "2026-10-01" & got$table$last_seen == "2026-10-01"))
})

test_that("a quiet day only moves last_seen", {
  day1 <- merge_tarballs(empty_tarballs(), .snap(), "2026-10-01", storm_max = 500L)$table
  got  <- merge_tarballs(day1, .snap(), "2026-10-02", storm_max = 500L)
  expect_equal(got$state, "updated")
  expect_equal(got$new_revisions, 0L)
  expect_equal(got$table[, c("package", "version", "revision", "first_seen")],
               day1[, c("package", "version", "revision", "first_seen")])
  expect_true(all(got$table$last_seen == "2026-10-02"))
})

test_that("a same-version re-upload opens revision 2 with its own MD5", {
  day1 <- merge_tarballs(empty_tarballs(),
    .one("lmeInfo", "0.3.2", 65124, "2023-04-17T08:30:07Z", "current", "8d3c5ea1c0d0c1f1e2a3b4c5d6e7f809"),
    "2026-09-20", storm_max = 500L)$table
  snap2 <- rbind(
    .one("lmeInfo", "0.3.2", 65124, "2023-04-17T08:30:07Z", "archive"),
    .one("lmeInfo", "0.3.2", 65532, "2026-09-27T15:46:29Z", "current", "2f85a1705379f8a2bb36d119a9ad90d5"))
  got <- merge_tarballs(day1, snap2, "2026-09-28", storm_max = 500L)
  t <- got$table[order(got$table$revision), ]
  expect_equal(t$revision, c(1L, 2L))
  expect_equal(t$size_bytes, c(65124L, 65532L))
  expect_equal(t$listing, c("archive", "current"))
  expect_equal(t$md5sum, c("8d3c5ea1c0d0c1f1e2a3b4c5d6e7f809", "2f85a1705379f8a2bb36d119a9ad90d5"))
  expect_equal(t$first_seen, c("2026-09-20", "2026-09-28"))
  expect_equal(got$new_revisions, 1L)
})

test_that("moving from current to archive keeps the revision and its MD5", {
  day1 <- merge_tarballs(empty_tarballs(),
    .one("cli", "3.6.6", 644134, "2026-04-09T09:50:29Z", "current", "eedc08ba864ae6cd640b05eff1a607ee"),
    "2026-10-01", storm_max = 500L)$table
  got <- merge_tarballs(day1, .one("cli", "3.6.6", 644134, "2026-04-09T09:50:29Z", "archive"),
                        "2026-10-02", storm_max = 500L)$table
  expect_equal(nrow(got), 1L)
  expect_equal(got$revision, 1L)
  expect_equal(got$listing, "archive")
  expect_equal(got$md5sum, "eedc08ba864ae6cd640b05eff1a607ee")
})

test_that("an MD5 missing on the first sighting is filled later", {
  day1 <- merge_tarballs(empty_tarballs(),
    .one("cli", "3.6.6", 644134, "2026-04-09T09:50:29Z", "current"), "2026-10-01", storm_max = 500L)$table
  got <- merge_tarballs(day1,
    .one("cli", "3.6.6", 644134, "2026-04-09T09:50:29Z", "current", "eedc08ba864ae6cd640b05eff1a607ee"),
    "2026-10-02", storm_max = 500L)
  expect_equal(got$table$md5sum, "eedc08ba864ae6cd640b05eff1a607ee")
  expect_equal(got$new_revisions, 0L)
})

test_that("an MD5 that disagrees for the same file is overwritten, not a new revision", {
  # PACKAGES.rds is regenerated about 47 seconds after current.rds, so one read
  # can pair a new file with the old checksum; the next run corrects it.
  day1 <- merge_tarballs(empty_tarballs(),
    .one("lmeInfo", "0.3.2", 65532, "2026-09-27T15:46:29Z", "current", "8d3c5ea1c0d0c1f1e2a3b4c5d6e7f809"),
    "2026-09-27", storm_max = 500L)$table
  got <- merge_tarballs(day1,
    .one("lmeInfo", "0.3.2", 65532, "2026-09-27T15:46:29Z", "current", "2f85a1705379f8a2bb36d119a9ad90d5"),
    "2026-09-28", storm_max = 500L)$table
  expect_equal(nrow(got), 1L)
  expect_equal(got$revision, 1L)
  expect_equal(got$md5sum, "2f85a1705379f8a2bb36d119a9ad90d5")
})

test_that("a file no longer listed becomes gone with last_seen frozen, and returns if relisted", {
  day1 <- merge_tarballs(empty_tarballs(), .snap(), "2026-10-01", storm_max = 500L)$table
  snap2 <- .snap()[.snap()$package != "relax", ]
  day2 <- merge_tarballs(day1, snap2, "2026-10-02", storm_max = 500L)$table
  r <- day2[day2$package == "relax", ]
  expect_equal(r$listing, "gone")
  expect_equal(r$last_seen, "2026-10-01")
  day3 <- merge_tarballs(day2, .snap(), "2026-10-03", storm_max = 500L)$table
  r3 <- day3[day3$package == "relax", ]
  expect_equal(r3$listing, "archive")
  expect_equal(r3$revision, 1L)
  expect_equal(r3$last_seen, "2026-10-03")
  expect_equal(nrow(day3), nrow(day1))
})

test_that("more new files for known versions than storm_max keeps the prior table", {
  day1 <- merge_tarballs(empty_tarballs(), .snap(), "2026-10-01", storm_max = 500L)$table
  touched <- .snap()
  touched$mtime <- "2026-10-02T03:00:00Z"   # a bulk touch of every file
  got <- merge_tarballs(day1, touched, "2026-10-02", storm_max = 5L)
  expect_equal(got$state, "storm")
  expect_equal(got$new_revisions, 0L)
  expect_identical(got$table, day1)
})

test_that("brand-new versions never count toward a storm", {
  day1 <- merge_tarballs(empty_tarballs(), .snap(), "2026-10-01", storm_max = 500L)$table
  fresh <- rbind(.snap(),
    .one("alpha", "1.0", 1000, "2026-10-02T01:00:00Z", "current"),
    .one("beta",  "1.0", 2000, "2026-10-02T01:00:00Z", "current"),
    .one("gamma", "1.0", 3000, "2026-10-02T01:00:00Z", "current"))
  got <- merge_tarballs(day1, fresh, "2026-10-02", storm_max = 2L)
  expect_equal(got$state, "updated")
  expect_equal(got$new_revisions, 3L)
})

# --- the same file twice in one snapshot ---------------------------------------

test_that("a file listed twice in one snapshot is one revision, with the MD5 either row gives", {
  twice <- rbind(
    .one("cli", "3.6.6", 644134, "2026-04-09T09:50:29Z", "current"),
    .one("cli", "3.6.6", 644134, "2026-04-09T09:50:29Z", "current", "eedc08ba864ae6cd640b05eff1a607ee"))
  got <- merge_tarballs(empty_tarballs(), twice, "2026-10-01", storm_max = 500L)
  expect_equal(nrow(got$table), 1L)
  expect_equal(got$table$revision, 1L)
  expect_equal(got$table$md5sum, "eedc08ba864ae6cd640b05eff1a607ee")
  expect_equal(got$new_revisions, 1L)
})

test_that("a re-upload listed twice opens revision 2 only and counts once toward a storm", {
  day1 <- merge_tarballs(empty_tarballs(),
    .one("lmeInfo", "0.3.2", 65124, "2023-04-17T08:30:07Z", "current"),
    "2026-09-20", storm_max = 500L)$table
  new <- .one("lmeInfo", "0.3.2", 65532, "2026-09-27T15:46:29Z", "current",
              "2f85a1705379f8a2bb36d119a9ad90d5")
  snap2 <- rbind(.one("lmeInfo", "0.3.2", 65124, "2023-04-17T08:30:07Z", "archive"), new, new)
  got <- merge_tarballs(day1, snap2, "2026-09-28", storm_max = 1L)
  expect_equal(got$state, "updated")
  expect_equal(got$new_revisions, 1L)
  expect_equal(got$table$revision, c(1L, 2L))
  expect_equal(got$table$size_bytes, c(65124L, 65532L))
})

test_that("a file given as both archive and current in one snapshot is kept as current", {
  both <- rbind(
    .one("cli", "3.6.6", 644134, "2026-04-09T09:50:29Z", "archive"),
    .one("cli", "3.6.6", 644134, "2026-04-09T09:50:29Z", "current", "eedc08ba864ae6cd640b05eff1a607ee"))
  day1 <- merge_tarballs(empty_tarballs(), both, "2026-10-01", storm_max = 500L)$table
  expect_equal(nrow(day1), 1L)
  expect_equal(day1$listing, "current")
  expect_equal(day1$md5sum, "eedc08ba864ae6cd640b05eff1a607ee")
  # The same holds for a file the table already has.
  day2 <- merge_tarballs(day1, both, "2026-10-02", storm_max = 500L)
  expect_equal(day2$new_revisions, 0L)
  expect_equal(day2$table$listing, "current")
  expect_equal(day2$table$last_seen, "2026-10-02")
})

# --- the table on disk ---------------------------------------------------------

test_that("export_tarballs writes the keyed, WITHOUT ROWID table with its check", {
  db <- withr::local_tempfile(fileext = ".db")
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  t1 <- merge_tarballs(empty_tarballs(), .snap(), "2026-10-01", storm_max = 500L)$table
  export_tarballs(con, t1)
  export_tarballs(con, t1)   # replaces, never appends
  expect_equal(RSQLite::dbGetQuery(con, "SELECT COUNT(*) AS n FROM cran_tarballs")$n, 6L)
  sql <- RSQLite::dbGetQuery(con, "SELECT sql FROM sqlite_master WHERE name = 'cran_tarballs'")$sql
  expect_match(sql, "WITHOUT ROWID", fixed = TRUE)
  expect_match(sql, "PRIMARY KEY (package, version, revision)", fixed = TRUE)
  expect_error(RSQLite::dbExecute(con, "INSERT INTO cran_tarballs VALUES
    ('cli', '9.9', 1, 10, '2026-10-01T00:00:00Z', NULL, 'current', '2026-10-02', '2026-10-01')"),
    "CHECK constraint failed")
})

test_that("read_prev_tarballs gives zero rows for a database that never had the table", {
  db <- withr::local_tempfile(fileext = ".db")
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  RSQLite::dbExecute(con, "CREATE TABLE cran_names_all (name_lower TEXT PRIMARY KEY)")
  RSQLite::dbDisconnect(con)
  got <- read_prev_tarballs(db)
  expect_equal(nrow(got), 0L)
  expect_equal(names(got), TARBALL_COLS)
})

test_that("read_prev_tarballs throws on a file that is not a database", {
  db <- withr::local_tempfile(fileext = ".db")
  writeBin(charToRaw(paste(rep("not a database", 200), collapse = " ")), db)
  expect_error(suppressWarnings(read_prev_tarballs(db)), "not a database")
})

test_that("a 600000-byte file survives the round trip through the database as one revision", {
  db <- withr::local_tempfile(fileext = ".db")
  arch <- list(datasauRus = .finfo("datasauRus/datasauRus_0.1.2.tar.gz", 600000,
                                   "2017-05-08 22:35:14"))
  snap <- build_tarball_snapshot(arch, .finfo(character(0), numeric(0), character(0)), NULL)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
  export_tarballs(con, merge_tarballs(empty_tarballs(), snap, "2026-10-01", storm_max = 500L)$table)
  RSQLite::dbDisconnect(con)
  got <- merge_tarballs(read_prev_tarballs(db), snap, "2026-10-02", storm_max = 500L)
  expect_equal(got$new_revisions, 0L)
  expect_equal(nrow(got$table), 1L)
  expect_equal(got$table$last_seen, "2026-10-02")
})
