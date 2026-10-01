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
