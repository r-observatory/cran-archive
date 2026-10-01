# The live CRAN list comes from PACKAGES with only the duplicates filter, so a
# package declaring OS_type: windows, or needing a newer R than this runner,
# is live, and a Recommended package listed twice counts once.

.fixture_repo <- function() {
  paste0("file://", normalizePath(testthat::test_path("fixtures", "cran-repo")))
}

test_that("PACKAGES is read from cran.r-project.org, beside the Meta indexes", {
  expect_equal(CRAN_PACKAGES_URL, "https://cran.r-project.org")
  expect_true(startsWith(CRAN_ARCHIVE_URL, paste0(CRAN_PACKAGES_URL, "/")))
})

test_that("a Windows-only package and one needing a newer R are both listed", {
  ap <- cran_available(.fixture_repo())
  expect_setequal(rownames(ap), c("boot", "cli", "hespdiv", "laterR"))
  expect_equal(unname(ap["hespdiv", "OS_type"]), "windows")
})

test_that("a Recommended package listed twice counts once, from the main tree", {
  ap <- cran_available(.fixture_repo())
  expect_equal(sum(rownames(ap) == "boot"), 1L)
  expect_equal(unname(ap["boot", "Repository"]),
               contrib.url(.fixture_repo(), type = "source"))
})

test_that("the default filters drop both packages on this runner", {
  skip_on_os("windows")
  ap <- utils::available.packages(repos = .fixture_repo())
  expect_setequal(rownames(ap), c("boot", "cli"))
})

test_that("a Windows-only package with archived versions is live, not archived", {
  current <- rownames(cran_available(.fixture_repo()))
  archive_list <- list(
    hespdiv = data.frame(mtime = as.POSIXct("2025-01-10", tz = "UTC"),
                         row.names = "hespdiv/hespdiv_1.2.9.tar.gz"),
    oldpkg  = data.frame(mtime = as.POSIXct("2019-05-01", tz = "UTC"),
                         row.names = "oldpkg/oldpkg_0.1.tar.gz"))
  expect_equal(build_archive(archive_list, current)$package, "oldpkg")
  nm <- build_names_all(archive_list, current)
  expect_equal(nm$identity_state[nm$name_lower == "hespdiv"], "live")
  expect_equal(nm$identity_state[nm$name_lower == "oldpkg"], "archived")
})

test_that("packages_md5_from keeps main-tree rows and blanks an empty MD5sum", {
  contrib <- "https://cran.r-project.org/src/contrib"
  ap <- cbind(
    Package    = c("cli", "Matrix", "tiny"),
    Version    = c("3.6.6", "1.7-5", "0.1"),
    MD5sum     = c("eedc08ba864ae6cd640b05eff1a607ee", "0123456789abcdef0123456789abcdef", ""),
    Repository = c(contrib, paste0(contrib, "/4.7.0/Recommended"), paste0(contrib, "/")))
  got <- packages_md5_from(ap, contrib)
  expect_equal(got$package, c("cli", "tiny"))
  expect_equal(got$version, c("3.6.6", "0.1"))
  expect_equal(got$md5sum, c("eedc08ba864ae6cd640b05eff1a607ee", NA))
})
