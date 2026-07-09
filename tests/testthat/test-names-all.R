# tests/testthat/test-names-all.R: unit tests for build_names_all.

test_that("build_names_all unions archive and live names with a live/archived state", {
  archive_list <- list(maptools = data.frame(), MASS = data.frame(), abc = data.frame())
  current_pkgs <- c("MASS", "ggplot2")
  df <- build_names_all(archive_list, current_pkgs)

  expect_setequal(df$name_lower, c("maptools", "mass", "abc", "ggplot2"))
  # live iff in current_pkgs
  expect_equal(df$identity_state[df$name_lower == "mass"], "live")
  expect_equal(df$identity_state[df$name_lower == "ggplot2"], "live")
  expect_equal(df$identity_state[df$name_lower == "maptools"], "archived")
  # canonical case is preserved verbatim from CRAN
  expect_equal(df$canonical_name[df$name_lower == "mass"], "MASS")
  expect_equal(df$canonical_name[df$name_lower == "ggplot2"], "ggplot2")
  # empty inputs yield a typed 0-row frame
  expect_equal(nrow(build_names_all(list(), character(0))), 0L)
})

test_that("build_names_all keeps the live entry on a case collision", {
  # "Foo" appears archived-cased in the archive index; "foo" is live under a
  # different case. They collide on name_lower and the live entry must win.
  df <- build_names_all(list(Foo = data.frame()), c("foo"))
  hit <- df[df$name_lower == "foo", ]
  expect_equal(nrow(hit), 1L)
  expect_equal(hit$canonical_name, "foo")
  expect_equal(hit$identity_state, "live")
})
