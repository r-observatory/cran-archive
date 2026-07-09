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

test_that("merge_names_all is append-only and freezes first_seen and casing", {
  prior <- data.frame(
    name_lower = c("mass", "oldpkg"), canonical_name = c("MASS", "OldPkg"),
    identity_state = c("live", "archived"),
    first_seen = c("2020-01-01", "2019-05-05"),
    last_seen  = c("2026-01-01", "2026-01-01"), stringsAsFactors = FALSE)
  current <- data.frame(
    name_lower = c("mass", "newpkg"), canonical_name = c("mass", "NewPkg"),
    identity_state = c("archived", "live"), stringsAsFactors = FALSE)

  out <- merge_names_all(prior, current, now = "2026-07-09")

  expect_setequal(out$name_lower, c("mass", "oldpkg", "newpkg"))
  m <- out[out$name_lower == "mass", ]
  expect_equal(m$first_seen, "2020-01-01")    # frozen from prior
  expect_equal(m$canonical_name, "MASS")      # canonical frozen from prior, not taken from current
  expect_equal(m$identity_state, "archived")  # refreshed from this run
  expect_equal(m$last_seen, "2026-07-09")     # touched
  # a name that vanished upstream this run is retained, not dropped
  old <- out[out$name_lower == "oldpkg", ]
  expect_equal(old$canonical_name, "OldPkg")
  expect_equal(old$identity_state, "archived")
  expect_equal(old$last_seen, "2026-07-09")
  # a brand-new name gets first_seen = now
  new <- out[out$name_lower == "newpkg", ]
  expect_equal(new$first_seen, "2026-07-09")
  expect_equal(new$canonical_name, "NewPkg")

  # cold start: NULL prior yields all-new rows
  cold <- merge_names_all(NULL, current, now = "2026-07-09")
  expect_setequal(cold$name_lower, c("mass", "newpkg"))
  expect_true(all(cold$first_seen == "2026-07-09"))

  # explicit 0-row prior (distinct from NULL) also cold-starts cleanly
  zero_prior <- data.frame(name_lower = character(0), canonical_name = character(0),
                           identity_state = character(0), first_seen = character(0),
                           last_seen = character(0), stringsAsFactors = FALSE)
  z <- merge_names_all(zero_prior, current, now = "2026-07-09")
  expect_setequal(z$name_lower, c("mass", "newpkg"))
  expect_true(all(z$first_seen == "2026-07-09"))
})
