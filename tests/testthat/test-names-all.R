# tests/testthat/test-names-all.R: unit tests for build_names_all.

# Source update.R if it has not been loaded already, so run_update is available
# regardless of test file execution order (mirrors test-run-update.R).
if (!exists("run_update", mode = "function")) {
  .candidates <- c(
    file.path(getwd(), "scripts", "update.R"),
    file.path(getwd(), "..", "..", "scripts", "update.R")
  )
  .upd <- .candidates[file.exists(.candidates)]
  if (length(.upd)) source(normalizePath(.upd[1L]))
}

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

test_that("names_size_ok rejects a partial or empty fetch", {
  expect_true(names_size_ok(20000, 28000))
  expect_false(names_size_ok(500, 28000))      # live CRAN came back short
  expect_false(names_size_ok(20000, 3))        # archive came back short
  expect_false(names_size_ok(0, 0))
})

test_that("export_names_all writes the cran_names_all table", {
  path <- tempfile(fileext = ".db")
  on.exit(unlink(path))
  con <- RSQLite::dbConnect(RSQLite::SQLite(), path)
  df <- data.frame(name_lower = c("mass", "oldpkg"), canonical_name = c("MASS", "OldPkg"),
                   identity_state = c("live", "archived"),
                   first_seen = "2026-07-09", last_seen = "2026-07-09", stringsAsFactors = FALSE)
  export_names_all(con, df)
  got <- RSQLite::dbGetQuery(con, "SELECT * FROM cran_names_all ORDER BY name_lower")
  RSQLite::dbDisconnect(con)
  expect_equal(nrow(got), 2L)
  expect_equal(got$canonical_name[got$name_lower == "mass"], "MASS")
  expect_equal(got$identity_state[got$name_lower == "oldpkg"], "archived")
})

test_that("run_update writes an append-only cran_names_all through injected io", {
  out_dir <- withr::local_tempdir()
  prior <- data.frame(
    name_lower = "oldpkg", canonical_name = "OldPkg", identity_state = "archived",
    first_seen = "2019-01-01", last_seen = "2025-01-01", stringsAsFactors = FALSE)
  io <- list(
    archive_rds      = function() list(maptools = data.frame(mtime = as.POSIXct("2020-01-01"),
                                                             row.names = "maptools/maptools_1.0.tar.gz"),
                                       MASS = data.frame(mtime = as.POSIXct("2021-01-01"),
                                                         row.names = "MASS/MASS_7.3.tar.gz")),
    current_packages = function() rep("MASS", 1),        # MASS live, maptools archived
    removal_reasons  = function() character(0),
    prev_names       = function() prior)
  # Bypass the size gate for this small fixture by lowering the floors.
  res <- run_update(io, out_dir, force_full = TRUE, live_floor = 0L, archive_floor = 0L)

  con <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(out_dir, "cran-archive.db"))
  got <- RSQLite::dbGetQuery(con, "SELECT * FROM cran_names_all ORDER BY name_lower")
  RSQLite::dbDisconnect(con)
  expect_setequal(got$name_lower, c("oldpkg", "mass", "maptools"))  # prior retained + new
  expect_equal(got$identity_state[got$name_lower == "mass"], "live")
  expect_equal(got$identity_state[got$name_lower == "maptools"], "archived")
  expect_equal(got$first_seen[got$name_lower == "oldpkg"], "2019-01-01")  # frozen
  expect_equal(res$manifest$n_names, 3L)
  expect_true(res$manifest$names_gate_ok)
})
