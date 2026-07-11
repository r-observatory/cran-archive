#!/usr/bin/env Rscript
# scripts/update.R: CRAN archived-packages catalog builder.
#
# Fetches the CRAN archive.rds and the current-packages list, identifies
# packages that are currently archived (in archive.rds but absent from the live
# CRAN listing), and writes a SQLite catalog plus a JSON manifest to out_dir.
#
# run_update(io, out_dir, force_full) takes an injectable io for offline testing.
# default_io() (in helpers.R) supplies the real network fetchers.
#
# Usage:
#   Rscript scripts/update.R [out_dir] [--bootstrap]
#
# --bootstrap sets force_full = TRUE (regenerate even if fingerprint unchanged).

options(timeout = 600)

suppressPackageStartupMessages({
  library(RSQLite)
  library(jsonlite)
  library(digest)
})

.this_file <- function() {
  for (i in rev(seq_len(sys.nframe()))) {
    of <- sys.frame(i)$ofile
    if (!is.null(of) && nzchar(of)) return(normalizePath(of))
  }
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
  if (length(f) == 1L && nzchar(f)) return(normalizePath(f))
  NA_character_
}
.script_dir <- { tf <- .this_file(); if (!is.na(tf)) dirname(tf) else "scripts" }
if (!exists("build_archive", mode = "function")) {
  source(file.path(.script_dir, "config.R"))
  source(file.path(.script_dir, "helpers.R"))
}

iso <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# ---------------------------------------------------------------------------
# run_update
# ---------------------------------------------------------------------------

run_update <- function(io, out_dir, force_full = FALSE,
                       live_floor = CRAN_LIVE_FLOOR, archive_floor = CRAN_ARCHIVE_FLOOR) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # 1. Fetch data sources via injectable io
  archive_list <- io$archive_rds()
  current_pkgs <- io$current_packages()
  reasons      <- io$removal_reasons()
  history_map  <- io$removal_history()

  # 2. Build the archived-package table, event log, and durable episode history
  archive_df <- build_archive(archive_list, current_pkgs, reasons)
  events_df  <- build_archive_events(archive_list, current_pkgs)
  history_df <- build_archive_history(archive_df, history_map)

  # 3. Compute a stable fingerprint over the archived set: SHA-256 hash of the
  #    sorted "package:archived_on" pairs joined by commas. If the archived set
  #    or any package's last-archived date changes, the fingerprint changes.
  #    Hashing keeps the stored value a fixed 64-char hex string regardless of
  #    how many packages are in the archive.
  raw_pairs <- paste(sort(paste0(archive_df$package, ":", archive_df$archived_on)), collapse = ",")
  archive_fingerprint <- digest::digest(raw_pairs, algo = "sha256", serialize = FALSE)

  # 4. Load the prior manifest from out_dir for change detection (cold start
  #    is handled gracefully: missing manifest => prev_fingerprint is "").
  manifest_path    <- file.path(out_dir, "manifest.json")
  prev_manifest    <- tryCatch({
    if (file.exists(manifest_path)) jsonlite::read_json(manifest_path) else list()
  }, error = function(e) list())
  prev_fingerprint <- prev_manifest$source$archive_fingerprint %||% ""

  changed <- isTRUE(force_full) || (archive_fingerprint != prev_fingerprint)

  # 5. Export database (always written, even when changed=FALSE, to ensure the
  #    DB is present and consistent with the current data).
  db_path <- file.path(out_dir, DB_FILENAME)
  export_archive(db_path, archive_df, events_df, history_df)

  # Append-only cran_names_all: union archive + live names, gated against a
  # partial fetch, folded into the prior published table.
  n_live        <- length(current_pkgs)
  n_arch        <- length(archive_list)
  names_gate_ok <- names_size_ok(n_live, n_arch, live_floor, archive_floor)
  prior         <- tryCatch(io$prev_names(), error = function(e) NULL)
  names_healthy <- !is.null(prior)   # NULL means prev_names threw: prior is UNREACHABLE
  n_names <- NA_integer_
  now_stamp <- format(Sys.time(), "%Y-%m-%d", tz = "UTC")
  if (!names_healthy) {
    # A transient prior-fetch failure must never cold-start and reset first_seen.
    message("prior cran_names_all unreachable; skipping the names write this run")
  } else if (names_gate_ok) {
    merged <- merge_names_all(prior, build_names_all(archive_list, current_pkgs), now_stamp)
    con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(RSQLite::dbDisconnect(con), add = TRUE)
    export_names_all(con, merged)
    n_names <- nrow(merged)
  } else if (nrow(prior) > 0L) {
    message("names size gate failed (live=", n_live, ", archive=", n_arch,
            "); reusing the prior cran_names_all")
    con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(RSQLite::dbDisconnect(con), add = TRUE)
    export_names_all(con, prior)
    n_names <- nrow(prior)
  }

  # 6. Write manifest
  manifest <- list(
    release             = paste0("v", format(Sys.time(), "%Y%m%d-%H%M%S", tz = "UTC")),
    generated_at        = iso(Sys.time()),
    n_archived          = nrow(archive_df),
    n_events            = nrow(events_df),
    changed             = changed,
    n_names             = n_names,
    names_gate_ok       = names_gate_ok,
    names_healthy       = names_healthy,
    source              = list(
      archive_fingerprint = archive_fingerprint
    )
  )
  write_manifest(manifest_path, manifest)

  list(changed = changed, manifest = manifest)
}

# ---------------------------------------------------------------------------
# Entry point when run as a standalone script
# ---------------------------------------------------------------------------

if (sys.nframe() == 0L) {
  args       <- commandArgs(trailingOnly = TRUE)
  out_dir    <- if (length(args) >= 1L && !startsWith(args[1L], "--")) args[1L] else "out"
  force_full <- "--bootstrap" %in% args
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  run_update(default_io(), out_dir, force_full)
}
