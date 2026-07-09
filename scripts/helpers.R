# scripts/helpers.R: pure helper functions for the cran-archive pipeline.

#' Null/NA/empty coalescing operator.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

#' Parse the version string from a tarball path of the form
#' "<package>/<package>_<version>.tar.gz".
#' Returns NA_character_ when the pattern does not match.
#'
#' @param path    A tarball path string, e.g. "ACA/ACA_1.1.tar.gz".
#' @param package The package name used as the prefix in the path.
parse_archive_version <- function(path, package) {
  pat <- paste0("^", package, "/", package, "_(.+)\\.tar\\.gz$")
  m   <- regmatches(path, regexec(pat, path))[[1L]]
  if (length(m) < 2L) return(NA_character_)
  m[2L]
}

#' Build a data.frame of currently-archived CRAN packages.
#'
#' A package is considered currently archived if it appears in archive_list but
#' is NOT present in current_pkgs. Packages that still have active CRAN
#' listings are excluded even though their old versions appear in archive.rds.
#'
#' @param archive_list  Named list of data.frames as returned by
#'   readRDS(url("https://cran.r-project.org/src/contrib/Meta/archive.rds")).
#'   Each element is a data.frame whose row names are tarball paths and which
#'   has an mtime column (POSIXct).
#' @param current_pkgs  Character vector of currently-available package names.
#' @param reasons       Named character vector mapping package name to a
#'   removal_reason string. Packages absent from the map receive NA.
#' @return data.frame(package, first_release, archived_on, last_version,
#'   removal_reason) sorted by package name, one row per archived package.
build_archive <- function(archive_list, current_pkgs, reasons = character(0)) {
  archived_pkgs <- setdiff(names(archive_list), current_pkgs)
  if (length(archived_pkgs) == 0L) {
    return(data.frame(
      package        = character(0),
      first_release  = character(0),
      archived_on    = character(0),
      last_version   = character(0),
      removal_reason = character(0),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(archived_pkgs, function(pkg) {
    df      <- archive_list[[pkg]]
    if (nrow(df) == 0L) return(NULL)
    mt      <- df$mtime
    min_idx <- which.min(mt)
    max_idx <- which.max(mt)
    first_release  <- format(as.Date(mt[min_idx]), "%Y-%m-%d")
    archived_on    <- format(as.Date(mt[max_idx]), "%Y-%m-%d")
    last_path      <- rownames(df)[max_idx]
    last_version   <- parse_archive_version(last_path, pkg)
    removal_reason <- if (pkg %in% names(reasons)) unname(reasons[pkg]) else NA_character_
    data.frame(
      package        = pkg,
      first_release  = first_release,
      archived_on    = archived_on,
      last_version   = last_version,
      removal_reason = removal_reason,
      stringsAsFactors = FALSE
    )
  })

  rows <- Filter(Negate(is.null), rows)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(out$package), , drop = FALSE]
}

#' Build a long-form event table for archived CRAN packages.
#'
#' For each archived package, emits one "release" event per archived tarball
#' version (event_date = that tarball's mtime date, version from filename) plus
#' one "archived" event at the max-mtime date (the last archived version date).
#'
#' NOTE: full gap detection (archived, restored, re-archived cycles) is a future
#' refinement; this implementation treats each package as having one contiguous
#' archive history.
#'
#' @param archive_list  Named list of data.frames (see build_archive).
#' @param current_pkgs  Character vector of currently-available package names.
#' @return data.frame(package, event_date, event_type, version) ordered by
#'   package then event_date.
build_archive_events <- function(archive_list, current_pkgs) {
  archived_pkgs <- setdiff(names(archive_list), current_pkgs)
  if (length(archived_pkgs) == 0L) {
    return(data.frame(
      package    = character(0),
      event_date = character(0),
      event_type = character(0),
      version    = character(0),
      stringsAsFactors = FALSE
    ))
  }

  parts <- lapply(archived_pkgs, function(pkg) {
    df       <- archive_list[[pkg]]
    if (nrow(df) == 0L) return(NULL)
    paths    <- rownames(df)
    mt       <- df$mtime
    versions <- vapply(paths, parse_archive_version, character(1L), package = pkg)
    dates    <- format(as.Date(mt), "%Y-%m-%d")
    # One release event per tarball row
    release_rows <- data.frame(
      package    = pkg,
      event_date = dates,
      event_type = "release",
      version    = versions,
      stringsAsFactors = FALSE
    )
    # One archived event at the max-mtime date
    max_idx <- which.max(mt)
    archived_row <- data.frame(
      package    = pkg,
      event_date = dates[max_idx],
      event_type = "archived",
      version    = versions[max_idx],
      stringsAsFactors = FALSE
    )
    rbind(release_rows, archived_row)
  })

  parts <- Filter(Negate(is.null), parts)
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out[order(out$package, out$event_date), , drop = FALSE]
}

#' Export the assembled archive tables to a fresh SQLite database.
#'
#' Creates (or replaces) the file at path with two tables:
#'   cran_archive        -- one row per archived package (7 columns)
#'   cran_archive_events -- one row per release/archived event (4 columns)
#' and an index on cran_archive_events(package).
#'
#' @param path       File path for the output .db file.
#' @param archive_df data.frame with columns (package, first_release,
#'   archived_on, last_version, removal_reason) as returned by build_archive().
#'   source and updated_at columns are added automatically.
#' @param events_df  data.frame with columns (package, event_date, event_type,
#'   version) as returned by build_archive_events().
export_archive <- function(path, archive_df, events_df) {
  if (file.exists(path)) unlink(path)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), path)
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)

  RSQLite::dbExecute(con, "
    CREATE TABLE cran_archive (
      package        TEXT PRIMARY KEY,
      first_release  TEXT,
      archived_on    TEXT,
      last_version   TEXT,
      removal_reason TEXT,
      source         TEXT,
      updated_at     TEXT
    )
  ")

  RSQLite::dbExecute(con, "
    CREATE TABLE cran_archive_events (
      package    TEXT NOT NULL,
      event_date TEXT,
      event_type TEXT,
      version    TEXT
    )
  ")

  RSQLite::dbExecute(con,
    "CREATE INDEX idx_cran_archive_events_pkg ON cran_archive_events(package)")

  # Augment archive_df with source and updated_at before writing.
  # Use rep_len to safely handle the zero-row case.
  now      <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  write_df <- archive_df
  write_df$source     <- rep_len("cran-archive-rds", nrow(write_df))
  write_df$updated_at <- rep_len(now, nrow(write_df))
  write_df <- write_df[, c("package", "first_release", "archived_on",
                            "last_version", "removal_reason",
                            "source", "updated_at"),
                       drop = FALSE]

  RSQLite::dbWriteTable(con, "cran_archive",        write_df,  append = TRUE)
  RSQLite::dbWriteTable(con, "cran_archive_events", events_df, append = TRUE)

  RSQLite::dbExecute(con, "VACUUM")
  invisible(NULL)
}

#' Write an R list as pretty-printed JSON.
#'
#' @param path File path for the output .json file.
#' @param obj  R list to serialise.
write_manifest <- function(path, obj) {
  jsonlite::write_json(obj, path, auto_unbox = TRUE, pretty = TRUE)
  invisible(NULL)
}

#' Parse CRAN's PACKAGES.in Debian-control file into a removal-reasons map.
#'
#' Reads the text of PACKAGES.in (fetched or provided as a string) and returns
#' a named character vector mapping package name -> X-CRAN-Comment value.  Only
#' packages that have an X-CRAN-Comment field are included; packages with no
#' comment are omitted.  Multi-line field values (continuation lines starting
#' with whitespace) are folded into a single space-separated string.  Robust to
#' CRLF and bare CR line endings.  X-CRAN-History fields are ignored.
#'
#' @param text  Character scalar: the full text of PACKAGES.in.
#' @return Named character vector (package -> X-CRAN-Comment).  Returns
#'   character(0) when no package has a comment.
parse_packages_in <- function(text) {
  # Normalize line endings: CRLF and bare CR -> LF
  text <- gsub("\r\n", "\n", text)
  text <- gsub("\r",   "\n", text)

  # Split into records on one or more consecutive blank lines
  records <- strsplit(text, "\n{2,}")[[1L]]

  result <- list()

  for (rec in records) {
    lines <- strsplit(rec, "\n")[[1L]]
    if (length(lines) == 0L) next

    pkg         <- NA_character_
    comment_val <- NULL
    in_comment  <- FALSE

    for (line in lines) {
      if (grepl("^Package:", line)) {
        pkg        <- trimws(sub("^Package:\\s*", "", line))
        in_comment <- FALSE
      } else if (grepl("^X-CRAN-Comment:", line)) {
        comment_val <- trimws(sub("^X-CRAN-Comment:\\s*", "", line))
        in_comment  <- TRUE
      } else if (in_comment && grepl("^[ \t]", line)) {
        # Continuation line. A line whose only content is "." is Debian control's
        # blank-line-within-a-field marker, so treat it as a paragraph break
        # rather than folding a literal dot into the text.
        cont <- trimws(line)
        if (cont != ".") {
          comment_val <- paste(comment_val, cont)
        }
      } else if (grepl("^[A-Za-z]", line)) {
        # Any new field header ends comment accumulation
        in_comment <- FALSE
      }
    }

    if (!is.na(pkg) && !is.null(comment_val)) {
      final_val <- gsub("[ \t]+", " ", trimws(comment_val))
      if (nzchar(final_val)) {
        result[[pkg]] <- final_val
      }
    }
  }

  if (length(result) == 0L) return(character(0))
  unlist(result)
}

#' Build the per-run CRAN name snapshot: every name in the archive index or the
#' live CRAN listing, with identity_state = "live" when currently available else
#' "archived". Casing is preserved verbatim from CRAN. One row per name_lower;
#' a case collision keeps the live entry.
build_names_all <- function(archive_list, current_pkgs) {
  all_names <- union(names(archive_list), current_pkgs)
  if (length(all_names) == 0L) {
    return(data.frame(name_lower = character(0), canonical_name = character(0),
                      identity_state = character(0), stringsAsFactors = FALSE))
  }
  df <- data.frame(
    name_lower     = tolower(all_names),
    canonical_name = all_names,
    identity_state = ifelse(all_names %in% current_pkgs, "live", "archived"),
    stringsAsFactors = FALSE
  )
  # A case collision (two CRAN names differing only in case) keeps the live row.
  df <- df[order(df$name_lower, df$identity_state != "live"), , drop = FALSE]
  df <- df[!duplicated(df$name_lower), , drop = FALSE]
  rownames(df) <- NULL
  df
}

#' Fold a per-run name snapshot into the prior published table, append-only.
#'
#' Prior rows are never dropped; their first_seen and canonical_name are frozen.
#' identity_state and last_seen are refreshed for names still present this run;
#' a name that vanished upstream keeps its prior state. New names get
#' first_seen = last_seen = now. `prior_df` may be NULL or 0-row.
#'
#' @param prior_df   Prior published table, or NULL for a cold start.
#' @param current_df Per-run snapshot from build_names_all.
#' @param now        ISO 8601 date string for this run (e.g., "2026-07-09").
#' @return data.frame(name_lower, canonical_name, identity_state, first_seen,
#'   last_seen), one row per name in prior_df or current_df, sorted by name_lower.
merge_names_all <- function(prior_df, current_df, now) {
  cols  <- c("name_lower", "canonical_name", "identity_state", "first_seen", "last_seen")
  empty <- data.frame(name_lower = character(0), canonical_name = character(0),
                      identity_state = character(0), first_seen = character(0),
                      last_seen = character(0), stringsAsFactors = FALSE)
  if (is.null(prior_df) || nrow(prior_df) == 0L) prior_df <- empty

  cur_state <- current_df$identity_state
  names(cur_state) <- current_df$name_lower

  out_prior <- prior_df[, cols, drop = FALSE]
  present   <- out_prior$name_lower %in% current_df$name_lower
  st        <- out_prior$identity_state
  st[present] <- unname(cur_state[out_prior$name_lower[present]])
  out_prior$identity_state <- st
  out_prior$last_seen      <- rep_len(now, nrow(out_prior))

  fresh <- current_df[!(current_df$name_lower %in% prior_df$name_lower), , drop = FALSE]
  out_fresh <- if (nrow(fresh) > 0L) {
    data.frame(name_lower = fresh$name_lower, canonical_name = fresh$canonical_name,
               identity_state = fresh$identity_state, first_seen = now, last_seen = now,
               stringsAsFactors = FALSE)
  } else empty

  out <- rbind(out_prior, out_fresh)
  out <- out[order(out$name_lower), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Reject a partial or empty name fetch. Returns FALSE when the live-CRAN or
#' archive-index count is below its floor, signalling the caller to reuse the
#' prior published database rather than publish a shrunken one.
names_size_ok <- function(n_live, n_archive,
                          live_floor = CRAN_LIVE_FLOOR,
                          archive_floor = CRAN_ARCHIVE_FLOOR) {
  is.finite(n_live) && is.finite(n_archive) &&
    n_live >= live_floor && n_archive >= archive_floor
}

#' Write the cran_names_all table into an open connection.
export_names_all <- function(con, names_df) {
  RSQLite::dbExecute(con, "DROP TABLE IF EXISTS cran_names_all")
  RSQLite::dbExecute(con, "
    CREATE TABLE cran_names_all (
      name_lower     TEXT PRIMARY KEY,
      canonical_name TEXT NOT NULL,
      identity_state TEXT NOT NULL,
      first_seen     TEXT NOT NULL,
      last_seen      TEXT NOT NULL
    )")
  if (nrow(names_df) > 0L) {
    RSQLite::dbWriteTable(con, "cran_names_all",
      names_df[, c("name_lower", "canonical_name", "identity_state",
                   "first_seen", "last_seen"), drop = FALSE], append = TRUE)
  }
  invisible(NULL)
}

#' Default IO providers: real network fetchers for production use.
#'
#' Returns a named list of zero-argument functions:
#'   archive_rds()      -- downloads and returns the CRAN archive.rds named list.
#'   current_packages() -- returns a character vector of currently-available packages.
#'   removal_reasons()  -- fetches PACKAGES.in and returns a named character vector
#'                         mapping package name -> X-CRAN-Comment value.
default_io <- function() {
  list(
    archive_rds = function() {
      readRDS(url(CRAN_ARCHIVE_URL))
    },

    current_packages = function() {
      rownames(available.packages(repos = CRAN_PACKAGES_URL))
    },

    removal_reasons = function() {
      parse_packages_in(
        paste(readLines(url(CRAN_PACKAGES_IN_URL), warn = FALSE), collapse = "\n")
      )
    },

    prev_names = function() {
      empty <- data.frame(name_lower = character(0), canonical_name = character(0),
                          identity_state = character(0), first_seen = character(0),
                          last_seen = character(0), stringsAsFactors = FALSE)
      tmp <- tempfile(); dir.create(tmp, showWarnings = FALSE)
      on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
      st <- suppressWarnings(system2("gh",
        c("release", "download", "current", "--repo", PUBLISH_REPO,
          "--pattern", DB_FILENAME, "--dir", tmp, "--clobber"),
        stdout = FALSE, stderr = FALSE))
      db <- file.path(tmp, DB_FILENAME)
      if (!identical(as.integer(st), 0L) || !file.exists(db)) return(empty)
      con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
      on.exit(RSQLite::dbDisconnect(con), add = TRUE)
      if (!RSQLite::dbExistsTable(con, "cran_names_all")) return(empty)
      RSQLite::dbGetQuery(con, "SELECT * FROM cran_names_all")
    }
  )
}
