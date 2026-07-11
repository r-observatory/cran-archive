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

#' Extract the archival date from an X-CRAN-Comment string.
#'
#' CRAN records the true archival date in the comment prose, e.g.
#' "Archived on 2026-07-10 as ..." or "Removed on 2024-11-25 for ...". Returns
#' the first ISO date (YYYY-MM-DD) found, or NA_character_ when the comment is
#' absent or carries no date.
#'
#' @param comment A single X-CRAN-Comment string (or NA).
comment_archived_on <- function(comment) {
  if (is.null(comment) || length(comment) == 0L || is.na(comment) || !nzchar(comment)) {
    return(NA_character_)
  }
  m <- regmatches(comment, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", comment))
  if (length(m) == 0L || !nzchar(m[1L])) NA_character_ else m[1L]
}

#' Strip the leading "<verb> on <date>" status prefix (and its connector) from an
#' X-CRAN-Comment, leaving just the cause: "Archived on 2026-07-10 as email ..."
#' becomes "email ...". The date is surfaced separately via comment_archived_on(),
#' so it need not be repeated in the reason. Returns the trimmed comment unchanged
#' when there is no date to strip, and NA_character_ for an absent comment.
#'
#' @param comment A single X-CRAN-Comment string (or NA).
clean_comment_reason <- function(comment) {
  if (is.null(comment) || length(comment) == 0L || is.na(comment) || !nzchar(comment)) {
    return(NA_character_)
  }
  # Drop everything up to and including the first ISO date, then a leading
  # connector word ("as"/"for"/"at"). Applied twice to catch a stray connector
  # left when the first date is not the leading status date.
  after <- sub("^.*?[0-9]{4}-[0-9]{2}-[0-9]{2}", "", comment, perl = TRUE)
  if (!nzchar(trimws(after))) after <- comment
  after <- sub("^[ ,;:.-]*(?:as|for|at)\\b[[:space:]]*", "", after, perl = TRUE)
  after <- sub("^[ ,;:.-]*(?:as|for|at)\\b[[:space:]]*", "", after, perl = TRUE)
  after <- gsub("[[:space:]]+", " ", trimws(after))
  after <- sub("[ .]+$", "", after)
  if (nzchar(after)) after else trimws(comment)
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
    last_path      <- rownames(df)[max_idx]
    last_version   <- parse_archive_version(last_path, pkg)
    raw_reason     <- if (pkg %in% names(reasons)) unname(reasons[pkg]) else NA_character_
    # The max tarball mtime is only the LAST-RELEASE date, which can predate the
    # actual archival by years (a stable package archived when its maintainer's
    # email became undeliverable, say). CRAN records the true archival date in the
    # X-CRAN-Comment ("Archived on YYYY-MM-DD as ..."); prefer it, and fall back to
    # the tarball mtime only when the comment carries no date. The stored reason is
    # the cause only, with that "Archived on <date> as" prefix stripped (the date
    # is surfaced separately via archived_on).
    archived_on    <- comment_archived_on(raw_reason) %||%
                        format(as.Date(mt[max_idx]), "%Y-%m-%d")
    removal_reason <- clean_comment_reason(raw_reason)
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
#' Creates (or replaces) the file at path with three tables:
#'   cran_archive         -- one row per archived package (7 columns)
#'   cran_archive_events  -- one row per release/archived event (4 columns)
#'   cran_archive_history -- one row per archival episode (closed + at most one
#'                            open per package), with a partial unique index
#'                            enforcing at most one open episode per package.
#' and indexes on cran_archive_events(package) and cran_archive_history.
#'
#' @param path       File path for the output .db file.
#' @param archive_df data.frame with columns (package, first_release,
#'   archived_on, last_version, removal_reason) as returned by build_archive().
#'   source and updated_at columns are added automatically.
#' @param events_df  data.frame with columns (package, event_date, event_type,
#'   version) as returned by build_archive_events().
#' @param history_df data.frame with columns (package, episode_seq,
#'   archived_on, relisted_on, removal_reason, last_version, relist_source,
#'   archived_on_source) as returned by build_archive_history(). source and
#'   updated_at columns are added automatically.
#' @param lineage_df data.frame with columns (package, seq, event_date, action,
#'   reason) as returned by build_archive_lineage(). Written to
#'   cran_archive_lineage, with an action histogram in cran_archive_action_counts.
export_archive <- function(path, archive_df, events_df, history_df, lineage_df) {
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

  RSQLite::dbExecute(con, "
    CREATE TABLE cran_archive_history (
      package            TEXT NOT NULL,
      episode_seq        INTEGER NOT NULL,
      archived_on        TEXT NOT NULL,
      relisted_on        TEXT,
      removal_reason     TEXT,
      last_version       TEXT,
      relist_source      TEXT,
      archived_on_source TEXT,
      source             TEXT NOT NULL DEFAULT 'cran-archive-rds',
      updated_at         TEXT NOT NULL,
      PRIMARY KEY (package, episode_seq),
      CHECK (relisted_on IS NULL OR relisted_on >= archived_on)
    )
  ")
  RSQLite::dbExecute(con,
    "CREATE UNIQUE INDEX ux_cran_archive_history_open ON cran_archive_history(package) WHERE relisted_on IS NULL")
  RSQLite::dbExecute(con,
    "CREATE INDEX idx_cran_archive_history_pkg ON cran_archive_history(package)")
  RSQLite::dbExecute(con,
    "CREATE INDEX idx_cran_archive_history_relisted ON cran_archive_history(relisted_on) WHERE relisted_on IS NOT NULL")

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

  hist_w <- history_df
  hist_w$source     <- rep_len("cran-archive-rds", nrow(hist_w))
  hist_w$updated_at <- rep_len(now, nrow(hist_w))
  hist_w <- hist_w[, c("package","episode_seq","archived_on","relisted_on","removal_reason",
                       "last_version","relist_source","archived_on_source","source","updated_at"),
                   drop = FALSE]
  RSQLite::dbWriteTable(con, "cran_archive_history", hist_w, append = TRUE)

  # Per-package ordered event lineage parsed from the PACKAGES.in annotations,
  # plus a histogram of how often each action occurs across all packages.
  RSQLite::dbExecute(con, "
    CREATE TABLE cran_archive_lineage (
      package    TEXT NOT NULL,
      seq        INTEGER NOT NULL,
      event_date TEXT,
      action     TEXT NOT NULL,
      reason     TEXT,
      PRIMARY KEY (package, seq)
    )
  ")
  RSQLite::dbExecute(con,
    "CREATE INDEX idx_cran_archive_lineage_pkg ON cran_archive_lineage(package)")
  RSQLite::dbExecute(con,
    "CREATE INDEX idx_cran_archive_lineage_action ON cran_archive_lineage(action)")

  RSQLite::dbExecute(con, "
    CREATE TABLE cran_archive_action_counts (
      action TEXT PRIMARY KEY,
      n      INTEGER NOT NULL
    )
  ")

  if (!is.null(lineage_df) && nrow(lineage_df) > 0L) {
    lin_w <- lineage_df[, c("package", "seq", "event_date", "action", "reason"), drop = FALSE]
    RSQLite::dbWriteTable(con, "cran_archive_lineage", lin_w, append = TRUE)

    tab <- as.data.frame(table(lineage_df$action), stringsAsFactors = FALSE)
    counts_df <- data.frame(action = tab[[1L]], n = as.integer(tab[[2L]]),
                            stringsAsFactors = FALSE)
    counts_df <- counts_df[order(counts_df$action), , drop = FALSE]
    RSQLite::dbWriteTable(con, "cran_archive_action_counts", counts_df, append = TRUE)
  }

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

#' Normalise a reason fragment: drop a leading "as ", trailing punctuation and
#' collapse whitespace. Returns NA_character_ when nothing remains.
clean_history_reason <- function(x) {
  x <- trimws(x)
  # Strip the connector CRAN uses after the archival date ("as"/"for"/"at"),
  # matching clean_comment_reason so open- and closed-episode reasons read the same.
  x <- sub("^(as|for|at)[[:space:]]+", "", x)
  x <- sub("[.[:space:]]+$", "", x)
  x <- gsub("[[:space:]]+", " ", x)
  if (!nzchar(x)) NA_character_ else x
}

#' Parse a single X-CRAN-History string into closed/open archival episodes.
#' Only dated "Archived on YYYY-MM-DD" / "Unarchived on YYYY-MM-DD" markers are
#' recognised; other verbs (Orphaned/Removed) and undated events are ignored.
#' An "Archived" opens an episode; the next "Unarchived" closes it.
parse_history_episodes <- function(hist) {
  if (is.null(hist) || is.na(hist) || !nzchar(hist)) return(list())
  pat <- "(Archived|Unarchived) on ([0-9]{4}-[0-9]{2}-[0-9]{2})"
  g <- gregexpr(pat, hist, perl = TRUE)[[1L]]
  if (g[1L] == -1L) return(list())
  starts <- as.integer(g); lens <- attr(g, "match.length")
  markers <- lapply(seq_along(starts), function(i) {
    s <- starts[i]; e <- s + lens[i] - 1L
    txt  <- substr(hist, s, e)
    kind <- sub(" on .*$", "", txt)
    date <- sub("^.* on ", "", txt)
    reason_end <- if (i < length(starts)) starts[i + 1L] - 1L else nchar(hist)
    reason <- clean_history_reason(substr(hist, e + 1L, reason_end))
    list(kind = kind, date = date, reason = reason)
  })
  episodes <- list(); open <- NULL
  for (mk in markers) {
    if (mk$kind == "Archived") {
      if (!is.null(open)) episodes[[length(episodes) + 1L]] <- open
      open <- list(archived_on = mk$date, removal_reason = mk$reason,
                   relisted_on = NA_character_)
    } else if (mk$kind == "Unarchived" && !is.null(open)) {
      open$relisted_on <- mk$date
      episodes[[length(episodes) + 1L]] <- open
      open <- NULL
    }
  }
  if (!is.null(open)) episodes[[length(episodes) + 1L]] <- open
  episodes
}

#' Parse PACKAGES.in text into a named list: package -> list of episodes from its
#' X-CRAN-History field. Mirrors parse_packages_in()'s record/continuation walk.
parse_packages_history <- function(text) {
  text <- gsub("\r\n", "\n", text); text <- gsub("\r", "\n", text)
  records <- strsplit(text, "\n{2,}")[[1L]]
  result <- list()
  for (rec in records) {
    lines <- strsplit(rec, "\n")[[1L]]
    if (length(lines) == 0L) next
    pkg <- NA_character_; hist_val <- NULL; in_hist <- FALSE
    for (line in lines) {
      if (grepl("^Package:", line)) {
        pkg <- trimws(sub("^Package:\\s*", "", line)); in_hist <- FALSE
      } else if (grepl("^X-CRAN-History:", line)) {
        hist_val <- trimws(sub("^X-CRAN-History:\\s*", "", line)); in_hist <- TRUE
      } else if (in_hist && grepl("^[ \t]", line)) {
        cont <- trimws(line)
        if (cont != ".") hist_val <- paste(hist_val, cont)
      } else if (grepl("^[A-Za-z]", line)) {
        in_hist <- FALSE
      }
    }
    if (!is.na(pkg) && !is.null(hist_val) && nzchar(hist_val)) {
      eps <- parse_history_episodes(hist_val)
      if (length(eps) > 0L) result[[pkg]] <- eps
    }
  }
  result
}

#' Compose the durable episode table: closed episodes (from X-CRAN-History) plus
#' at most one open episode per package (the current archival, from archive_df).
#' Only well-formed CLOSED history episodes are kept, so the sole NULL-relisted_on
#' row per package is the open one (preserving the single-open invariant).
build_archive_history <- function(archive_df, history_map = list()) {
  empty <- data.frame(package=character(0), episode_seq=integer(0),
    archived_on=character(0), relisted_on=character(0), removal_reason=character(0),
    last_version=character(0), relist_source=character(0), archived_on_source=character(0),
    stringsAsFactors=FALSE)

  open_map <- list()
  if (nrow(archive_df) > 0L) for (i in seq_len(nrow(archive_df))) {
    if (is.na(archive_df$archived_on[i])) next
    p <- archive_df$package[i]
    open_map[[p]] <- list(archived_on = archive_df$archived_on[i], relisted_on = NA_character_,
      removal_reason = archive_df$removal_reason[i], last_version = archive_df$last_version[i],
      relist_source = NA_character_, archived_on_source = "archive-rds")
  }
  all_pkgs <- union(names(history_map), names(open_map))
  if (length(all_pkgs) == 0L) return(empty)

  rows <- list()
  for (p in all_pkgs) {
    eps <- list()
    for (e in (history_map[[p]] %||% list())) {
      if (is.na(e$relisted_on)) next                                   # keep only closed
      if (!is.na(e$archived_on) && e$relisted_on < e$archived_on) next # drop negative-duration
      eps[[length(eps) + 1L]] <- list(archived_on = e$archived_on, relisted_on = e$relisted_on,
        removal_reason = e$removal_reason, last_version = NA_character_,
        relist_source = "x-cran-history", archived_on_source = "x-cran-history")
    }
    if (!is.null(open_map[[p]])) {
      od  <- open_map[[p]]$archived_on
      eps <- Filter(function(e) is.na(e$archived_on) || e$archived_on != od, eps)
      eps[[length(eps) + 1L]] <- open_map[[p]]
    }
    if (length(eps) == 0L) next
    eps <- eps[order(vapply(eps, function(e) e$archived_on %||% "", character(1L)))]
    for (k in seq_along(eps)) {
      e <- eps[[k]]
      rows[[length(rows) + 1L]] <- data.frame(package = p, episode_seq = k,
        archived_on = e$archived_on, relisted_on = e$relisted_on %||% NA_character_,
        removal_reason = e$removal_reason %||% NA_character_,
        last_version = e$last_version %||% NA_character_,
        relist_source = e$relist_source %||% NA_character_,
        archived_on_source = e$archived_on_source %||% NA_character_,
        stringsAsFactors = FALSE)
    }
  }
  if (length(rows) == 0L) return(empty)
  out <- do.call(rbind, rows); rownames(out) <- NULL
  out[order(out$package, out$episode_seq), , drop = FALSE]
}

#' Canonicalise a CRAN status verb to one of the six lineage actions.
canon_action <- function(verb) {
  v <- tolower(verb)
  if (v %in% c("unarchived", "unorphaned", "restored", "reinstated")) "unarchived"
  else if (v == "archived")   "archived"
  else if (v == "orphaned")   "orphaned"
  else if (v == "removed")    "removed"
  else if (v == "renamed")    "renamed"
  else if (v == "replaced")   "replaced"
  else if (v == "deprecated") "deprecated"
  else NA_character_
}

# Status verbs that begin an event line.
.LINEAGE_VERBS <- c("Unarchived", "Unorphaned", "Archived", "Orphaned",
                    "Removed", "Renamed", "Replaced", "Restored",
                    "Reinstated", "Deprecated")

#' Cause for a lineage event line. For a dated event, the text after the first ISO
#' date with a leading "as"/"for"/"at" connector removed, or NA when there is no
#' cause (e.g. "Unarchived on 2020-02-13." has nothing after the date). For an
#' undated event, the line itself (a descriptive cause such as a version-range
#' removal), trailing period trimmed.
event_reason <- function(line, date) {
  if (is.na(date)) {
    r <- sub("[ .]+$", "", gsub("[[:space:]]+", " ", trimws(line)))
    return(if (nzchar(r)) r else NA_character_)
  }
  after <- sub("^.*?[0-9]{4}-[0-9]{2}-[0-9]{2}", "", line, perl = TRUE)
  after <- sub("^[ ,;:.-]*(?:as|for|at)\\b[[:space:]]*", "", after, perl = TRUE)
  after <- sub("^[ ,;:.-]*(?:as|for|at)\\b[[:space:]]*", "", after, perl = TRUE)
  after <- sub("[ .]+$", "", gsub("[[:space:]]+", " ", trimws(after)))
  if (nzchar(after)) after else NA_character_
}

#' Parse a DCF field value (X-CRAN-History or X-CRAN-Comment) into an ordered list
#' of events, each list(action, date, reason). read.dcf keeps each event on its own
#' line, so we split on newlines: a verb-led line (or a "Versions ... removed" line)
#' starts a new event; other lines are continuation reason for the current event;
#' "." marker lines and blanks are dropped. The date is the first ISO date on the
#' event line (NA when undated); the reason is the cause via event_reason() (NA when
#' a dated event carries no cause, e.g. an unarchival).
parse_event_lines <- function(field) {
  if (is.null(field) || length(field) == 0L || is.na(field) || !nzchar(field)) return(list())
  lines <- trimws(strsplit(field, "\n", fixed = TRUE)[[1L]])
  lines <- lines[nzchar(lines) & lines != "."]
  events <- list(); cur <- NULL
  for (ln in lines) {
    action <- NA_character_
    for (v in .LINEAGE_VERBS) {
      if (startsWith(ln, v)) {
        nxt <- substr(ln, nchar(v) + 1L, nchar(v) + 1L)
        if (!nzchar(nxt) || !grepl("[A-Za-z]", nxt)) { action <- canon_action(v); break }
      }
    }
    if (is.na(action) && grepl("^Versions? .*removed", ln)) action <- "removed"
    if (!is.na(action)) {
      if (!is.null(cur)) events <- c(events, list(cur))
      d <- regmatches(ln, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", ln))
      d <- if (length(d)) d[1L] else NA_character_
      cur <- list(action = action, date = d, reason = event_reason(ln, d))
    } else if (!is.null(cur)) {
      extra <- sub("[.[:space:]]+$", "", trimws(ln))
      cur$reason <- if (is.null(cur$reason) || is.na(cur$reason) || !nzchar(cur$reason)) extra
                    else paste(cur$reason, extra)
    }
  }
  if (!is.null(cur)) events <- c(events, list(cur))
  events
}

#' Build the per-package event lineage from a read.dcf() matrix of PACKAGES.in.
#' For each package: the past events from X-CRAN-History (chronological), then the
#' current open archival from X-CRAN-Comment, then a "replaced" event if a
#' Replaced_by field is present. Returns data.frame(package, seq, event_date,
#' action, reason) ordered by package then seq.
build_archive_lineage <- function(dcf) {
  empty <- data.frame(package = character(0), seq = integer(0), event_date = character(0),
                      action = character(0), reason = character(0), stringsAsFactors = FALSE)
  if (is.null(dcf) || nrow(dcf) == 0L || !"Package" %in% colnames(dcf)) return(empty)
  cols <- colnames(dcf)
  getf <- function(i, f) if (f %in% cols) dcf[i, f] else NA_character_
  frames <- vector("list", nrow(dcf))
  for (i in seq_len(nrow(dcf))) {
    # Matrix indexing carries the column name onto the scalar; drop it so it does
    # not surface as a discarded data.frame row name.
    pkg <- unname(dcf[i, "Package"])
    if (is.na(pkg) || !nzchar(pkg)) next
    ev <- c(parse_event_lines(getf(i, "X-CRAN-History")),
            parse_event_lines(getf(i, "X-CRAN-Comment")))
    rb <- getf(i, "Replaced_by")
    if (!is.na(rb) && nzchar(rb)) {
      ev <- c(ev, list(list(action = "replaced", date = NA_character_,
                            reason = paste0("replaced by ", trimws(rb)))))
    }
    if (length(ev) == 0L) next
    frames[[i]] <- data.frame(
      package    = pkg,
      seq        = seq_along(ev),
      event_date = vapply(ev, function(e) if (is.null(e$date) || is.na(e$date)) NA_character_ else e$date, character(1L)),
      action     = vapply(ev, function(e) e$action, character(1L)),
      reason     = vapply(ev, function(e) if (is.null(e$reason) || is.na(e$reason) || !nzchar(e$reason)) NA_character_ else e$reason, character(1L)),
      stringsAsFactors = FALSE)
  }
  frames <- Filter(Negate(is.null), frames)
  if (length(frames) == 0L) return(empty)
  out <- do.call(rbind, frames); rownames(out) <- NULL
  out[order(out$package, out$seq), , drop = FALSE]
}

#' Default IO providers: real network fetchers for production use.
#'
#' Returns a named list of zero-argument functions:
#'   archive_rds()      -- downloads and returns the CRAN archive.rds named list.
#'   current_packages() -- returns a character vector of currently-available packages.
#'   removal_reasons()  -- fetches PACKAGES.in and returns a named character vector
#'                         mapping package name -> X-CRAN-Comment value.
#'   removal_history()  -- fetches PACKAGES.in (cached with removal_reasons() in the
#'                         same call) and returns a named list of X-CRAN-History
#'                         episodes, package -> list of episodes.
#'   packages_dcf()     -- fetches PACKAGES.in (same cache) and returns it parsed as
#'                         a read.dcf() character matrix for the event lineage.
#'   prev_names()       -- downloads the prior published database and returns its
#'                         cran_names_all table (0-row on a genuine cold start;
#'                         throws when the prior release is unreachable).
default_io <- function() {
  .pin <- NULL
  packages_in <- function() {
    if (is.null(.pin)) {
      .pin <<- paste(readLines(url(CRAN_PACKAGES_IN_URL), warn = FALSE), collapse = "\n")
    }
    .pin
  }
  list(
    archive_rds      = function() readRDS(url(CRAN_ARCHIVE_URL)),
    current_packages = function() rownames(available.packages(repos = CRAN_PACKAGES_URL)),
    removal_reasons  = function() parse_packages_in(packages_in()),
    removal_history  = function() parse_packages_history(packages_in()),
    packages_dcf     = function() read.dcf(textConnection(packages_in())),

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
      # A failed download is a transient/network problem, not evidence that no
      # table has ever been published. Throw so the caller can tell this apart
      # from a genuine cold start and avoid resetting first_seen for everyone.
      if (!identical(as.integer(st), 0L) || !file.exists(db)) {
        stop("prior release unreachable")
      }
      con <- RSQLite::dbConnect(RSQLite::SQLite(), db)
      on.exit(RSQLite::dbDisconnect(con), add = TRUE)
      # A successful download with no cran_names_all table is the genuine
      # cold-start case: the table has simply never been published yet.
      if (!RSQLite::dbExistsTable(con, "cran_names_all")) return(empty)
      RSQLite::dbGetQuery(con, "SELECT * FROM cran_names_all")
    }
  )
}
