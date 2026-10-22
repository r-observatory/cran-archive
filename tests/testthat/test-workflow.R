# The release step must never upload a database that lost a table carrying
# state, and a run that left cran_tarballs unchanged must not look green.
# Hashing must stop in time for the publish step to run inside the timeout.

.update_yml <- function() {
  readLines(testthat::test_path("..", "..", ".github", "workflows", "update.yml"))
}

test_that("the publish step skips a database whose prior cran_tarballs was unreadable", {
  wf <- paste(.update_yml(), collapse = "\n")
  expect_match(wf, "TARBALLS_STATE=$(jq -r '.tarballs_state' out/manifest.json)", fixed = TRUE)
  expect_match(wf, '[ "$NAMES_HEALTHY" != "false" ] && [ "$TARBALLS_STATE" != "unreachable" ]',
               fixed = TRUE)
})

test_that("a run that did not update cran_tarballs fails after the publish step", {
  wf <- .update_yml()
  publish <- grep('- name: Publish to the "current" release', wf, fixed = TRUE)
  check   <- grep("- name: Check the tarball record", wf, fixed = TRUE)
  expect_length(check, 1L)
  expect_gt(check, publish)
  expect_true(any(grepl("cold_start|updated) ;;", wf, fixed = TRUE)))
  expect_true(any(grepl("exit 1 ;;", wf[check:length(wf)], fixed = TRUE)))
})

test_that("the update job records when it started, before the build step reads it", {
  wf <- .update_yml()
  job      <- grep("^  update:", wf)
  record   <- grep('echo "JOB_STARTED_AT=$(date +%s)" >> "$GITHUB_ENV"', wf, fixed = TRUE)
  checkout <- grep("- uses: actions/checkout@", wf, fixed = TRUE)
  build    <- grep("- name: Build archive database", wf, fixed = TRUE)
  expect_length(record, 1L)
  # The first step of the job, so the clock starts with the job's own.
  expect_gt(record, job)
  expect_lt(record, checkout[checkout > job][1])
  expect_lt(record, build)
})

test_that("hashing stops early enough for its last download and the publish to fit the job timeout", {
  wf <- .update_yml()
  job <- grep("^  update:", wf)
  tm  <- grep("timeout-minutes:", wf, fixed = TRUE)
  tm  <- tm[tm > job][1]
  minutes <- as.numeric(sub("^.*timeout-minutes:[[:space:]]*", "", wf[tm]))
  expect_equal(minutes, 30)
  # A download already under way at the deadline runs at most one more attempt;
  # five minutes cover writing the database and the publish step.
  expect_lte(TARBALL_HASH_JOB_SECS + TARBALL_HASH_TIMEOUT + 300, minutes * 60)
  expect_lte(TARBALL_HASH_MAX_SECS, TARBALL_HASH_JOB_SECS)
})

test_that("the tarball check prints the hash counts and fails only when hashing broke", {
  wf <- .update_yml()
  check <- grep("- name: Check the tarball record", wf, fixed = TRUE)
  next_job <- grep("^  keepalive:", wf)
  body  <- paste(wf[check:(next_job - 1L)], collapse = "\n")
  for (field in c(".tarballs_hashed", ".tarballs_to_hash", ".tarball_hash_mismatches",
                  ".tarball_hash_failures", ".tarball_hash_stop")) {
    expect_match(body, field, fixed = TRUE)
  }
  expect_match(body, '[ "$HASH_STOP" = "error" ]', fixed = TRUE)
  expect_match(body, "::warning::", fixed = TRUE)
})
