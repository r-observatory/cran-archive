# The release step must never upload a database that lost a table carrying
# state, and a run that left cran_tarballs unchanged must not look green.

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
