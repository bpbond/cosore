# test-release.R

context("release")

test_that("make_cosore_release", {

  # Read test dataset, duplicate (for an embargo version) and make into a database object
  x <- read_dataset("TEST_licordata", raw_data = "test_raw_data/")
  x_embargo <- x
  x_embargo$description$CSR_DATASET <- paste0(x$description$CSR_DATASET, "_embargo")
  x_embargo$description$CSR_EMBARGO <- TRUE  # just needs to exist
  all_data <- combine_data(c(x$description$CSR_DATASET,
                             x_embargo$description$CSR_DATASET),
                           x, x_embargo)

  # Test directory-exists check
  expect_error(csr_make_release(all_data, "sdffds"), regexp = "doesn't exist")

  # Test the clean-directory check
  releasedir <- file.path(tempdir(check = TRUE), "temp")
  dir.create(releasedir)
  tf <- file.path(releasedir, "tempfile")
  file.create(tf)
  expect_error(csr_make_release(all_data, releasedir), regexp = "release directory is not clean")
  unlink(tf)

  # Can't really test the clean-git check I don't think

  # Test the vignette-rebuild check
  expect_error(csr_make_release(all_data, releasedir, force = TRUE), regexp = "Vignette rebuilt")

  # Removes embargoed data
  expect_message(csr_make_release(all_data, releasedir,
                                  vignette_rebuilt = TRUE, force = TRUE,
                                  run_report = FALSE, zip_release = FALSE),
                 regexp = "has an embargo entry--removing data")
  unlink(releasedir)
})
