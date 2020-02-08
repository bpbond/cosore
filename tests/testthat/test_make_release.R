# Test csr_make_release and associated code

context("make-release")

test_that("substitute_release_info", {
  # Errors on bad data
  expect_error(substitute_release_info(1, "1", 0))
  expect_error(substitute_release_info("1", 1, 0))
  expect_error(substitute_release_info("1", "1", "0"))

  fdata <- c("%VERSION", "%DATE", "%DATABASE_SIZE", "%FILELIST")
  fdesc <- c("file1" = "file1", "file2" = "file2")
  out <- substitute_release_info(fdata, fdesc, 0, TRUE)
  expect_identical(length(fdata), length(out))
  expect_identical(out[which(fdata == "%VERSION")],
                   as.character(packageVersion("cosore")))
  expect_identical(out[which(fdata == "%DATE")], as.character(Sys.Date()))

  filelist <- strsplit(out[which(fdata == "%FILELIST")], split = "\n", fixed = TRUE)[[1]]
  expect_identical(length(filelist), length(fdesc))
})

test_that("csr_make_release", {
  # Errors on bad data
  expect_error(csr_make_release(1, TRUE, TRUE, TRUE, TRUE, "1"))
  expect_error(csr_make_release("1", "TRUE", TRUE, TRUE, TRUE, "1"))
  expect_error(csr_make_release("1", TRUE, "TRUE", TRUE, TRUE, "1"))
  expect_error(csr_make_release("1", TRUE, TRUE, "TRUE", TRUE, "1"))
  expect_error(csr_make_release("1", TRUE, TRUE, TRUE, "TRUE", "1"))
  expect_error(csr_make_release("1", TRUE, TRUE, TRUE, TRUE, 1))

  # Nonexistent path
  expect_error(csr_make_release("1", TRUE, TRUE, TRUE, TRUE, "1"))
  # Release directory not empty
  rd <- file.path(tempdir(), "test-temp")
  dir.create(rd)
  on.exit(unlink(rd))
  file.create(file.path(rd, "foo"))
  expect_error(csr_make_release(path = rd, TRUE, TRUE, TRUE, TRUE, "1"))
  unlink(file.path(rd, "foo"))
  # Vignette not rebuilt
  expect_error(csr_make_release("1", vignette_rebuilt = FALSE, TRUE, TRUE, TRUE, "1"))

  # At this point we use force=TRUE; testing git status seems hard
  zip <- csr_make_release(rd, TRUE, TRUE, FALSE, zip_release = FALSE, datasets = "TEST_licordata")
  expect_type(zip, "character")
  expect_true(file.exists(file.path(rd, "description.csv")))
  expect_true(file.exists(file.path(rd, "contributors.csv")))
  expect_true(file.exists(file.path(rd, "ports.csv")))
  expect_true(file.exists(file.path(rd, "columns.csv")))
  expect_true(file.exists(file.path(rd, "ancillary.csv")))
  expect_true(file.exists(file.path(rd, "diagnostics.csv")))
  expect_false(file.exists(zip))  # we didn't ask to create it

  # Creating the zip file generates annoying messages so skip for now
})
