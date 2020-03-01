# Test csr_make_release and associated code

context("make-release")

test_that("check_file_description_consistency", {
  # Bad inputs
  expect_error(check_file_description_consistency(1, "1"))
  expect_error(check_file_description_consistency("1", 1))

  # Consistent
  fd <- c("x" = "x-desc", "y" = "y-desc")
  af <- names(fd)
  expect_silent(check_file_description_consistency(af, fd))

  # Missing file
  af <- names(fd)[1]
  expect_error(check_file_description_consistency(af, fd),
               regexp = "Missing file for descriptions")
})

test_that("write_dataset_data", {
  # Bad inputs
  expect_error(write_dataset_data(1, list(), "", 1))
  expect_error(write_dataset_data("1", 1, "", 1))
  expect_error(write_dataset_data("1", list(), 1, 1))
  expect_error(write_dataset_data("1", list(), "", "1"))

  # Skips embargoed data
  ds <- list(description = data.frame(CSR_DATASET = "test_data", CSR_EMBARGO = "yes"))
  expect_message(write_dataset_data("1", ds, "", 0), regexp = "has an embargo entry")

  # Writes data and updates db_size
  ds <- list(description = data.frame(CSR_DATASET = "test_data"), data = cars)
  td <- tempdir()
  expect_message(x <- write_dataset_data("test_data", ds, td, 0), regexp = "Writing")
  expect_gt(x, 0)
})

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
  expect_error(csr_make_release("1", TRUE, TRUE, TRUE, TRUE, "1", sys_call = 1))

  # Nonexistent path
  expect_error(csr_make_release("1", TRUE, TRUE, TRUE, TRUE, "1"))

  # Release directory not empty
  rd <- file.path(tempdir(), "test-temp")
  dir.create(rd)
  on.exit(unlink(rd))
  file.create(file.path(rd, "foo"))
  expect_error(csr_make_release(path = rd, force = FALSE), regexp = "is not empty")
  unlink(file.path(rd, "foo"))

  # Vignette not rebuilt
  expect_error(csr_make_release(path = rd, force = TRUE, vignette_rebuilt = FALSE), regexp = "Vignette rebuilt")

  # Handles git-not-clean
  sys2 <- function(...) 1
  expect_error(csr_make_release(path = rd, force = FALSE,
                                vignette_rebuilt = TRUE,
                                sys_call = sys2),
               regexp = "git working directory is not clean")

  # Writes combined tables
  zip <- csr_make_release(path = rd, vignette_rebuilt = TRUE, force = TRUE,
                          run_report = FALSE, zip_release = FALSE,
                          datasets = list_datasets()[1:2])
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
