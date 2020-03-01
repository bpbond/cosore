# test-.R

context("user")

test_that("csr_table", {
  # Handles bad data
  expect_error(csr_table(), regexp = "should be one of")  # informative error message

  x <- csr_table("sdfkjh")  # should return an empty data frame
  expect_s3_class(x, "data.frame")
  expect_identical(nrow(x), 0L)
})

test_that("csr_dataset", {
  # Handles bad data
  expect_error(csr_dataset(1))
  expect_error(csr_dataset(LETTERS))
  expect_error(csr_dataset("x", quiet = "quiet"))
  expect_error(csr_dataset("x", metadata_only = "metadata_only"))

  x <- csr_dataset("TEST_licordata", metadata_only = TRUE)
  expect_is(x, "list")
  expect_identical(sort(names(x)),
                   sort(c("description", "contributors", "ports", "columns", "ancillary")))
})

test_that("csr_database", {
  x <- csr_database()
  expect_s3_class(x, "data.frame")
  y <- list_datasets()
  expect_identical(nrow(x), length(y))  # should be one row per dataset
})

test_that("extract_table", {
  expect_s3_class(extract_table("TEST_licordata", "description", TRUE), "data.frame")
  expect_null(extract_table("x", "x", TRUE, retrieval_function = function(...) NULL))
  expect_null(extract_table("x", "x", TRUE, retrieval_function = function(...) list(x = 1)))
  expect_null(extract_table("x", "x", TRUE, retrieval_function = function(...) list(x = data.frame())))
})
