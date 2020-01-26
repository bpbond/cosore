# test-.R

context("user")

test_that("csr_table", {
  expect_message(csr_table(), "should be one of")  # informative error message
  x <- csr_table("sdfkjh")  # should return an empty data frame
  expect_s3_class(x, "data.frame")
  expect_identical(nrow(x), 0L)
})

test_that("csr_dataset", {

})

test_that("csr_database", {
  x <- csr_database()
  expect_s3_class(x, "data.frame")
  y <- list_datasets()
  expect_identical(nrow(x), length(y))  # should be one row per dataset
})
