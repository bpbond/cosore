# test-.R

context("user")

test_that("csr_table", {

})

test_that("csr_dataset", {

})

test_that("csr_database", {
  x <- csr_database()
  expect_s3_class(x, "data.frame")
  y <- list_datasets()
  expect_identical(nrow(x), length(y))  # should be one row per dataset
})
