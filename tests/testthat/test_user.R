# test-.R

context("user")

test_that("user utilities work", {

  all_data <- list(list(data = cars, dataset_name = "a"),
                   list(data = cars, dataset_name = "b"),
                   list(data = cars, dataset_name = "c"))

  x <- csr_table(all_data, "data")
  expect_is(x, "data.frame")
  expect_identical(nrow(x), nrow(cars) * length(all_data))
  expect_true("CSR_DATASET" %in% names(x))
  })
