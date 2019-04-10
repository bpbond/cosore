# test-utils.R

context("utils")

test_that("split_licor_file", {
  tf <- tempfile()
  nr <- "LI-8100"  # new record delimiter

  # handles empty file
  cat("", sep = "\n", file = tf)
  expect_equal(split_licor_file(tf), 0)

  cat(rep(c(nr, rep("data", 10)), 2), sep = "\n", file = tf)

  # split_lines smaller than record size
  expect_equal(split_licor_file(tf, split_lines = 2), 2)
  # split_lines larger than record size
  expect_equal(split_licor_file(tf, split_lines = 20), 1)
  # split_lines larger than file size
  expect_equal(split_licor_file(tf, split_lines = 200), 1)

  # bad input
  expect_error(split_licor_file("nonexistent-file"))
  expect_error(split_licor_file(tf, split_lines = 0))
  expect_error(split_licor_file(tf, out_dir = "nonexistent-dir"))
})
