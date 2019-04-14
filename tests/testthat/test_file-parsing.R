# File parsing tests

context("file-parsing")

test_that("extract_line", {
  labels <- c("1", "One", "Two", "Three label")
  dat <- c("one", "1", "This is two", "3")
  sep <- ":"
  fd <- paste(labels, dat, sep = sep)

  for(i in seq_along(labels)) {
    expect_identical(extract_line(fd, line_label = labels[i], sep = sep), dat[i])
  }

  # Numeric and non-numeric data
  expect_is(extract_line("one:1", "one", numeric_data = TRUE), "numeric")
  expect_error(extract_line("one:one", "one", numeric_data = TRUE))

  # Data contains separator
  expect_identical(extract_line("two:this is:two", "two"), "this is:two")

  # Data not present
  expect_error(extract_line(fd, "not there"))
  expect_identical(extract_line(fd, "not there", required = FALSE), NA_character_)
})

test_that("list_datasets", {
  expect_is(list_datasets(), "character")

  td <- tempdir()
  td <- file.path(td, "clean-dir")
  dir.create(td)
  dir.create(file.path(td, "not-a-dataset"))
  d <- "d20190407_TEST"
  dir.create(file.path(td, d))

  # greps for correct pattern
  expect_identical(list_datasets(path = td), d)
})

test_that("resolve_dataset", {
  # errors on non-existent data
  expect_error(resolve_dataset("this doesn't exist"))
})

test_that("read_file", {
  # strips out comments
  fd <- c("One", "# Two", "Three")
  expect_identical(length(read_file(file_data = fd)), 2L)
})

test_that("read_csv_data", {

  x <- c("One,\tTwo,Three", "1,2,three", "2,,\ttrois")

  # reads data correctly
  y <- read_csv_data(x)
  expect_is(y, "data.frame")
  expect_equal(nrow(y), length(x) - 1)
  expect_is(y$Three, "character")  # no factors

  # errors on empty required column
  expect_error(read_csv_data(x, required = "Two"))
  # errors on non-existent column
  expect_error(read_csv_data(x, required = "Four"))
})

test_that("read_description_file", {
  # description file
  labels <- c("Site_name", "Longitude", "Latitude", "Elevation", "UTC_offset", "Instrument", "IGBP")
  dat <- c("site", "1", "2", "3", "-1", "ins", "et")
  fd <- paste(labels, dat, sep = ":")

  x <- read_description_file("x", file_data = fd)
  expect_is(x, "data.frame")
  expect_equivalent(nrow(x), 1)
  expect_true(all(labels %in% names(x)))

  # Ignores unknown labels
  labels <- c(labels, "Unknown_label")
  dat <- c(dat, "x")
  fd <- paste(labels, dat, sep = ":")
  x1 <- read_description_file("x", file_data = fd)
  expect_identical(colnames(x), colnames(x1))
})
