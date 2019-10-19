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
  labels <- c("CSR_SITE_NAME", "CSR_LONGITUDE", "CSR_LATITUDE", "CSR_ELEVATION",
              "CSR_TIMEZONE", "CSR_IGBP",
              "CSR_INSTRUMENT",
              "CSR_FILE_FORMAT", "CSR_TIMESTAMP_FORMAT", "CSR_TIMESTAMP_TZ")
  dat <- c("site", "1", "2", "3",
           "America/New_York", "igbp",
           "ins",
           "ff", "tsf", "tstz")
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

test_that("read_contributors_file", {
  # contributors file
  labels <- c("CSR_FIRST_NAME", "CSR_FAMILY_NAME", "CSR_EMAIL", "CSR_ORCID", "CSR_ROLE")
  dat <- c("Ben", "BL", "ben@bbl.com", "0000-0000-0000-0000", "role")
  fd <- c(paste(labels, collapse = ","), paste(dat, collapse = ","))

  x <- read_contributors_file("x", file_data = fd)
  expect_is(x, "data.frame")
  expect_equivalent(nrow(x), 1)
  expect_true(all(labels %in% names(x)))

  # Catches blank names
  dat <- c("", "BL", "ben@bbl.com", "0000-0000-0000-0000", "role")
  fd <- c(paste(labels, collapse = ","), paste(dat, collapse = ","))
  expect_error(read_contributors_file("x", file_data = fd))
  dat <- c("Ben", "", "ben@bbl.com", "0000-0000-0000-0000", "role")
  fd <- c(paste(labels, collapse = ","), paste(dat, collapse = ","))
  expect_error(read_contributors_file("x", file_data = fd))

  # Catches invalid emails
  dat <- c("Ben", "BL", "bbl.com", "0000-0000-0000-0000", "role")
  fd <- c(paste(labels, collapse = ","), paste(dat, collapse = ","))
  expect_error(read_contributors_file("x", file_data = fd))
  dat <- c("Ben", "BL", "ben@", "0000-0000-0000-0000", "role")
  fd <- c(paste(labels, collapse = ","), paste(dat, collapse = ","))
  expect_error(read_contributors_file("x", file_data = fd))
  dat <- c("Ben", "BL", "", "0000-0000-0000-0000", "role")
  fd <- c(paste(labels, collapse = ","), paste(dat, collapse = ","))
  expect_error(read_contributors_file("x", file_data = fd))
  # Handles multiple (semicolon) emails
  dat <- c("Ben", "BL", "ben@bbl.com; ben@gmail.com", "0000-0000-0000-0000", "role")
  fd <- c(paste(labels, collapse = ","), paste(dat, collapse = ","))
  expect_silent(read_contributors_file("x", file_data = fd))

  # Catches invalid ORCIDs
  dat <- c("Ben", "BL", "ben@bbl.com", "0000-000-0000-0000", "role")
  fd <- c(paste(labels, collapse = ","), paste(dat, collapse = ","))
  expect_error(read_contributors_file("x", file_data = fd))
  dat <- c("Ben", "BL", "ben@bbl.com", "0000-000X-0000-0000", "role")
  fd <- c(paste(labels, collapse = ","), paste(dat, collapse = ","))
  expect_error(read_contributors_file("x", file_data = fd))
  # Valid forms
  dat <- c("Ben", "BL", "ben@bbl.com", "0000-0000-0000-0000", "role")
  fd <- c(paste(labels, collapse = ","), paste(dat, collapse = ","))
  expect_silent(read_contributors_file("x", file_data = fd))
  dat <- c("Ben", "BL", "ben@bbl.com", "0000-0000-0000-000X", "role")
  fd <- c(paste(labels, collapse = ","), paste(dat, collapse = ","))
  expect_silent(read_contributors_file("x", file_data = fd))
})

test_that("read_ports_file", {
  # ports file
  labels <- c("CSR_PORT", "CSR_MSMT_VAR", "CSR_TREATMENT")
  dat <- c("0", "Rs", "None")
  fd <- c(paste(labels, collapse = ","), paste(dat, collapse = ","))

  x <- read_ports_file("x", file_data = fd)
  expect_is(x, "data.frame")
  expect_equivalent(nrow(x), 1)
  expect_true(all(labels %in% names(x)))

  # Catches missing required variable
  for(i in seq_along(labels)) {
    fd <- c(paste(labels[-i], collapse = ","), paste(dat[-i], collapse = ","))
    expect_error(read_ports_file("x", file_data = fd), info = labels[i])
  }

  # Illegal measurement variable
  labels <- c("CSR_PORT", "CSR_MSMT_VAR", "CSR_TREATMENT")
  dat <- c("0", "Rz", "None")
  fd <- c(paste(labels, collapse = ","), paste(dat, collapse = ","))
  expect_error(read_ports_file("x", file_data = fd))
})

test_that("map_columns", {
  # handles missing input
  expect_null(map_columns(NULL, data.frame()))
  expect_error(map_columns(data.frame(), NULL)) # not a data frame
  expect_error(map_columns(data.frame(), data.frame())) # not correct columns

  # renames columns
  dat <- data.frame(x = 1:2, y = 2:3)
  columns <- data.frame(Database = "z", Dataset = "x", stringsAsFactors = FALSE)
  res <- map_columns(dat, columns)
  expect_identical(res, data.frame(z = 1:2, y = 2:3))

  # handles factors
  columns <- data.frame(Database = "z", Dataset = "x")
  expect_identical(map_columns(dat, columns), res)

  # computes on columns
  columns <- data.frame(Database = "z", Dataset = "x", Computation = "x * 2", stringsAsFactors = FALSE)
  expect_identical(map_columns(dat, columns), data.frame(y = 2:3, z = c(2, 4)))

  # errors on nonexistent or duplicated columns
  columns <- data.frame(Database = "z", Dataset = "q")
  expect_error(map_columns(dat, columns))
  columns <- data.frame(Database = "x", Dataset = "x")
  expect_error(map_columns(dat, columns))
})
