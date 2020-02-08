# test_parse_processed.R

context("parse-processed")

create_test_files <- function(td, nfiles, sep, extension) {
  for(i in seq_len(nfiles)) {
    df <- data.frame(x = i, row.names = FALSE)
    fqfn <- file.path(td, paste0("test", i, ".txt"))
    write.table(df, file = fqfn, sep = sep, col.names = TRUE, row.names = FALSE)
  }
}

test_that("parse_PROCESSED", {
  # Create a test directory
  td <- file.path(tempdir(), runif(1))
  dir.create(td)
  nfiles <- 3

  # whitespace
  fnames <- create_test_files(td, nfiles = nfiles, sep = " ", extension = ".txt")
  x <- parse_PROCESSED(td)
  expect_s3_class(x, "data.frame")
  expect_identical(x$x, seq_len(nfiles))
  unlink(fnames)
  unlink(td)
})

test_that("parse_PROCESSED_CSV", {
  # Create a test directory
  td <- file.path(tempdir(), runif(1))
  dir.create(td)
  nfiles <- 3

  # csv
  fnames <- create_test_files(td, nfiles = nfiles, sep = ",", extension = ".csv")
  x <- parse_PROCESSED(td)
  expect_s3_class(x, "data.frame")
  expect_identical(x$x, seq_len(nfiles))
  unlink(fnames)
  unlink(td)
})
