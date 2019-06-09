# File parsing routines (internal)

#' Parse labels from information files
#'
#' @param file_data Character vector of file lines
#' @param line_label Character label
#' @param numeric_data Numeric data? Logical
#' @param sep Separator
#' @note If label is not found, produces an error (if label was required) or returns
#' \code{NA} (if not).
#' @keywords internal
#' @return Value (character) of data opposite label
extract_line <- function(file_data, line_label,
                         required = TRUE, sep = ":", numeric_data = FALSE) {
  rx <- paste0("^", line_label, sep)
  fd <- file_data[grep(rx, file_data)]
  if(length(fd) > 1) {
    #  browser()
    stop(length(fd), " entries found for required label ", line_label)
  }
  if(required & length(fd) == 0) {
    stop("No entries found for required label ", line_label)
  }
  if(!required & length(fd) == 0) {
    return(NA_character_)
  }

  d <- trimws(gsub(rx, "", fd))
  if(length(d) == 0) d <- NA_character_

  if(numeric_data) {
    dn <- suppressWarnings(as.numeric(d))
    if(d != "" & is.na(dn)) {
      stop(d, " could not be converted to numeric for ", line_label)
    }
    dn
  } else {
    d
  }
}


#' List available datasets
#'
#' @param path Path name
#' @return Character vector of datasets.
#' @export
#' @examples
#' list_datasets()
list_datasets <- function(path = resolve_dataset("")) {
  ds <- list.files(path)
  ds[grep("^d[0-9]{8}_", ds)]  # dataset folders start with "d" followed by eight numbers
}

#' Get the full path of a dataset folder(s)
#'
#' @param dataset_name Dataset name(s), character
#' @export
#' @return Fully-qualified filename(s) of dataset folder in \code{inst/extdata}
#'  (\code{extdata/} in built package).
resolve_dataset <- function(dataset_name) {
  system.file(file.path("extdata", dataset_name), package = "cosore", mustWork = TRUE)
}


#' Read a text file and strip out comments
#'
#' @param dataset_name Dataset name, character
#' @param file_name File name, character
#' @param file_data File data, character vector; optional for testing
#' @param comment_char Start-of-line comment character
#' @keywords internal
#' @return Contents of file in a character vector.
read_file <- function(dataset_name, file_name, file_data = NULL, comment_char = "#") {
  if(is.null(file_data)) {
    file_data <- readLines(file.path(resolve_dataset(dataset_name), file_name))
  }
  file_data[grep(paste0("^", comment_char), file_data, invert = TRUE)]
}


#' Read dataset DESCRIPTION file
#'
#' @param dataset_name Dataset name, character
#' @param file_data File data, character vector; optional for testing
#' @keywords internal
#' @importFrom tibble tibble
#' @return A \code{data.frame} with the following columns:
#' \item{Dataset}{Dataset name, character}
#' \item{Site_name}{Site name, character}
#' \item{Longitude}{Decimal Longitude, numeric}
#' \item{Latitude}{Decimal latitude, numeric}
#' \item{Instrument}{Instrument type, character}
#' \item{Ecosystem_type}{Ecosystem type, character}
#' \item{Primary_pub}{Primary publication, character}
#' \item{Other_pubs}{Other publications, character}
read_description_file <- function(dataset_name, file_data = NULL) {
  file_data <- read_file(dataset_name, "DESCRIPTION.txt", file_data = file_data)

  d <- tibble(Site_name = extract_line(file_data, "Site_name"),
              Longitude = extract_line(file_data, "Longitude", numeric_data = TRUE),
              Latitude = extract_line(file_data, "Latitude", numeric_data = TRUE),
              Elevation = extract_line(file_data, "Elevation", numeric_data = TRUE),
              UTC_offset = extract_line(file_data, "UTC_offset", numeric_data = TRUE),
              Timezone = extract_line(file_data, "Timezone"),
              IGBP = extract_line(file_data, "IGBP"),
              Network = extract_line(file_data, "Network", required = FALSE),
              Site_ID = extract_line(file_data, "Site_ID", required = FALSE),
              Instrument = extract_line(file_data, "Instrument"),
              File_format = extract_line(file_data, "File_format"),
              Timestamp_format = extract_line(file_data, "Timestamp_format"),
              Timestamp_timezone = extract_line(file_data, "Timestamp_timezone"),
              Primary_pub = extract_line(file_data, "Primary_pub", required = FALSE),
              Other_pubs = extract_line(file_data, "Other_pub", required = FALSE),
              Data_URL = extract_line(file_data, "Data_URL", required = FALSE),
              Acknowledgment = extract_line(file_data, "Acknowledgment", required = FALSE),
              CSR_DATASET = dataset_name)

  if(is.na(d$UTC_offset) | d$UTC_offset == "" | abs(d$UTC_offset) >= 15) {
    stop("Bad UTC_offset in ", dataset_name)
  }
  d
}


#' Read dataset CONTRIBUTORS file
#'
#' @param dataset_name Dataset name, character
#' @param file_data File data, character vector; optional for testing
#' @keywords internal
#' @note For information about ORCID see \url{https://orcid.org}. For
#' CRediT roles, see \url{https://www.casrai.org/credit.html}.
#' @return A \code{data.frame} with the following columns:
#' \item{First_name}{Site name, character}
#' \item{Family_name}{Decimal Longitude, numeric}
#' \item{Email}{Decimal latitude, numeric}
#' \item{ORCID}{Instrument type, character}
#' \item{Role}{Role, character}
read_contributors_file <- function(dataset_name, file_data = NULL) {
  file_data <- read_file(dataset_name, "CONTRIBUTORS.txt", file_data)
  read_csv_data(file_data, required = c("First_name", "Family_name"))
}


#' Read comma-separated data from a character vector
#'
#' @param file_data File data to read, character vector
#' @param required Vector of column names that must be all filled in, optional
#' @return A data frame with loaded data.
#' @keywords internal
read_csv_data <- function(file_data, required = NULL) {
  x <- read.csv(textConnection(file_data), strip.white = TRUE, stringsAsFactors = FALSE)
  for(req in required) {
    if(!req %in% colnames(x)) {
      stop(req, " is not a column name")
    }
    empty <- which(is.na(x[[req]]) | x[[req]] == "")
    if(length(empty)) {
      stop("Column ", req, " is required but has empty entries: ", empty)
    }
  }
  tibble::as_tibble(x)
}


#' Read dataset PORTS file
#'
#' @param dataset_name Dataset name, character
#' @param file_data File data, character vector; optional for testing
#' @keywords internal
#' @return A \code{data.frame} with the following columns:
#' \item{Port}{Port number, numeric; 0 = all ports}
#' \item{Treatment}{Treatment, character; by default "None"}
#' \item{Species}{Species, character}
read_ports_file <- function(dataset_name, file_data = NULL) {
  file_data <- read_file(dataset_name, "PORTS.txt", file_data)
  read_csv_data(file_data, required = c("Port", "Treatment"))
}


#' Read dataset COLUMNS file
#'
#' @param dataset_name Dataset name, character
#' @param file_data File data, character vector; optional for testing
#' @keywords internal
#' @return A \code{data.frame} with the following columns:
#' \item{Database}{Database column name, character}
#' \item{Dataset}{Dataset column name, character}
#' \item{Computation}{Optional computation R-parseable to perform, character}
#' \item{Port}{Optional port number, integer}
#' \item{Notes}{Optional notes, character}
read_columns_file <- function(dataset_name, file_data = NULL) {
  file_data <- read_file(dataset_name, "COLUMNS.txt", file_data)
  read_csv_data(file_data, required = c("Database", "Dataset"))
}

#' Read dataset ANCILLARY file
#'
#' @param dataset_name Dataset name, character
#' @param file_data File data, character vector; optional for testing
#' @keywords internal
#' @importFrom utils read.csv
#' @note This is simply a comma-separated table.
#' @return A \code{data.frame} containing any data in the file.
read_ancillary_file <- function(dataset_name, file_data = NULL) {
  file_data <- read_file(dataset_name, "ANCILLARY.txt", file_data)
  read_csv_data(file_data)
}


#' Map data columns to new names/values.
#'
#' @param dat Dataset data, a \code{data.frame}
#' @param columns Column mapping data from the \code{COLUMNS.txt} file, a \code{data.frame}
#' @return The \code{dat} data frame with column names transformed, and possibly
#' computed, as defined by \code{columns}.
#' @export
#' @examples
#' dat <- data.frame(x = 1:3)
#' columns <- data.frame(Database = "y", Dataset = "x", Computation = "x * 2")
#' map_columns(dat, columns)  # produces a data.frame(y = c(2, 4, 6))
map_columns <- function(dat, columns) {
  if(!is.data.frame(dat)) return(NULL)

  stopifnot(is.data.frame(columns))
  stopifnot(all(c("Database", "Dataset") %in% names(columns)))

  if(!"Computation" %in% names(columns)) {
    columns$Computation <- NA_character_
  }

  # As usual, factors screw things up, so make sure not dealing with them
  columns$Database <- as.character(columns$Database)
  columns$Dataset <- as.character(columns$Dataset)
  columns$Computation <- as.character(columns$Computation)

  for(col in seq_len(nrow(columns))) {
    dbcol <- columns$Database[col]
    dscol <- columns$Dataset[col]
    comp <- columns$Computation[col]

    # Apply map/computation
    if(!dscol %in% names(dat)) {
      stop("Column ", dscol, " not found in data")
    }
    stopifnot(dscol != dbcol)
    if(is.na(comp) | comp == "") {
      message(dbcol, " <- ", dscol)
      names(dat)[which(names(dat) == dscol)] <- dbcol  # rename
    } else {
      message(dbcol, " <- ", comp)
      dat[[dbcol]] <- with(dat, eval(parse(text = comp)))
      dat[[dscol]] <- NULL  # remove original column
    }
  }

  dat
}

#' Read a complete dataset
#'
#' @param dataset_name Dataset name, character
#' @param raw_data Path to the raw data folder (not in package)
#' @param log Log messages? Logical
#' @return A list with (at least) elements:
#' \item{description}{Contents of \code{DESCRIPTION.txt} file}
#' \item{contributors}{Contents of \code{CONTRIBUTORS.txt} file}
#' \item{ports}{Contents of \code{PORTS.txt} file}
#' \item{data}{Continuous soil respiration data, parsed into a \code{data.frame}}
#' \item{ancillary}{Ancillary site information}
#' @export
#' @examples
#' read_dataset("TEST_licordata")
read_dataset <- function(dataset_name, raw_data, log = TRUE) {

  dataset <- list(dataset_name = dataset_name,
                  description = read_description_file(dataset_name),
                  contributors = read_contributors_file(dataset_name),
                  ports = read_ports_file(dataset_name),
                  columns = read_columns_file(dataset_name),
                  ancillary = read_ancillary_file(dataset_name)
  )

  # Parse the actual data
  # Test data live inside the package, in a data/ subdirectory, but generally
  # we look in raw_data/ which is supplied by the user (external to the package)
  if(missing(raw_data)) {
    df <- file.path(resolve_dataset(dataset_name), "data")
  } else {
    df <- file.path(raw_data, dataset_name)
  }

  if(log) {
    # tf <- tempfile()
    # zz <- file(tf, open = "wt")
    # sink(zz, type = "output")
    # sink(zz, type = "message")
  }

  # Processing statistics table
  diag <- tibble(CSR_RECORDS = 0,
                 CSR_COLUMNS_DROPPED = "",
                 CSR_RECORDS_REMOVED_NA = 0,
                 CSR_RECORDS_REMOVED_ERR = 0,
                 CSR_RECORDS_REMOVED_TOOLOW = 0,
                 CSR_RECORDS_REMOVED_TOOHIGH = 0,
                 CSR_FLUX_LOWBOUND = NA,
                 CSR_FLUX_HIGHBOUND = NA,
                 CSR_BAD_TCHAMBER = 0,
                 CSR_RECORDS_REMOVED_TIMESTAMP = 0)

  dsd <- NULL  # dataset data

  if(!dir.exists(df)) {
    warning("No data folder found for ", dataset_name)
  } else {
    # Dispatch to correct parsing function based on file format
    ff <- toupper(dataset$description$File_format)
    if(ff == "CUSTOM") {
      ff <- dataset_name   # if "Custom" that means there's custom code for this dataset
    }
    func <- paste("parse", ff, sep = "_")
    if(exists(func)) {
      dsd <- do.call(func, list(df))
    } else {
      warning("Unknown format ", ff, " in ", dataset_name)
    }
  }

  if(!is.null(dsd)) {
    dsd <- tibble::as_tibble(dsd)
    # Column mapping and computation
    dsd <- map_columns(dsd, dataset$columns)

    # Change the timestamp column to a datetime object
    original_ts <- dsd$CSR_TIMESTAMP
    dsd$CSR_TIMESTAMP <- as.POSIXct(dsd$CSR_TIMESTAMP,
                                    format = dataset$description$Timestamp_format,
                                    tz = dataset$description$Timestamp_timezone)
    nats <- is.na(dsd$CSR_TIMESTAMP) & !is.na(original_ts)
    diag$CSR_RECORDS_REMOVED_TIMESTAMP <- sum(nats)
    dsd <- dsd[!nats,]

    if(nrow(dsd) == 0) {
      stop("Timestamps could not be parsed with ", dataset$description$Timestamp_format,
           " and tz ", dataset$description$Timestamp_timezone)
    }

    # Drop any unmapped columns
    drops <- grep("^CSR_", names(dsd), invert = TRUE)
    diag$CSR_COLUMNS_DROPPED <- paste(names(dsd)[drops], collapse = ", ")
    dsd[drops] <- NULL

    # Add port column if necessary
    if(!"CSR_PORT" %in% names(dsd) & nrow(dsd)) {
      dsd$CSR_PORT <- 0
    }

    # Remove NA flux records
    na_flux <- is.na(dsd$CSR_FLUX)
    diag$CSR_RECORDS_REMOVED_NA <- sum(na_flux)
    dsd <- dsd[!na_flux,]

    # Remove error records
    if("CSR_ERROR" %in% names(dsd)) {
      err <- dsd$CSR_ERROR
      diag$CSR_RECORDS_REMOVED_ERR <- sum(err)
      dsd <- dsd[!err,]
      dsd$CSR_ERROR <- NULL
    }

    # Remove records with flux data way out of anything possible
    fl <- c(-1, 50)   # flux limits
    diag$CSR_FLUX_LOWBOUND <- min(fl)
    diag$CSR_FLUX_HIGHBOUND <- max(fl)
    toolow <- dsd$CSR_FLUX < min(fl)
    diag$CSR_RECORDS_REMOVED_TOOLOW <- sum(toolow, na.rm = TRUE)
    toohigh <- dsd$CSR_FLUX > max(fl)
    diag$CSR_RECORDS_REMOVED_TOOHIGH <- sum(toohigh, na.rm = TRUE)
    dsd <- dsd[!toolow & !toohigh,]
  }

  # Remove bad chamber temperature values
  if("CSR_TCHAMBER" %in% names(dsd)) {
    tl <- c(-50, 100)  # temperature limits
    bad_temps <- dsd$CSR_TCHAMBER < min(tl) | dsd$CSR_TCHAMBER > max(tl)
    dsd$CSR_TCHAMBER[bad_temps] <- NA
    diag$CSR_BAD_TCHAMBER <- sum(bad_temps, na.rm = TRUE)
  }

  # Add new tables to the dataset structure and return
  diag$Records <- nrow(dsd)
  dataset$diagnostics <- diag
  dataset$data <- dsd

  if(log) {
    # sink(type = "message")
    # sink(type = "output")
    # dataset$log <- readLines(tf)
    # unlink(tf)
  }

  dataset
}
