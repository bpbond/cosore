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


#' List datasets available to build
#'
#' @param path Path name
#' @return Character vector of datasets.
#' @details This returns a vector of metadata folder names available in
#' the package's \code{inst/exdata} folder. The \code{\link{csr_build}}
#' function uses these names, in combination with a user-specified raw
#' data folder, to build the database.
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
#' @keywords internal
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
#' \item{CSR_DATASET}{Dataset name (internal to COSORE), character}
#' \item{CSR_SITE_NAME}{Site name, character}
#' \item{CSR_LONGITUDE}{Decimal longitude (degrees), numeric}
#' \item{CSR_LATITUDE}{Decimal latitude (degrees), numeric}
#' \item{CSR_ELEVATION}{Elevation (m), numeric}
#' \item{CSR_TIMEZONE}{Timezone, character}
#' \item{CSR_IGBP}{IGBP class, character}
#' \item{CSR_NETWORK}{Site network name, character}
#' \item{CSR_SITE_ID}{Site ID in network, character}
#' \item{CSR_INSTRUMENT}{Measurement instrument, character}
#' \item{CSR_FILE_FORMAT}{Data file format, character}
#' \item{CSR_TIMESTAMP_FORMAT}{Data timestamp format (see \code{\link{strptime}}), character}
#' \item{CSR_TIMESTAMP_TZ}{Data timestamp timezone, character}
#' \item{CSR_PRIMARY_PUB}{Primary publication, character}
#' \item{CSR_OTHER_PUBS}{Other publications, character}
#' \item{CSR_DATA_URL}{Data URL or DOI, character}
#' \item{CSR_ACKNOWLEDGMENT}{Acknowledgment text, character}
read_description_file <- function(dataset_name, file_data = NULL) {
  f <- read_file(dataset_name, "DESCRIPTION.txt", file_data = file_data)

  tibble(CSR_DATASET = dataset_name,
         CSR_SITE_NAME = extract_line(f, "CSR_SITE_NAME"),
         CSR_LONGITUDE = extract_line(f, "CSR_LONGITUDE", numeric_data = TRUE),
         CSR_LATITUDE = extract_line(f, "CSR_LATITUDE", numeric_data = TRUE),
         CSR_ELEVATION = extract_line(f, "CSR_ELEVATION", numeric_data = TRUE),
         CSR_TIMEZONE = extract_line(f, "CSR_TIMEZONE"),
         CSR_IGBP = extract_line(f, "CSR_IGBP"),
         CSR_NETWORK = extract_line(f, "CSR_NETWORK", required = FALSE),
         CSR_SITE_ID = extract_line(f, "CSR_SITE_ID", required = FALSE),
         CSR_INSTRUMENT = extract_line(f, "CSR_INSTRUMENT"),
         CSR_FILE_FORMAT = extract_line(f, "CSR_FILE_FORMAT"),
         CSR_TIMESTAMP_FORMAT = extract_line(f, "CSR_TIMESTAMP_FORMAT"),
         CSR_TIMESTAMP_TZ = extract_line(f, "CSR_TIMESTAMP_TZ"),
         CSR_PRIMARY_PUB = extract_line(f, "CSR_PRIMARY_PUB", required = FALSE),
         CSR_OTHER_PUBS = extract_line(f, "CSR_OTHER_PUBS", required = FALSE),
         CSR_DATA_URL = extract_line(f, "CSR_DATA_URL", required = FALSE),
         CSR_ACKNOWLEDGMENT = extract_line(f, "CSR_ACKNOWLEDGMENT", required = FALSE),
         CSR_NOTES = extract_line(f, "CSR_NOTES", required = FALSE),
         CSR_EMBARGO = extract_line(f, "CSR_EMBARGO", required = FALSE))
}


#' Read dataset CONTRIBUTORS file
#'
#' @param dataset_name Dataset name, character
#' @param file_data File data, character vector; optional for testing
#' @keywords internal
#' @note For information about ORCID see \url{https://orcid.org}. For
#' CRediT roles, see \url{https://www.casrai.org/credit.html}.
#' @return A \code{data.frame} with the following columns:
#' \item{CSR_FIRST_NAME}{First (personal) name, character}
#' \item{CSR_FAMILY_NAME}{Family name, character}
#' \item{CSR_EMAIL}{Email address, character}
#' \item{CSR_ORCID}{ORCID identifier, character}
#' \item{CSR_ROLE}{CRediT role, character}
read_contributors_file <- function(dataset_name, file_data = NULL) {
  file_data <- read_file(dataset_name, "CONTRIBUTORS.txt", file_data)
  cfd <- read_csv_data(file_data, required = c("CSR_FIRST_NAME", "CSR_FAMILY_NAME"))

  # Have to provide at least one contributor
  if(cfd$CSR_FIRST_NAME[1] == "" | cfd$CSR_FAMILY_NAME[1] == "" | cfd$CSR_EMAIL[1] == "") {
    stop("Name and/or email for primary contributor is blank")
  }
  # Check for invalid email addresses
  invalid_emails <- grep("^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}$",
                         cfd$CSR_EMAIL, ignore.case = TRUE, invert = TRUE)
  if(length(invalid_emails) && any(cfd$CSR_EMAIL[invalid_emails] != "")) {
    stop("Invalid emails for contributors ", invalid_emails)
  }

  cfd
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
#' \item{CSR_PORT}{Port number, numeric; 0 = all ports}
#' \item{CSR_TREATMENT}{Treatment, character; by default "None"}
#' \item{CSR_SPECIES}{Species, character}
#' \item{CSR_DEPTH}{Depth of collar, cm}
#' \item{CSR_AREA}{Ground area of chamber, cm2}
read_ports_file <- function(dataset_name, file_data = NULL) {
  file_data <- read_file(dataset_name, "PORTS.txt", file_data)
  read_csv_data(file_data, required = c("CSR_PORT", "CSR_TREATMENT"))
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
#' @keywords internal
#' @examples
#' \dontrun{
#' dat <- data.frame(x = 1:3)
#' columns <- data.frame(Database = "y", Dataset = "x", Computation = "x * 2")
#' map_columns(dat, columns)  # produces a data.frame(y = c(2, 4, 6))
#' }
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

#' Read a complete dataset from raw files
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
#' @importFrom utils head
#' @examples
#' read_dataset("TEST_licordata")
read_dataset <- function(dataset_name, raw_data, log = TRUE) {

  dataset <- list(description = read_description_file(dataset_name),
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
                 CSR_BAD_TEMPERATURE = 0,
                 CSR_RECORDS_REMOVED_TIMESTAMP = 0,
                 CSR_EXAMPLE_BAD_TIMESTAMPS = "")

  dsd <- NULL  # dataset data

  if(!dir.exists(df)) {
    warning("No data folder found for ", dataset_name)
  } else {
    # Dispatch to correct parsing function based on file format
    ff <- toupper(dataset$description$CSR_FILE_FORMAT)
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

    # Change the timestamp column to a datetime object...
    original_ts <- dsd$CSR_TIMESTAMP
    dsd$CSR_TIMESTAMP <- as.POSIXct(dsd$CSR_TIMESTAMP,
                                    format = dataset$description$CSR_TIMESTAMP_FORMAT,
                                    tz = dataset$description$CSR_TIMESTAMP_TZ)
    nats <- is.na(dsd$CSR_TIMESTAMP) & !is.na(original_ts)
    diag$CSR_RECORDS_REMOVED_TIMESTAMP <- sum(nats)
    diag$CSR_EXAMPLE_BAD_TIMESTAMPS <- paste(head(original_ts[nats]), collapse = ", ")
    dsd <- dsd[!nats,]

    if(nrow(dsd) == 0) {
      stop("Timestamps could not be parsed with ",
           dataset$description$CSR_TIMESTAMP_FORMAT,
           " and tz ", dataset$description$CSR_TIMESTAMP_TZ)
    }

    # ...and to the site's timezone
    # This attribute-changing makes me nervous, but apparently it's the
    # only way to change timezone without either using lubridate::with_tz(),
    # or using format() to a string and then casting back
    attr(dsd$CSR_TIMESTAMP, "tzone") <- dataset$DESCRIPTION$CSR_TIMEZONE

    # Drop any unmapped columns
    drops <- grep("^CSR_", names(dsd), invert = TRUE)
    diag$CSR_COLUMNS_DROPPED <- paste(names(dsd)[drops], collapse = ", ")
    dsd[drops] <- NULL

    # Add port column if necessary
    if(!"CSR_PORT" %in% names(dsd) & nrow(dsd)) {
      dsd$CSR_PORT <- 0
    }
    dsd$CSR_PORT <- as.numeric(dsd$CSR_PORT)

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

    diag$CSR_RECORDS <- nrow(dsd)
  }

  # Remove bad temperature values
  tl <- c(-50, 60)  # temperature limits
  for(tmp in c("CSR_TCHAMBER", "CSR_T5")) {
    if(tmp %in% names(dsd) && nrow(dsd)) {
      dsd[,tmp] <- as.numeric(unlist(dsd[tmp])) # ensure numeric
      tmpvals <- dsd[tmp]
      bad_temps <- tmpvals < min(tl) | tmpvals > max(tl)
      bad_temps[is.na(bad_temps)] <- FALSE
      dsd[bad_temps, tmp] <- NA  # NA out bad values
      diag$CSR_BAD_TEMPERATURE <- diag$CSR_BAD_TEMPERATURE +
        sum(bad_temps, na.rm = TRUE)
    }
  }

  # Add new tables to the dataset structure and return
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
