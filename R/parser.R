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
  if(nchar(d) == 0) d <- NA_character_
  if(required & is.na(d)) {
    stop("Required label ", line_label, " has a blank entry")
  }

  if(numeric_data) {
    dn <- suppressWarnings(as.numeric(d))
    if(required & is.na(dn)) {
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
  # dataset folders start with "d" followed by eight numbers (as opposed to test
  # datasets we don't want to return to the user)
  ds[grep("^d[0-9]{8}_", ds)]
}

#' Get the full path of a dataset folder(s)
#'
#' @param dataset_name Dataset name(s), character
#' @keywords internal
#' @return Fully-qualified filename(s) of dataset folder in \code{inst/extdata/datasets}
#'  (\code{extdata/datasets/} in built package).
resolve_dataset <- function(dataset_name) {
  system.file(file.path("extdata/datasets", dataset_name), package = "cosore", mustWork = TRUE)
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
#' \item{CSR_MSMT_LENGTH}{Measurement legnth (s), numeric}
#' \item{CSR_FILE_FORMAT}{Data file format, character}
#' \item{CSR_TIMESTAMP_FORMAT}{Data timestamp format (see \code{\link{strptime}}), character}
#' \item{CSR_TIMESTAMP_TZ}{Data timestamp timezone, character}
#' \item{CSR_PRIMARY_PUB}{Primary publication, character}
#' \item{CSR_OTHER_PUBS}{Other publications, character}
#' \item{CSR_DATA_URL}{Data URL or DOI, character}
#' \item{CSR_ACKNOWLEDGMENT}{Acknowledgment text, character}
#' Infrequently, the \code{description} object may include:
#' \item{CSR_NOTES}{Notes, character}
#' \item{CSR_EMBARGO}{Embargo information, character}
read_description_file <- function(dataset_name, file_data = NULL) {
  f <- read_file(dataset_name, "DESCRIPTION.txt", file_data = file_data)

  x <- tibble(CSR_DATASET = dataset_name,
              CSR_SITE_NAME = extract_line(f, "CSR_SITE_NAME"),
              CSR_LONGITUDE = extract_line(f, "CSR_LONGITUDE", numeric_data = TRUE),
              CSR_LATITUDE = extract_line(f, "CSR_LATITUDE", numeric_data = TRUE),
              CSR_ELEVATION = extract_line(f, "CSR_ELEVATION", numeric_data = TRUE, required = FALSE),
              CSR_TIMEZONE = extract_line(f, "CSR_TIMEZONE"),
              CSR_IGBP = extract_line(f, "CSR_IGBP"),
              CSR_NETWORK = extract_line(f, "CSR_NETWORK", required = FALSE),
              CSR_SITE_ID = extract_line(f, "CSR_SITE_ID", required = FALSE),
              CSR_INSTRUMENT = extract_line(f, "CSR_INSTRUMENT"),
              CSR_MSMT_LENGTH = extract_line(f, "CSR_MSMT_LENGTH", numeric_data = TRUE, required = FALSE),
              CSR_FILE_FORMAT = extract_line(f, "CSR_FILE_FORMAT"),
              CSR_TIMESTAMP_FORMAT = extract_line(f, "CSR_TIMESTAMP_FORMAT"),
              CSR_TIMESTAMP_TZ = extract_line(f, "CSR_TIMESTAMP_TZ"),
              CSR_PRIMARY_PUB = extract_line(f, "CSR_PRIMARY_PUB", required = FALSE),
              CSR_OTHER_PUBS = extract_line(f, "CSR_OTHER_PUBS", required = FALSE),
              CSR_DATA_URL = extract_line(f, "CSR_DATA_URL", required = FALSE),
              CSR_ACKNOWLEDGMENT = extract_line(f, "CSR_ACKNOWLEDGMENT", required = FALSE),
              CSR_NOTES = extract_line(f, "CSR_NOTES", required = FALSE),
              CSR_EMBARGO = extract_line(f, "CSR_EMBARGO", required = FALSE))

  if(!x$CSR_IGBP %in% c("Wetland",
                        "Broadleaf evergreen forest",
                        "Evergreen needleleaf forest",
                        "Evergreen needleleaf plantation", # TODO
                        "Deciduous broadleaf forest", "Open shrubland",
                        "Closed shrubland",
                        "Evergreen broadleaf forest", "Mixed forests", "Woody savanna",
                        "Grassland", "Cropland",
                        "Poplar short rotation coppice plantation",  # TODO fix this
                        "Savannas", "Desert woodland")) {
    stop("Unknown IGBP: ", x$CSR_IGBP)
  }
  x
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

  # Have to provide first contributor email
  if(is.na(cfd$CSR_EMAIL[1])) {
    stop(dataset_name, ": email for primary contributor is missing")
  }
  # Check for invalid email addresses
  eml <- sapply(strsplit(cfd$CSR_EMAIL, ";"), function(x) x[1])
  invalid_emails <- grep("^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}$",
                         eml, ignore.case = TRUE, invert = TRUE)
  if(length(invalid_emails) && any(cfd$CSR_EMAIL[invalid_emails] != "")) {
    stop(dataset_name, ": invalid emails for contributors ", invalid_emails)
  }
  # Check for invalid ORCID ID
  invalid_orcids <- grep("^[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]{1}$",
                         cfd$CSR_ORCID, ignore.case = TRUE, invert = TRUE)
  entries <- !is.na(cfd$CSR_ORCID) & cfd$CSR_ORCID != ""
  if(length(invalid_orcids) && any(entries[invalid_orcids])) {
    stop(dataset_name, ": invalid ORCID IDs for contributors ", invalid_orcids)
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
      stop("Required column ", req, " not found: ", paste(colnames(x), collapse = ", "))
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
#' \item{CSR_MSMT_VAR}{Measurement variable, "Rs" (soil respiration), "Rh" (heterotrophic respiration),
#' or "NEE" (net ecosystem exchange from a clear chamber)}
#' \item{CSR_TREATMENT}{Treatment, character; by default "None"}
#' \item{CSR_SPECIES}{Species, character}
#' \item{CSR_DEPTH}{Depth of collar, cm}
#' \item{CSR_AREA}{Ground area of chamber, cm2}
read_ports_file <- function(dataset_name, file_data = NULL) {
  file_data <- read_file(dataset_name, "PORTS.txt", file_data)
  pfd <- read_csv_data(file_data, required = c("CSR_PORT", "CSR_MSMT_VAR", "CSR_TREATMENT"))

  # Measurement variable is highly standardized; make sure all ok
  ok <- pfd$CSR_MSMT_VAR %in% c("Rs", "Rh", "Reco", "NEE")
  if(!all(ok)) {
    stop(dataset_name, ": illegal CSR_MSMT_VAR entry in PORTS file: ", paste(pfd$CSR_MSMT_VAR[!ok], collapse = ", "))
  }
  pfd
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
#' dat <- data.frame(x = 1:3)
#' columns <- data.frame(Database = "y", Dataset = "x", Computation = "x * 2")
#' cosore:::map_columns(dat, columns)  # produces a data.frame(y = c(2, 4, 6))
map_columns <- function(dat, columns) {
  if(!is.data.frame(dat)) return(NULL)

  stopifnot(is.data.frame(columns))
  stopifnot(all(c("Database", "Dataset") %in% names(columns)))
  stopifnot(!any(duplicated(columns$Database)))

  if(!"Computation" %in% names(columns)) {
    columns$Computation <- NA_character_
  }

  # As usual, factors screw things up, so make sure not dealing with them
  columns$Database <- as.character(columns$Database)
  columns$Dataset <- as.character(columns$Dataset)
  columns$Computation <- as.character(columns$Computation)

  newdat <- tibble(.rows = nrow(dat))

  for(col in seq_len(nrow(columns))) {
    dbcol <- columns$Database[col]
    dscol <- columns$Dataset[col]
    comp <- columns$Computation[col]

    # Apply map/computation
    if(!dscol %in% names(dat)) {
      stop("Column ", dscol, " not found in data, which has columns ",
           paste(names(dat), collapse = ","))
    }
    if(is.na(comp) | comp == "") {
      message("\t", dbcol, " <- ", dscol)
      newdat[[dbcol]] <- dat[[dscol]]
    } else {
      message("\t", dbcol, " <- ", comp)
      newdat[[dbcol]] <- with(dat, eval(parse(text = comp)))
    }
  }

  newdat
}

#' Read a complete dataset from raw files
#'
#' @param dataset_name Dataset name, character
#' @param raw_data Path to the raw data folder (not in package)
#' @param dataset The dataset (metadata only when called)
#' @importFrom utils head
#' @keywords internal
#' @return A list with (at least) elements:
#' \item{data}{Continuous soil respiration data, parsed into a \code{data.frame}}
#' \item{diagnostics}{Diagnostics on the data parsing and QC process}
#' @note This is normally called only from \code{\link{read_dataset}}.
read_raw_dataset <- function(dataset_name, raw_data, dataset) {
  df <- file.path(raw_data, dataset_name)

  # Processing statistics table
  diag <- tibble(CSR_RECORDS = 0,
                 CSR_RECORDS_REMOVED_NA = 0,
                 CSR_RECORDS_REMOVED_ERR = 0,
                 CSR_RECORDS_REMOVED_TOOLOW = 0,
                 CSR_RECORDS_REMOVED_TOOHIGH = 0,
                 CSR_FLUX_LOWBOUND = NA,
                 CSR_FLUX_HIGHBOUND = NA,
                 CSR_BAD_TEMPERATURE = 0,
                 CSR_RECORDS_REMOVED_TIMESTAMP = 0,
                 CSR_EXAMPLE_BAD_TIMESTAMPS = "",
                 CSR_TIME_BEGIN = NA_character_,
                 CSR_TIME_END = NA_character_)

  dsd <- NULL  # dataset data

  if(!dir.exists(df)) {
    message("No data folder found for ", dataset_name)
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

    # Compute timestamp begin and/or ends

    tf <- dataset$description$CSR_TIMESTAMP_FORMAT
    tz <- dataset$description$CSR_TIMESTAMP_TZ
    ml <- dataset$description$CSR_MSMT_LENGTH

    if(is.na(ml)) {
      ml <- 60
      diag$CSR_ASSUMED_MSMT_LENGTH <- ml
    }

    ctlist <- calc_timestamps(dsd, ml, tf, tz)
    dsd <- ctlist[["dsd"]]

    # Remove records with invalid timestamps
    diag$CSR_RECORDS_REMOVED_TIMESTAMP <- sum(ctlist$na_ts)
    diag$CSR_EXAMPLE_BAD_TIMESTAMPS <- ctlist$bad_examples
    dsd <- remove_invalid_timestamps(dsd, tf, tz)

    # Change to the site's timezone (which is usually the same but might be different)
    dsd <- lubridate::with_tz(dsd, tzone = dataset$description$CSR_TIMEZONE)

    # Diagnostic information
    diag$CSR_TIME_BEGIN <- format(min(dsd$CSR_TIMESTAMP_BEGIN), format = "%Y-%m-%d")
    diag$CSR_TIME_END <- format(max(dsd$CSR_TIMESTAMP_END), format = "%Y-%m-%d")

    # Add port column if necessary
    dsd <- add_port_column(dsd)

    # Rearrange columns
    dsd <- rearrange_columns(dsd, required_cols =
                               c("CSR_PORT", "CSR_TIMESTAMP_BEGIN",
                                 "CSR_TIMESTAMP_END", "CSR_FLUX"))

    return(qaqc_data(dsd, diag))
  }

  # If no data this gets returned
  list(dsd = dsd, diag = diag)
}

#' Read a complete dataset from either standardized or raw files
#'
#' @param dataset_name Dataset name, character
#' @param raw_data Path to the raw data folder (not in package)
#' @param force_raw Ignore existing standardized data and read raw data, logical
#' @param quiet Print progress messages and warnings? Logical
#' @param metadata_only Quick-read metadata only? Logical
#' @return A list with (at least) elements:
#' \item{description}{Contents of \code{DESCRIPTION.txt} file}
#' \item{contributors}{Contents of \code{CONTRIBUTORS.txt} file}
#' \item{ports}{Contents of \code{PORTS.txt} file}
#' \item{data}{Continuous soil respiration data, parsed into a \code{data.frame}}
#' \item{diagnostics}{Diagnostics on the data parsing and QC process}
#' \item{ancillary}{Ancillary site information}
#' @export
#' @examples
#' suppressWarnings(read_dataset("TEST_licordata"))
read_dataset <- function(dataset_name, raw_data, force_raw = FALSE, quiet = FALSE, metadata_only = FALSE) {
  stopifnot(is.character(dataset_name))
  stopifnot(length(dataset_name) == 1)
  stopifnot(is.logical(force_raw))

  dataset <- list(description = read_description_file(dataset_name),
                  contributors = read_contributors_file(dataset_name),
                  ports = read_ports_file(dataset_name),
                  columns = read_columns_file(dataset_name),
                  ancillary = read_ancillary_file(dataset_name))

  if(!metadata_only) {

    # Parse the actual data. There are three possibilities:

    # 1. By default we try to read 'standardized' data; these are stored
    # inside the package, and are data we've already parsed and QC'd from
    # contributed 'raw' data. They live in inst/extdata/{dataset}/data

    # 2. 'Raw' data are used if standardized data not found; these need to
    # be read from an external {raw_data} directory, then have their columns
    # mapped, be QC'd, etc.

    # 3. If neither standardized nor raw data are found, return a dataset
    # with no data (the `data` or `diagnostics` list members).

    data_dir <- file.path(resolve_dataset(dataset_name), "data")
    datafile <- file.path(data_dir, "data.RDS")

    if(force_raw | !file.exists(datafile)) {  # raw
      if(missing(raw_data)) {
        if(!quiet) message(dataset_name, "\tNo standardized or raw data found")
        return(dataset)
      }
      if(!quiet) message(dataset_name, "\tReading and parsing raw data")
      x <- read_raw_dataset(dataset_name, raw_data, dataset)
      dataset$diagnostics <- x$diag
      dataset$data <- x$dsd

    } else {  # standardized
      if(!quiet) message(dataset_name, "\tReading standardized data")
      # Read data
      dataset$data <- readRDS(datafile)
      # Read diagnostics info
      diagfile <- file.path(data_dir, "diag.RDS")
      dataset$diagnostics <- readRDS(diagfile)
    }
  }

  dataset
}
