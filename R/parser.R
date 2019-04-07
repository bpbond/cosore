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
  file_data <- file_data[grep(rx, file_data)]
  if(required & length(file_data) != 1) {
    stop(length(file_data), " entries found for required label ", line_label)
  }
  d <- trimws(gsub(rx, "", file_data))
  if(length(d) == 0) d <- NA_character_

  if(numeric_data) {
    dn <- suppressWarnings(as.numeric(d))
    if(is.na(dn)) stop(d, " could not be converted to numeric for ", line_label)
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

#' Get the full path of a dataset folder
#'
#' @param dataset_name Dataset name, character
#' @keywords internal
#' @return Fully-qualified filename of dataset folder in \code{inst/extdata}.
resolve_dataset <- function(dataset_name) {
  system.file(file.path("extdata", dataset_name), package = "csrdb", mustWork = TRUE)
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

  data.frame(Dataset = dataset_name,
             Site_name = extract_line(file_data, "Site_name"),
             Longitude = extract_line(file_data, "Longitude", numeric_data = TRUE),
             Latitude = extract_line(file_data, "Latitude", numeric_data = TRUE),
             Instrument = extract_line(file_data, "Instrument"),
             Ecosystem_type = extract_line(file_data, "Ecosystem_type"),
             Primary_pub = extract_line(file_data, "Primary_pub", required = FALSE),
             Other_pubs = extract_line(file_data, "Other_pub", required = FALSE),
             Data_URL = extract_line(file_data, "Data_URL", required = FALSE),
             stringsAsFactors = FALSE)
}


#' Read dataset CONTRIBUTORS file
#'
#' @param dataset_name Dataset name, character
#' @param file_data File data, character vector; optional for testing
#' @keywords internal
#' @note For information about ORCID see \url{https://orcid.org}. For
#' CRediT roles, see \url{https://www.casrai.org/credit.html}.
#' @return A \code{data.frame} with the following columns:
#' \item{Dataset}{Dataset name, character}
#' \item{First_name}{Site name, character}
#' \item{Family_name}{Decimal Longitude, numeric}
#' \item{Email}{Decimal latitude, numeric}
#' \item{ORCID}{Instrument type, character}
#' \item{Role}{Role, character}
read_contributors_file <- function(dataset_name, file_data = NULL) {
  file_data <- read_file(dataset_name, "CONTRIBUTORS.txt", file_data)

  contribs <- data.frame()
  entries <- grep("^First_name", file_data)
  for(i in seq_along(entries)) {
    if(i < length(entries)) {
      fd <- file_data[entries[i]:entries[i + 1] - 1]
    } else {
      fd <- file_data[entries[i]:length(file_data)]
    }

    df <- data.frame(Dataset = dataset_name,
                     First_name = extract_line(fd, "First_name"),
                     Family_name = extract_line(fd, "Family_name"),
                     Email = extract_line(fd, "Email", required = FALSE),
                     ORCID = extract_line(fd, "ORCID", required = FALSE),
                     Role = extract_line(fd, "Role", required = FALSE),
                     stringsAsFactors = FALSE)
    contribs <- rbind(contribs, df)
  }
  contribs
}


#' Read dataset PORTS file
#'
#' @param dataset_name Dataset name, character
#' @param file_data File data, character vector; optional for testing
#' @keywords internal
#' @return A \code{data.frame} with the following columns:
#' \item{Dataset}{Dataset name, character}
#' \item{Port}{Port number, numeric; 0 = all ports}
#' \item{Treatment}{Treatment, character; by default "None"}
#' \item{Species}{Species, character}
#' \item{V1-V4}{Sensor information for options V1-V4 probes}
read_ports_file <- function(dataset_name, file_data = NULL) {
  file_data <- read_file(dataset_name, "PORTS.txt", file_data)

  ports <- data.frame()
  entries <- grep("^Port", file_data)
  for(i in seq_along(entries)) {
    if(i < length(entries)) {
      fd <- file_data[entries[i]:entries[i + 1] - 1]
    } else {
      fd <- file_data[entries[i]:length(file_data)]
    }

    df <- data.frame(Dataset = dataset_name,
                     Port = extract_line(fd, "Port", numeric_data = TRUE),
                     Treatment = extract_line(fd, "Treatment"),
                     Species = extract_line(fd, "Species", required = FALSE),
                     V1 = extract_line(fd, "V1", required = FALSE),
                     V2 = extract_line(fd, "V2", required = FALSE),
                     V3 = extract_line(fd, "V3", required = FALSE),
                     V4 = extract_line(fd, "V4", required = FALSE),
                     stringsAsFactors = FALSE)
    ports <- rbind(ports, df)
  }
  ports
}


#' Read a complete dataset
#'
#' @param dataset_name Dataset name, character
#' @return A list with elements:
#' \item{description}{Contents of \code{DESCRIPTION.txt} file}
#' \item{contributors}{Contents of \code{CONTRIBUTORS.txt} file}
#' \item{ports}{Contents of \code{PORTS.txt} file}
#' \item{data}{Continuous soil respiration data, parsed into a \code{data.frame}}
#' @export
#' @examples
#' read_dataset("d20190406_TEST")
read_dataset <- function(dataset_name) {
  list(description = read_description_file(dataset_name),
       contributors = read_contributors_file(dataset_name),
       ports = read_ports_file(dataset_name),
       site = NULL, # TODO
       data = NULL  # TODO
  )
}
