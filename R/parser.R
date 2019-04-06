# File parser

#' Parse labels from information files
#'
#' @param file_data Character vector of file lines
#' @param line_label Character label
#' @param sep Separator
#'
#' @return Value (character) of data opposite label
#' @export
#'
#' @examples
extract_line <- function(file_data, line_label, required = TRUE, sep = ":") {
  rx <- paste0("^", line_label, sep)
  file_data <- file_data[grep(rx, file_data)]
  if(required & length(file_data) != 1) {
    stop(length(file_data), "entries found for required label ", line_label)
  }
  trimws(gsub(rx, "", file_data))
}

list_datasets <- function() {
  ds <- list.files(path = resolve_dataset(""))
  ds[grep("^d", ds)]  # dataset folders must start with "d"
}

resolve_dataset <- function(dataset_name) {
  system.file(file.path("extdata", dataset_name), package = "csrdb", mustWork = TRUE)
}

read_file <- function(dataset_name, file_name) {
  file_data <- readLines(file.path(resolve_dataset(dataset_name), file_name))
  file_data[grep("^#", file_data, invert = TRUE)]
}

read_description_file <- function(dataset_name) {
  file_data <- read_file(dataset_name, "DESCRIPTION.txt")
  data.frame(Dataset = dataset_name,
             Site_name = extract_line(file_data, "Site name"),
             Longitude = extract_line(file_data, "Longitude"),
             Latitude = extract_line(file_data, "Latitude"),
             Instrument = extract_line(file_data, "Instrument"),
             Ecosystem_type = extract_line(file_data, "Ecosystem type"),
             Primary_pub = extract_line(file_data, "Primary publication"),
             Other_pubs = extract_line(file_data, "Other publications", required = FALSE)
  )
}

read_contributors_file <- function(dataset_name) {
  file_data <- read_file(dataset_name, "CONTRIBUTORS.txt")

  contribs <- data.frame()
  entries <- grep("^First name", file_data)
  for(i in seq_along(entries)) {
    if(i < length(entries)) {
      fd <- file_data[entries[i]:entries[i + 1] - 1]
    } else {
      fd <- file_data[entries[i]:length(file_data)]
    }

    df <- data.frame(Dataset = dataset_name,
                     First_name = extract_line(fd, "First name"),
                     Family_name = extract_line(fd, "Family name"),
                     Email = extract_line(fd, "Email", required = FALSE),
                     ORCID = extract_line(fd, "ORCID", required = FALSE),
                     Role = extract_line(fd, "Role", required = FALSE))
    contribs <- rbind(contribs, df)
  }
  contribs
}

read_ports_file <- function(dataset_name) {
  file_data <- read_file(dataset_name, "PORTS.txt")

  ports <- data.frame()
  entries <- grep("^Port", file_data)
  for(i in seq_along(entries)) {
    if(i < length(entries)) {
      fd <- file_data[entries[i]:entries[i + 1] - 1]
    } else {
      fd <- file_data[entries[i]:length(file_data)]
    }

    df <- data.frame(Dataset = dataset_name,
                     Port = extract_line(fd, "Port"),
                     Treatment = extract_line(fd, "Treatment"),
                     Species = extract_line(fd, "Species", required = FALSE),
                     V1 = extract_line(fd, "V1", required = FALSE),
                     V2 = extract_line(fd, "V2", required = FALSE),
                     V3 = extract_line(fd, "V3", required = FALSE),
                     V4 = extract_line(fd, "V4", required = FALSE))
    ports <- rbind(ports, df)
  }
  ports
}

read_dataset <- function(dataset_name) {
  list(Description = read_description_file(dataset_name),
       Contributors = read_contributors_file(dataset_name),
       Ports = read_ports_file(dataset_name),
       Data = NULL  # todo
       )
}
