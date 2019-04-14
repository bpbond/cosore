# parse-LI8100.R


#' Parse a LI-8100 (with LI-8150 multiplexer) data file.
#'
#' @param filename Filename, character
#' @param port_data Port information extracted from \code{PORTS.txt} file
#' @param UTC_offset Offset from UTC in hour, numeric
#' @return A \code{data.frame} containing extracted data.
#' @importFrom utils read.table
#' @export
parse_LI8100_file <- function(filename, port_data, UTC_offset) {

  # Read file into memory and find records
  filedata <- readLines(filename)
  record_starts <- grep(pattern = "^LI-8100", filedata)
  bfn <- basename(filename)
  message("Reading ", bfn, ": lines = ", length(filedata), " records = ", length(record_starts))

  if(length(record_starts) == 0) {
    return(NULL)  # ¯\_(ツ)_/¯
  } else {
    # Set up results data frame and fill it in as we go
    results <- data.frame(
      Record = seq_along(record_starts),
      # can't use NA for some reason--screws up mean() timestamp computation later
      Timestamp = as.POSIXct("1970-01-01", tz = "UTC"),
      Label = NA_character_,
      Port = NA_integer_,
      # next two are converted to numeric at end for performance
      Flux = NA_character_,
      R2 = NA_character_,
      Tcham = NA_real_,
      Area = NA_real_,
      V1 = NA_real_, V2 = NA_real_, V3 = NA_real_, V4 = NA_real_,
      RH = NA_real_,
      Cdry = NA_real_,
      Comments = NA_character_,
      Error = FALSE,
      stringsAsFactors = FALSE)

    # Main loop
    for (i in seq_along(record_starts)) {
      if(i < length(record_starts)) {
        record_end <- record_starts[i+1]-1
      } else {
        record_end <- length(filedata)
      }

      # Isolate the lines of this record
      record <- filedata[record_starts[i]:record_end]

      # There are three categories of data here:
      # 1 - record-level data that occur BEFORE the data table (e.g. port number)
      # 2 - table data (e.g. CO2 measurements)
      # 3 - record-level data AFTER the table (e.g. mean flux)

      # 1 - record-level data that occur BEFORE the data table
      results$Label[i] <- extract_line(record, "Label")
      results$Port[i] <- as.integer(extract_line(record, "Port#"))
      results$Area[i] <- extract_line(record, "Area")
      results$Comments[i] <- extract_line(record, "Comments")

      # 2 - table data
      # Find the data table start
      table_start <- grep("^Type\t", record)
      # Look for the next non-numeric line; this marks the end
      table_stop <-  grep("^[A-Z]", record[-(1:table_start)])[1] + table_start - 1

      # Sometimes the Licor aborts in the middle of a measurement. Handle gracefully
      if(is.na(table_stop)) {
        results$Error[i] <- TRUE
        message("Licor abort in ", bfn, " ", record_starts[i], ":", record_end)
        next()
      }

      # Find names, discarding any trailing 'Annotation' column, because if it's empty
      # the Licor software doesn't add a trailing comma, which read.tsv can't handle
      col_names <- strsplit(record[table_start], "\t", fixed = TRUE)[[1]]
      col_names <- col_names[!grepl("Annotation", col_names)]

      con <- textConnection(record[(table_start+1):table_stop])
      dat <- try({
        read.table(con, col.names = col_names, sep = "\t", stringsAsFactors = FALSE)
      }, silent = TRUE)
      close(con)

      if(class(dat) == "try-error") {
        results$Error[i] <- TRUE
        message("read.table error in ", bfn, " ", i, " ", record_starts[i], ":", record_end)
        next
      }
      # Check whether an error (e.g. chamber closing problem) occurred
      errorlines <- which(dat$Type < 0)
      if(length(errorlines) || class(dat) == "try-error") {
        results$Error[i] <- TRUE
        message("Licor error in ", bfn, " ", record_starts[i], ":", record_end)
        next()
      }

      # Convert to POSIXct and UTC so we can take mean timestamp
      stopifnot(abs(UTC_offset) < 15)
      dat$Date <- as.POSIXct(dat$Date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") - UTC_offset * 60 * 60

      # Pull out the table-level data we're interested in
      index <- which(dat$Type == 1)
      results$Timestamp[i] <- mean(dat$Date)
      results$Tcham[i] <- mean(dat$Tcham[index])
      results$V1[i] <- mean(dat$V1[index])
      results$V2[i] <- mean(dat$V2[index])
      results$V3[i] <- mean(dat$V3[index])
      results$V4[i] <- mean(dat$V4[index])
      results$RH[i] <- mean(dat$RH[index])
      results$Cdry[i] <- mean(dat$Cdry[index])

      # 3 - record-level data AFTER the table
      results$Flux[i] <- extract_line(record, "Exp_Flux", required = FALSE)
      results$R2[i] <- extract_line(record, "Exp_R2", required = FALSE)

      # Check the V1...4 (voltage) information fields; if any sensors are found,
      # use the PORTS info to create new data fields
      for(v in 1:4) {
        info <- extract_line(record, paste0("V", v, " Info"))
        if(grepl("(THERM|SM)$", info)) {
          port_info <- port_data[[paste0("V", v)]]
          # A port 0 entry means "all ports"
          w <- which(port_data$Port == 0 | port_data$Port == results$Port[i])

          if(port_info[w] == "") {
            message(filename, i, v, "Sensors detected but no info given for this port")
          } else { # create new column
            results[[port_info[w]]] <- results[[paste0("V", v)]]
          }
        }
      }

    } # for i

    # Clean up and return
    results$Flux <- as.numeric(results$Flux)
    results$R2 <- as.numeric(results$R2)
    results
  }
}

#' Loop through directory and read raw multiplexed Licor-8100 data
#'
#' @param path Directory path, character
#' @param port_data Port data, a list returned by \code{\link{read_ports_file}}
#' @param UTC_offset Offset from UTC in hour, numeric
#' @return A data frame with all data read from file(s).
#' @export
parse_LI8100_raw <- function(path, port_data, UTC_offset) {
  files <- list.files(path, pattern = ".81x$", full.names = TRUE, recursive = TRUE)
  do.call("rbind", lapply(files, parse_LI8100_file, port_data, UTC_offset))
}

#' Loop through directory and read processed multiplexed Licor-8100 data
#'
#' @param path Directory path, character
#' @param UTC_offset Offset from UTC in hour, numeric
#' @return A data frame with all data read from file(s).
#' @note Processed (in the Licor application) data consists of a tab-delimited
#' text file with a standard set of columns.
#' @importFrom utils read.delim
#' @export
parse_LI8100_processed <- function(path, UTC_offset) {
  files <- list.files(path, pattern = "^[0-9]{8}.txt$", full.names = TRUE, recursive = TRUE)
  dat <- do.call("rbind", lapply(files, read.delim, stringsAsFactors = FALSE, check.names = FALSE))
  dat$`IV Date` <- as.POSIXct(dat$`IV Date`, format = "%Y-%m-%d %H:%M:%S", tz = "UTC") - UTC_offset * 60 * 60
  dat
}
