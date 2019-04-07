# parse-LI8100.R


#' Parse a LI-8100 (with LI-8150 multiplexer) data file.
#'
#' @param filename Filename, character
#' @param port_data Port information extracted from \code{PORTS.txt} file.
#' @return A \code{data.frame} containing extracted data.
#' @export
parse_LI8100_LI8150_file <- function(filename, port_data) {

  # Read file into memory and find records
  filedata <- readLines(filename)
  record_starts <- grep(pattern = "^LI-8100", filedata)
  message("Reading ", basename(filename), ": lines = ", length(filedata), " records = ", length(record_starts), "\n")

  # Set up results data frame and fill it in as we go
  results <- data.frame(Record = seq_along(record_starts),
                        Timestamp = as.POSIXct(NA),
                        Label = NA_character_,
                        Port = NA_integer_,
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

  for (i in seq_along(record_starts)) {
    if(i < length(record_starts)) {
      record_end <- record_starts[i+1]-1
    } else {
      record_end <- length(filedata)
    }

    # Isolate the lines of this record...
    record <- filedata[record_starts[i]:record_end]
    # ...and get rid of blank lines because that can screw up paste(collapse()) below
    record <- record[grep("^$", record, invert = TRUE)]

    # Find the data table start
    table_start <- tail(grep("^Type\t", record), n = 1)
    # Look for the next non-numeric line; this marks the end
    table_stop <-  head(grep("^[A-Z]", record[-(1:table_start)]), n = 1) + table_start - 1

    # Sometimes the Licor aborts in the middle of a measurement. Handle gracefully
    if(length(table_stop) == 0) {
      results$Error[i] <- TRUE
      message("Licor abort in ", basename(filename), " ", record_starts[i], ":", record_end)
      next()
    }
    # Find names, discarding any trailing 'Annotation' column, because if it's empty
    # the Licor software doesn't add a trailing comma, which read.tsv can't handle
    col_names <- strsplit(record[table_start], "\t", fixed = TRUE)[[1]]
    col_names <- col_names[!grepl("Annotation", col_names)]

    con <- textConnection(paste(record[(table_start+1):table_stop], collapse = "\n"))
    df <- try({
      read.table(con, col.names = col_names, sep = "\t", stringsAsFactors = FALSE)
    }, silent = TRUE)
    close(con)

    if(class(df) == "try-error") {
      results$Error[i] <- TRUE
      message("read.table error reading table ", i, " ", record_starts[i], ":", record_end)
      next
    }
    errorlines <- which(df$Type < 0)
    if(length(errorlines) || class(df) == "try-error") {
      results$Error[i] <- TRUE
      message("Licor error in ", basename(filename), " ", record_starts[i], ":", record_end)
      next()
    }

    # Convert to POSIXct
    df$Date <- as.POSIXct(df$Date,format="%Y-%m-%d %H:%M:%S")

    # Pull out the data we're interested in
    index <- which(df$Type == 1)
    results$Timestamp[i] <- mean(df$Date)
    results$Label[i] <- extract_line(record, "Label")
    results$Port[i] <- extract_line(record, "Port#")
    results$Flux[i] <- extract_line(record, "Exp_Flux")
    results$R2[i] <- extract_line(record, "Exp_R2")
    results$Tcham[i] <- mean(df$Tcham[index])
    results$Area[i] <- extract_line(record, "Area")
    results$V1[i] <- mean(df$V1[index])
    results$V2[i] <- mean(df$V2[index])
    results$V3[i] <- mean(df$V3[index])
    results$V4[i] <- mean(df$V4[index])
    results$RH[i] <- mean(df$RH[index])
    results$Cdry[i] <- mean(df$Cdry[index])
    results$Comments[i] <- extract_line(record, "Comments")
  }

  # Clean up and return
  results$Port <- as.integer(results$Port)
  results$Flux <- as.numeric(results$Flux)
  results$R2 <- as.numeric(results$R2)
  results
}

#----- Function to loop through directory and read multiplexed Licor-8100 data -----
parse_LI8100_LI8150 <- function(path, port_data) {
  files <- list.files(path, pattern = ".81x$", full.names = TRUE)
  do.call("rbind", lapply(files, read_licor_data, port_data))
}
