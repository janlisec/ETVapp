#' @title import_data
#' @description \code{import_data} will import time resolved ICP-MS data in
#'     various file formats from different instrument types.
#' @details The function works for .csv, .TXT, and .exp files. Codes are
#'     provided to transform raw data from common ICP-MS instruments to a
#'     data.frame with a time column and additional columns for the acquired
#'     intensities. You may check why data import of your files fails in the app
#'     on this function and potentially extend it to handle your files.
#' @param file_path Valid file path.
#' @param simplify Use FALSE to ensure that names list is returned also for single file as input.
#' @examples
#' pth <- system.file(package = "ETVapp", "extdata", 'fmt')
#' imp <- import_data(dir(pth, full.names = TRUE))
#' lapply(imp, head)
#'
#' import_data("not_existent.file")
#'
#' \dontrun{
#' wfs <- c("ExtCal", "ExtGasCal", "IDMS", "oIDMS")
#' ETVapp_testdata <- setNames(lapply(wfs, function(wf) {
#'   subflds <- dir(system.file(package = "ETVapp", "extdata", wf))
#'   setNames(lapply(subflds, function(sf) {
#'   fls <- dir(system.file(package = "ETVapp", "extdata", wf, sf), full.names = TRUE)
#'     import_data(fls, simplify = FALSE)
#'   }), subfld)
#' }), wfs)
#' usethis::use_data(ETVapp_testdata)
#' }
#'
#' @return A data.frame containing numeric columns or a list of such data.frames
#'     in case that file_path is a vector.
#' @export

import_data <- function (file_path, simplify = TRUE) {
  import_data_internal <- function(file_path) {
    if (file.exists(file_path) & grepl("*.csv|*.TXT|*.exp", file_path)) {
      if (grepl("*.csv", file_path)) {
        file_content <- readLines(file_path)
        if (grepl("sep=;", file_content[1])) {
          raw_data <- utils::read.csv(file_path, skip = 1, sep = ";", colClasses = "numeric") #iCAP file
          colnames(raw_data) <- gsub("^X", "", colnames(raw_data))
          colnames(raw_data)[2] <- "Time"
          raw_data <- raw_data[,c(2,seq(3,ncol(raw_data),2))]

        } else {
          comment_line <- max(grep("*Time*", readLines(file_path)))
          raw_data <- utils::read.csv(file_path, skip = comment_line - 1)

          if ("Scan" %in% colnames(raw_data)) {
            raw_data <- utils::read.csv(file_path, skip = comment_line - 1, colClasses = "numeric") #Spectro file
            #raw_data <- subset(raw_data, select = -c("Scan", "Analog_Temp"))
            raw_data <- raw_data[,!(colnames(raw_data) %in% c("Scan", "Analog_Temp"))]
            # colnames(raw_data) <- sapply(strsplit(colnames(raw_data), "[.]"), function(x) {
            #   paste0(x[1], paste(x[-1], collapse="."))
            # })
            # remove first seperator '.'
            colnames(raw_data) <- sub("[.]", "", colnames(raw_data))
            if (all(grep("Time", colnames(raw_data))==1)) colnames(raw_data)[1] <- "Time" else message("Check format of Spectro file. Possible incorrect 'Time' column.")

          } else {
            raw_data <- utils::read.csv(file_path, skip = comment_line - 1) #Agilent file
            # remove comment from end
            flt <- apply(raw_data, 1, function(x) { any(is.na(x)) })
            if (any(flt)) raw_data <- raw_data[!flt,]
            raw_data[,1] <- as.numeric(raw_data[,1])
            colnames(raw_data) <- sapply(colnames(raw_data), function(x) {
              y <- gregexpr("[[:digit:]]+$", x)
              ifelse(all(y==-1), x, substr(x, y, nchar(x)))
            })
            #colnames(raw_data)[2:ncol(raw_data)] <- stringr::str_extract(colnames(raw_data[2:ncol(raw_data)]), "[[:digit:]]+$")
            if (all(grep("Time", colnames(raw_data))==1)) colnames(raw_data)[1] <- "Time" else message("Check format of Agilent file. Possible incorrect 'Time' column.")

          }
        }

      } else {
        file_content <- readLines(file_path)
        value_line <- grep("^[[:digit:]]", readLines(file_path))
        if (grepl("*Neptune*", file_content[1])) {
          raw_data <- utils::read.delim(file_path, skip = value_line[1] - 2) #Multicollector file
          if ("Time" %in% colnames(raw_data)) {
            raw_data[, "Time"] <- paste0(substr(raw_data[, "Time"], 1, 8), ".", substr(raw_data[, "Time"], 10, 12))
            raw_data[, "Time"] <- as.POSIXct(raw_data[, "Time"], format = "%H:%M:%OS")
            raw_data[, "Time"] <- as.numeric(raw_data[, "Time"] - raw_data[1, "Time"])
            reord <- c(1:grep("Time", colnames(raw_data)), ncol(raw_data), (grep("Time", colnames(raw_data)) + 1):(ncol(raw_data) - 1))
            raw_data <- raw_data[, reord]
            raw_data <- raw_data[, !apply(raw_data, 2, function(x) {
              all(is.na(x))
            }), drop = FALSE]}
          # remove several columns
          raw_data <- raw_data[,-1]
          raw_data <- subset(raw_data, select = which(grepl("Time|[[:digit:][:alpha:]]$", colnames(raw_data))))
          flt <- apply(raw_data, 1, function(x) { any(is.na(x)) })
          if (any(flt)) { raw_data <- raw_data[!flt,] }
          raw_data <- apply(raw_data, 2, as.numeric)
          colnames(raw_data) <- gsub("^X", "", colnames(raw_data))
          raw_data <- as.data.frame(raw_data)

        } else {
          raw_data <- utils::read.delim(file_path) #Element2 file
          colnames(raw_data) <- gsub("Trace.for.Mass.$", "Time", colnames(raw_data))
          raw_data <- raw_data[1:max(nrow(raw_data)-4, 1), ]
          raw_data <- apply(raw_data, 2, as.numeric)
          colnames(raw_data) <- gsub(".MR.$", "", colnames(raw_data))
          # colnames(raw_data) <- sapply(strsplit(colnames(raw_data), "[.]"), function(x) {
          #   paste0(x[1], paste(x[-1], collapse="."))
          # })
          # remove first seperator '.'
          colnames(raw_data) <- sub("[.]", "", colnames(raw_data))
          if (all(grep("Time", colnames(raw_data))==1)) colnames(raw_data)[1] <- "Time" else message("Check format of Element2 file. Possible incorrect 'Time' column.")
          raw_data <- as.data.frame(raw_data)
        }
      }
    } else {
      message("File '", basename(file_path), "' not found. Returning NULL.")
      raw_data <- NULL
    }

    return(raw_data)
  }

  if (length(file_path)>=2 | !simplify) {
    stats::setNames(lapply(file_path, function(x) { import_data_internal(x) }), basename(file_path))
  } else {
    import_data_internal(file_path = file_path)
  }

}
