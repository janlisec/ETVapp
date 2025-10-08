#' @title get_peakdata.
#' @description \code{get_peakdata} will generate a table with peak data (atart
#'     and end point, peak area) or the data of a signal range (start and end point, mean signal intensity) from a data.frame.
#' @details This function provides a simple detection algorithm for peak boundaries
#'     based on the peak height and alternatively allows for manual setting of the
#'     peak boundaries or selecting a signal range. It has implemented a method for baseline correction based on
#'     polynomial fitting and will compute the peak area using the trapezoidal rule.
#' @param pro_data Data.frame with at least two numeric columns.
#' @param int_col Column name of the intensity column to be used.
#' @param time_col Column name of the time column to be used.
#' @param peak_start Value which is taken as peak start point, when manual peak picking is chosen.
#' @param peak_end Value which is taken as peak end point, when manual peak picking is chosen.
#' @param minpeakheight A threshold value for peak picking via peak height.
#' @param PPmethod Peak picking method.
#' @param BLmethod Method for baseline correction.
#' @param deg Degree of polynomial for baseline correction.
#' @param cf A correction value for cutting the area around the detected peak.
#'
#' @return A data.frame with the peak boundaries in seconds and the peak area in cps
#' .
#' @examples
#' raw_data <- ETVapp::ETVapp_testdata[["oIDMS"]][["Samples"]][[1]]
#' pro_data <- process_data(raw_data, c1 = "117Sn", c2 = "80Se", fl = 151)
#' peak_data <- get_peakdata(pro_data, int_col = "117Sn", peak_start = 80, peak_end = 130)
#' plot(x = pro_data[,1:2], type = "l")
#' abline(v = peak_data[1,2:3], col=grey(0.7))
#'
#' @export
get_peakdata <- function (pro_data, int_col, time_col = "Time",
                          peak_start = NULL, peak_end = NULL, minpeakheight = 1000,
                          PPmethod = c("Peak (height)", "Peak (manual)", "mean_signal"),
                          BLmethod = c("modpolyfit", "none"), deg = 1, cf = 50) {

  BLmethod <- match.arg(BLmethod)
  PPmethod <- match.arg(PPmethod)

  ensure_that(int_col %in% colnames(pro_data), "Please select an existing Intensity column.", opt = "stop")

  # get peak data
  peak_data <- get_peak(df = pro_data[,c(time_col, int_col)], PPmethod = PPmethod, peak_start = peak_start, peak_end = peak_end, minpeakheight = minpeakheight, cf =cf)

  if (PPmethod == "mean_signal") { return(peak_data) }

  # Baseline correction
  corr_data <- blcorr_col(df = pro_data[peak_data$Start_corr:peak_data$End_corr,c(time_col, int_col)], nm = int_col, BLmethod = BLmethod, deg = deg)

  # $$JL: this will only BL correct the peak range and not the entire measurement. See difference below in the commented section
  # x1 <- blcorr_col(df = pro_data[peak_data$Start_corr:peak_data$End_corr,,drop=FALSE], nm =  int_col, BLmethod = BLmethod, deg = deg)
  # x2 <- blcorr_col(df = pro_data, nm =  int_col, BLmethod = BLmethod, deg = deg)[peak_data$Start_corr:peak_data$End_corr,]
  # identical(x1,x2)

  # Integration of peak area
  idx <- ((peak_data[,"Peakstart"]-peak_data[,"Start_corr"])+1):(nrow(corr_data)-abs(peak_data[,"Peakend"]-peak_data[,"End_corr"]))
  peak_area <- pracma::trapz(corr_data[idx, 1], corr_data[idx, 2])

  # prepare output
  peak_data <- data.frame(
    "Isotope" = int_col,
    "Start [s]" = ifelse(PPmethod == "Peak (manual)", peak_start, pro_data[peak_data[,2], time_col]),
    "End [s]" = ifelse(PPmethod == "Peak (manual)", peak_end, pro_data[peak_data[,3], time_col]),
    "Area [cts]" = peak_area,
    "BLmethod" = BLmethod,
    check.names = FALSE
  )

  return(peak_data)
}
