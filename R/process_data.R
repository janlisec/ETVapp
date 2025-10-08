#' @title process_data
#' @description Corrects a numeric values in a data.frame column with respect to
#'     a column containing standard values and applies Savitzky-Golay smoothing.
#' @param data Data.frame with at least two numeric columns.
#' @param wf Calibration method/Workflow.
#' @param c1 Column name of the intensity column to be used (ExtCal) or of the spike isotope (IDMS).
#' @param c2 Column name of the intensity column to be used as internal standard for Argon correction (ExtCal) or sample isotope (IDMS).
#' @param fl Filter length, has to be odd.
#' @param amend Set TRUE to amend transformed columns instead of replacing them.
#'
#' @details
#' Additional details...
#'
#' @return A data.frame with two columns.
#'
#' @examples
#' imp <- ETVapp::ETVapp_testdata[['ExtGasCal']][['Samples']][[1]]
#' plot(imp[,c("Time","13C")], type="l")
#' pro_data <- process_data(imp, c1 = "13C", c2 = "80Se", fl = 5)
#' head(pro_data)
#' lines(pro_data, col=3)
#'
#' head(process_data(imp, c1 = "13C", c2 = "80Se", fl = 5, amend = TRUE))
#'
#' # test all error messages
#' \dontrun{
#'   colnames(imp)[1] <- "TIME"
#'   head(process_data(imp, c1 = "122Sn", c2 = "test", fl = 2))
#' }
#'
#' @export
process_data <- function(data, wf = c("ExtCal", "ExtGasCal", "IDMS", "oIDMS"), c1, c2 = NULL, fl = NULL, amend = FALSE) {
# $$ JL: channel amend parameter to _col functions
  wf <- match.arg(wf)

  # ensure time_col
  time_col <- check_Time_col(data)

  if (wf %in% c("ExtCal", "ExtGasCal")) {
    out <- data[,c(time_col, c1)]

    # correct data using intensity in c2
    if (!is.null(c2) && c2 != c1) {
      if (c2 %in% colnames(data)) {
        out <- cbind(out, data[,c2,drop=FALSE])
        # perform smoothing
        out <- smooth_col(df = out, nm = c1, fl = fl, amend = if (amend) "_smooth" else NULL)
        out <- smooth_col(df = out, nm = c2, fl = fl, amend = if (amend) "_smooth" else NULL)
        # perform scaling (of smoothed columns)
        out <- scale_col(df = out, nm = ifelse(amend, paste0(c1, "_smooth"), c1), std = ifelse(amend, paste0(c2, "_smooth"), c2), amend = if (amend) "_scale" else NULL)
      } else {
        message("Column c2 '", c2, "' not found. Correction step omitted.")
        out <- smooth_col(df = out, nm = c1, fl = fl, amend = if (amend) "_smooth" else NULL)
      }
    } else {
      out <- smooth_col(df = out, nm = c1, fl = fl, amend = if (amend) "_smooth" else NULL)
    }
  }

  if (wf %in% c("IDMS", "oIDMS")) {
    check_iso_cols(data, c1, c2)
    out <- data[,c("Time", c1, c2)]

    # perform smoothing
    out <- smooth_col(df = out, nm = c1, fl = fl, amend = if (amend) "_smooth" else NULL)
    out <- smooth_col(df = out, nm = c2, fl = fl, amend = if (amend) "_smooth" else NULL)

    if (wf == "oIDMS") {
      out <- cbind(
        out,
        "R_m" = out[,ifelse(amend, paste0(c1, "_smooth"), c1)] / out[,ifelse(amend, paste0(c2, "_smooth"), c2)]
      )
    }
  }

  return(out)
}
