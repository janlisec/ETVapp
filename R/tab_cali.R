#' Tabulate calibration data.
#'
#' Transfer standard information to a data.frame containing peak/signal data.
#'
#' @details Selecting "ExtGasCal" enables the input of a conversion factor to
#'     calculate the gas flows. A conversion factor (mL/min to µL/s) is
#'     implemented in the function.
#'
#' @param peak_data Data.frame containing peak information.
#' @param wf Calibration method/Workflow.
#' @param ExtCal_unit The measurement unit of the ExtCal workflow to correctly format the output.
#' @param ExtGasCal_unit The measurement unit of the ExtGasCal workflow to correctly format the output.
#' @param std_info A numeric value giving the analyte mass (ExtCal), the concentration of an ionic standard solution in µg/L (oIDMS) or the gas flow of calibration gas (ExtGasCal).
#' @param fac A factor to convert the gas flow (gas_density x mass_fraction x mass_percentage).
#'
#' @return A data.frame with the standard no, the isotope, the peak/signal
#'     boundaries in seconds, the peak area in counts/mean signal in cps and
#'     information for the analyte content in the calibration standard.
#'
#' @examples
#' # import all cali files into a list from example data folder
#' cali_imp <- ETVapp::ETVapp_testdata[["ExtCal"]][["Cali"]]
#'
#' # process cali files (and generate plots)
#' cali_pro <- process_data(cali_imp, c1 = "157", fl = 7)
#' par(mfrow=grDevices::n2mfrow(nr.plots = length(cali_pro)))
#' for (i in 1:length(cali_pro)) plot(cali_pro[[i]][,1:2], type="l")
#'
#' # get cali peaks and combine
#' ps <- rep(145, length(cali_pro))
#' pe <- seq(180, 230, length.out=length(cali_pro))
#' cali_pks <- get_peakdata(cali_pro, PPmethod = "Peak (manual)", int_col = "157",
#'     peak_start = ps, peak_end = pe)
#' tab_cali(peak_data = cali_pks, wf = "ExtCal", std_info = seq(0,50,10))
#'
#' # check if unit specification works
#' si <- seq(0,50,10)
#' tab_cali(peak_data = cali_pks, wf = "ExtCal", ExtCal_unit = "ng", std_info = si)
#' tab_cali(peak_data = cali_pks, wf = "ExtGasCal", ExtGasCal_unit = "\u00b5L/min", std_info = si)
#'
#' # import and process cali data of `oIDMS` workflow
#' spion_imp <- ETVapp::ETVapp_testdata[["oIDMS"]][["sp_ionic"]]
#' cali_pks <- get_peakdata(spion_imp, int_col = "197Au",
#'     PPmethod = "mean signal", peak_start = 0.003, peak_end = 60)
#' tab_cali(peak_data = cali_pks, wf = "oIDMS", std_info = c(20, 50, 100, 200, 500))
#'
#' @export
tab_cali <- function (peak_data, wf = c("ExtCal", "ExtGasCal", "oIDMS"),
                      ExtCal_unit = c("pg", "ng", "\u00b5g"),
                      ExtGasCal_unit = c("nL/min", "\u00b5L/min", "mL/min"),
                      std_info = 0, fac = 0) {

  wf <- match.arg(wf)
  ExtCal_unit <- match.arg(ExtCal_unit)
  ExtGasCal_unit <- match.arg(ExtGasCal_unit)

  unit <- switch(
    wf,
    ExtCal = ExtCal_unit,
    ExtGasCal = ExtGasCal_unit,
    oIDMS = "\u00b5g/L"
  )
  unit2 <- switch(
    ExtGasCal_unit,
    "nL/min" = "pg/s",
    "\u00b5L/min" = "ng/s",
    "mL/min" = "\u00b5g/s"
  )

  # Additional parameters
  ml_to_mul <- 1000
  min_to_s <- 60

  # Calibration table
  if (wf == "ExtCal") {
    std_info <- check_std_info(std_info = std_info, n = nrow(peak_data))
    out <- cbind(peak_data, "Analyte mass [unit]" = std_info)
  }
  if (wf == "ExtGasCal") {
    std_info <- check_std_info(std_info = std_info, n = nrow(peak_data))
    out <- cbind(
      peak_data,
      "Gas flow [unit]" = std_info,
      "Gas flow [unit2]" = std_info * (ml_to_mul / min_to_s) * fac
    )
  }
  if (wf == "oIDMS") {
    std_info <- check_std_info(std_info = std_info, n = nrow(peak_data))
    out <- cbind(peak_data, "Concentration [unit]" =  std_info)
  }

  colnames(out) <- gsub("unit2", unit2, colnames(out))
  colnames(out) <- gsub("unit", unit, colnames(out))

  return(out)
}
