#' @title tab_cali.
#' @description \code{tab_cali} will transfer standard information to a data.frame containing peak/signal data.
#' @details Selecting "ExtGasCal" enables the input of a conversion factor to calculate the gas flows. A conversion factor (mL/min to µL/s) is implemented in the function. 
#' @param peak_data Data.frame containing peak information.
#' @param wf Calibration method/Workflow.
#' @param std_info A numeric value giving the analyte mass (ExtCal), the concentration of an ionic standard solution in µg/L (oIDMS) or the gas flow of calibration gas (ExtGasCal).
#' @param gas_flow Gas flow of calibration gas.
#' @param fac A factor to convert the gas flow (gas_density x mass_fraction x mass_percentage).
#'
#' @return A data.frame with the standard no, the isotope, the peak/signal boundaries in seconds, the peak area in counts/mean signal in cps and information for the
#' analyte content in the calibration standard.
#'
#' @examples
#' # import all cali files into a list from example data folder
#' cali_imp <- ETVapp::ETVapp_testdata[["ExtCal"]][["Cali"]]
#'
#' # process cali files (and generate plots)
#' cali_pro <- lapply(cali_imp, function(x) {
#'   process_data(x, c1 = "157", fl = 7)
#' })
#' par(mfrow=grDevices::n2mfrow(nr.plots = length(cali_pro)))
#' for (i in 1:length(cali_pro)) plot(cali_pro[[i]][,1:2], type="l")
#'
#' # get cali peaks and combine
#' peak_start <- rep(145, length(cali_pro))
#' peak_end <- seq(180, 230, length.out=length(cali_pro))
#' cali_pks <- ldply_base(1:length(cali_pro), function(i) {
#'   pk <- get_peakdata(cali_pro[[i]], PPmethod = "Peak (manual)", int_col = "157",
#'     peak_start = peak_start[i], peak_end = peak_end[i])
#' })
#' tab_cali(peak_data = cali_pks, wf = "ExtCal", std_info = seq(0,50,10))
#'
#' # import and process cali data of `oIDMS` workflow
#' spion_imp <- ETVapp::ETVapp_testdata[["oIDMS"]][["sp_ionic"]]
#' cali_pks <- ldply_base(1:length(spion_imp), function(i) {
#'   get_peakdata(spion_imp[[i]][,c("Time", "197Au")], int_col = "197Au",
#'     PPmethod = "mean_signal", peak_start = 0.003, peak_end = 60)
#' })
#' fac <- 1.661 * 0.01104347 * 12/44
#' tab_cali(peak_data = cali_pks, wf = "oIDMS", std_info = c(20, 50, 100, 200, 500),
#'   fac = fac)
#'
#' @export

tab_cali <- function (peak_data, wf = c("ExtCal", "ExtGasCal", "oIDMS"), std_info = 0, gas_flow = 0, fac = 0) {

  wf <- match.arg(wf)

  # Additional parameters
  ml_to_mul <- 1000
  min_to_s <- 60

  # Calibration table
  if (wf == "ExtCal") {
    std_info <- check_std_info(std_info = std_info, n = nrow(peak_data))
    out <- cbind(peak_data, "Analyte mass [ng]" = std_info)
  }
  if (wf == "ExtGasCal") {
    std_info <- check_std_info(std_info = gas_flow, n = nrow(peak_data))
    out <- cbind(
      peak_data,
      "Gas flow [mL/min]" = std_info,
      "Gas flow [\u00b5g/s]" = std_info * (ml_to_mul / min_to_s) * fac
    )
  }
  if (wf == "oIDMS") {
    std_info <- check_std_info(std_info = std_info, n = nrow(peak_data))
    out <- cbind(peak_data, "Concentration [\u00b5g/L]" =  std_info)
  }
  return(out)
}
