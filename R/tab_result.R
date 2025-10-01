#' @title tab_result
#' @description \code{tab_result} computes result data from peak data or a data.frame containing at least two numeric columns.
#' @details Result data table for data of the type "sample". Depending on the selected method, the analyte mass will be
#' calculated from the peak area and calibration data which are provided in data.frames. For an external gas calibration, values of the
#' intensity column from a data.frame containing at least two columns are transferred into mass flow data using calibration data from a
#' data.frame. The analyte mass will be determined via peak integration using the function \code{get_peakdata}. Result data will be provided
#' for the analyte component itself and as element, as well as content value related to the sample mass.
#' @param peak_data Data.frame containing peak information.
#' @param wf Calibration method/Workflow.
#' @param a Intercept from a calibration model (for ExtCal).
#' @param b Slope from a calibration model (for ExtCal).
#' @param K K (for IMDS).
#' @param amae Analyte mass as element  (for IMDS).
#' @param mass_fraction2 Mass fraction of the analyte element in the analyte component.
#' @param sample_mass Sample mass in [mg].
#' @return A data.frame.
#' @export
tab_result <- function(peak_data, wf = c("ExtCal", "ExtGasCal", "IDMS", "oIDMS"), a = 0, b = 1, K = 1, amae = 1, mass_fraction2 = 1, sample_mass = 1) {
  # Checks
  # tbd

  # Result table
  wf <- match.arg(wf)
  out <- NULL
  if (wf %in% c("ExtCal","ExtGasCal")) {
    result <- (peak_data[,4] - a) / b
  }
  if (wf %in% c("IDMS","oIDMS")) {
    result <- amae
    if (wf == "IDMS") {
      peak_data[,"R_corr"] <- peak_data[,"R_m"] * K
    }
    if (wf == "oIDMS") {
      peak_data <- peak_data[,!(colnames(peak_data) %in% c("Isotope", "Area [cts]"))]
    }
  }
  sample_mass <- rep(sample_mass, length.out=length(result))
  mass_fraction2 <- rep(mass_fraction2, length.out=length(result))
  out <- cbind(
    peak_data,
    "Analyte mass as element [ng]" = result,
    "Analyte mass [\u00b5g]" = result / mass_fraction2,
    "Mass fraction" = mass_fraction2,
    "Sample mass [mg]" = sample_mass,
    "Content as element [ppm]" = result / sample_mass,
    "Content as analyte [ppm]" = result / mass_fraction2 / sample_mass
  )
  return(out)
}
