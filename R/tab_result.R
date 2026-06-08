#' Tabulate sample data
#'
#' Computes result data from peak data or a data.frame containing at least two
#' numeric columns.
#'
#' @details Result data table. Depending on the selected method, the analyte
#'     mass will be calculated from the peak area and calibration data which are
#'     provided in data.frames. For the on-line IDMS workflow, values of the
#'     intensity column from a data.frame containing at least two columns are
#'     transferred into mass flow data using calibration data from a data.frame.
#'     The analyte mass will be determined via peak integration using the
#'     function \code{get_peakdata}. Result data will be provided for the
#'     analyte component itself and as element, as well as content value related
#'     to the sample mass.
#'
#' @param peak_data Data.frame containing peak information.
#' @param wf Calibration method/Workflow.
#' @param ExtCal_unit The measurement unit of the ExtCal workflow to correctly format the output.
#' @param ExtGasCal_unit The measurement unit of the ExtGasCal_unit workflow to correctly format the output.
#' @param c_sp_unit The measurement unit of the "IDMS", "oIDMS" workflow to correctly format the output.
#' @param a Intercept from a calibration model (for ExtCal/ExtGasCal).
#' @param b Slope from a calibration model (for ExtCal/ExtGasCal).
#' @param K K (for IDMS).
#' @param amae Analyte mass as element (for IDMS/oIDMS).
#' @param mass_fraction2 Mass fraction of the analyte element in the analyte component.
#' @param sample_mass Sample mass in [mg].
#' @return A data.frame.
#'
#' @examples
#' # wf "ExtCal", "ExtGasCal" use col = 4
#' pd <- data.frame("A"=1:3,"B"=1:3,"R_m"=1:3,"col4"=1:3)
#' tab_result(peak_data=pd, wf="ExtCal")
#' # wf "IDMS", "oIDMS" use col = "R_m"
#' tab_result(peak_data=pd, wf="IDMS", c_sp_unit="g/L")
#' tab_result(peak_data=pd, wf="IDMS", c_sp_unit="mg/L")
#' tab_result(peak_data=pd, wf="IDMS", c_sp_unit="mg/L", mass_fraction2 = 10^3)
#' tab_result(peak_data=pd, wf="IDMS", c_sp_unit="mg/L", mass_fraction2 = 10^4)
#'
#' @export
tab_result <- function(peak_data, wf = c("ExtCal", "ExtGasCal", "IDMS", "oIDMS"),
                       ExtCal_unit = c("pg", "ng", "\u00b5g"),
                       ExtGasCal_unit = c("nL/min", "\u00b5L/min", "mL/min"),
                       c_sp_unit = c("\u00b5g/L", "mg/L", "g/L"),
                       a = 0, b = 1, K = 1, amae = 1, mass_fraction2 = 1, sample_mass = 1) {
  # Checks
  # tbd

  # Result table
  wf <- match.arg(wf)
  ExtCal_unit <- match.arg(ExtCal_unit)
  ExtGasCal_unit <- match.arg(ExtGasCal_unit)
  c_sp_unit <- match.arg(c_sp_unit)

  ExtGasCal_unit <- switch(ExtGasCal_unit,
                           "nL/min" = "pg",
                           "\u00b5L/min" = "ng",
                           "mL/min" = "\u00b5g")
  c_sp_unit <- switch(c_sp_unit,
                      "\u00b5g/L" = "pg",
                      "mg/L" = "ng",
                      "g/L" = "\u00b5g")
  unit <- switch(wf, ExtCal = ExtCal_unit, ExtGasCal = ExtGasCal_unit, IDMS = c_sp_unit, oIDMS = c_sp_unit)
  unit2 <- switch(unit,
                  "pg" = "ppb",
                  "ng" = "ppm",
                  "\u00b5g" = "g/100 g")

  out <- NULL
  if (wf %in% c("ExtCal","ExtGasCal")) {
    result <- (peak_data[,4] - a) / b
  }
  if (wf %in% c("IDMS","oIDMS")) {
    result <- amae
    if (wf == "IDMS") {
      #browser()
      peak_data[,"R_corr"] <- peak_data[,"R_m"] * K
    }
    if (wf == "oIDMS") {
      peak_data <- peak_data[,!(colnames(peak_data) %in% c("Isotope", "Area [cts]"))]
    }
  }
  sample_mass <- rep(sample_mass, length.out=length(result))
  if (unit2 == "g/100 g") { fac2 <- 10 } else { fac2 <- 1 }

  mass_fraction2 <- rep(mass_fraction2, length.out=length(result))
  if (any(result < 0.001 | (result / mass_fraction2 / (sample_mass * fac2)) < 0.001)) {
    unit <- switch(unit, "pg" = "fg", "ng" = "pg", "\u00b5g" = "ng")
    fac <- 10^3
    unit2 <- switch(unit,
                    "fg" = "ppt",
                    "pg" = "ppb",
                    "ng" = "ppm")
    fac2 <- 1
  } else {
    fac <- 1
  }
  out <- cbind(
    peak_data,
    "Analyte mass as element [unit]" = result * fac,
    "Analyte mass [unit]" = result / mass_fraction2 * fac,
    "Mass fraction" = mass_fraction2,
    "Sample mass [mg]" = sample_mass,
    "Content as element [unit2]" = result / (sample_mass * fac2) * fac,
    "Content as analyte [unit2]" = result / mass_fraction2 / (sample_mass * fac2) * fac
  )
  if (all(mass_fraction2==1)) out <- out[,!(colnames(out) %in% c("Analyte mass [unit]", "Mass fraction", "Content as analyte [unit2]")),drop=FALSE]

  colnames(out) <- gsub("unit2", unit2, colnames(out))
  colnames(out) <- gsub("unit", unit, colnames(out))

  return(out)
}
