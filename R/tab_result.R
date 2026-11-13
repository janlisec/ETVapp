#' Tabulate sample data
#'
#' Computes result data from a data.frame containing peak and calibration data (ExtCal/ExtGasCal),
#'  or the direct input of an analyte mass as element (IDMS/oIDMS). Unit assignment is
#'  realized through the input of the standard or stock solution concentration unit.
#'  The input of a standard mass will compute the analyte content in the sample. For
#'  moleculare and crystalline structures, the results are additionally calculated
#'  "as analyte" by the input of a mass fraction.
#'
#' @details For results < 0.001, a unit switch to the next lower order of magnitude is
#'            implemented. A data.frame, e.g., computed by \code{get_peakdata}, is required for
#'            peak data even if the result data is provided through an other parameter.
#'
#' @param peak_data Data.frame containing peak information.
#' @inheritParams tab_LOX
#' @param a Intercept from a calibration model (for ExtCal/ExtGasCal).
#' @param b Slope from a calibration model (for ExtCal/ExtGasCal).
#' @param K K (for IDMS).
#' @param amae Analyte mass as element (for IDMS/oIDMS).
#' @return A data.frame.
#'
#' @examples
#' # wf "ExtCal", "ExtGasCal" use col = 4
#' pd <- data.frame("A"=1:3,"B"=1:3,"R_m"=1:3,"col4"=1:3)
#' tab_result(peak_data=pd, wf="ExtCal")
#' # wf "IDMS", "oIDMS" use col = "R_m"
#' tab_result(peak_data=pd, wf="IDMS", c_sp_unit="g/L")
#' tab_result(peak_data=pd, wf="IDMS", c_sp_unit="mg/L")
#' tab_result(peak_data=pd, wf="IDMS", c_sp_unit="mg/L", mass_fraction2 = 0.75)
#' tab_result(peak_data=pd, wf="IDMS", c_sp_unit="mg/L", mass_fraction2 = 0.5)
#'
#' @export
tab_result <- function(peak_data, wf = c("ExtCal", "ExtGasCal", "IDMS", "oIDMS"),
                       ExtCal_unit = c("pg", "ng", "\u00b5g"),
                       ExtGasCal_unit = c("nL/min", "\u00b5L/min", "mL/min"),
                       c_sp_unit = c("\u00b5g/L", "mg/L", "g/L"),
                       a = 0, b = 1, K = 1, amae = 1, mass_fraction2 = 1, sample_mass = 1) {
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
      peak_data[,"R_corr"] <- peak_data[,"R_m"] * K
    }
    if (wf == "oIDMS") {
      peak_data <- peak_data[,!(colnames(peak_data) %in% c("Isotope", "Area [cts]"))]
    }
  }
  sample_mass <- rep(sample_mass, length.out=length(result))
  if (unit2 == "g/100 g") { fac2 <- 10 } else { fac2 <- 1 }

  mass_fraction2 <- rep(mass_fraction2, length.out=length(result))
  if (any(result < 0.001 | (result / (sample_mass * fac2)) < 0.001)) {
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
