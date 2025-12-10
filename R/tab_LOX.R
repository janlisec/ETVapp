#' @title tab_LOX
#' @description \code{tab_LOX} estimates the limit of detection (LOD) and
#'     quantification (LOQ) and returns a data.frame.
#' @details The LOD and LOQ will be calculated as three and ten times the
#'     standard deviation of at least three peak areas divided by the slope
#'     of a linear calibration curve (for workflow ExtCal/ExtGasCal). Results are provided
#'     as element, analyte and relative to a sample mass.
#' @param x Variable containing the numeric data to infer SD (either peak area (for ExtCal/ExtGasCal) or analyte mass (for IDMS/oIDMS)).
#' @param cali_slope A slope from the calibration data.
#' @param wf Calibration method/Workflow.
#' @param mass_fraction2 Mass fraction of the analyte element in the analyte component.
#' @param sample_mass Sample mass.
#' @param ExtCal_unit The measurement unit of the ExtCal workflow to correctly format the output.
#' @param ExtGasCal_unit The measurement unit of the ExtGasCal_unit workflow to correctly format the output.
#' @param c_sp_unit The measurement unit of the "IDMS", "oIDMS" workflow to correctly format the output.
#'
#' @return A data.frame.
#'
#' @examples
#' tab_LOX(x = runif(2))
#' tab_LOX(x = runif(5), wf = "IDMS")
#' tab_LOX(x = runif(10), mass_fraction2 = 0.5, sample_mass = 3, ExtCal_unit = "ng")
#'
#' @export
tab_LOX <- function(
    x, cali_slope = 1, wf = c("ExtCal", "ExtGasCal", "IDMS", "oIDMS"),
    ExtCal_unit = c("pg", "ng", "\u00b5g"),
    ExtGasCal_unit = c("nL/min", "\u00b5L/min", "mL/min"),
    c_sp_unit = c("\u00b5g/L", "mg/L", "g/L"),
    mass_fraction2 = 1, sample_mass = 1
) {
  # Checks
  wf <- match.arg(wf)
  ExtCal_unit <- match.arg(ExtCal_unit)
  ExtGasCal_unit <- match.arg(ExtGasCal_unit)
  c_sp_unit <- match.arg(c_sp_unit)
  #browser()
  ExtGasCal_unit <- switch(
    ExtGasCal_unit,
    "nL/min" = "pg",
    "\u00b5L/min" = "ng",
    "mL/min" = "\u00b5g"
  )
  c_sp_unit <- switch(
    c_sp_unit,
    "\u00b5g/L" = "pg",
    "mg/L" = "ng",
    "g/L" = "\u00b5g"
  )
  unit <- switch(
    wf,
    "ExtCal" = ExtCal_unit,
    "ExtGasCal" = ExtGasCal_unit,
    "IDMS" = c_sp_unit,
    "oIDMS" = c_sp_unit
  )
  unit2 <- switch(
    unit,
    "pg" = "ppb",
    "ng" = "ppm",
    "\u00b5g" = "g/100 g"
  )

  if (wf %in% c("IDMS", "oIDMS")) cali_slope <- 1

  ensure_that(is.numeric(cali_slope), "Calibration data (and derived slope) is required for estimating the LOD and LOQ.")

  # Calculation of SD
  n <- sum(is.finite(x))
  if (n < 3) {
    ensure_that(n >= 3, "At least three finite values are needed for a statistical evaluation.", opt = "warn")
    SD <- NA
  } else {
    ensure_that(n >= 10, "At least ten blank values are recommended for estimating the LOD and LOQ.", opt = "message")
    SD <- stats::sd(x, na.rm=TRUE)
  }

  #Calculation of LOD/LOQ
  LOD <- 3 * SD / cali_slope
  LOQ <- 10 * SD / cali_slope

  mass_fraction2 <- rep(mass_fraction2, length.out=length(LOD))
  sample_mass <- rep(sample_mass, length.out=length(LOD))
  if (unit2 == "g/100 g"){fac2 <- 10} else {fac2 <- 1}

  out <- data.frame(
    "LOD as element [unit]" = LOD,
    "LOQ as element [unit]" = LOQ,
    "Mass fraction" = mass_fraction2,
    "Sample mass [mg]" = sample_mass,
    "LOD as analyte [unit]" = LOD / mass_fraction2,
    "LOQ as analyte [unit]" = LOQ / mass_fraction2,
    "LOD per sample mass [unit2]" = LOD / (sample_mass * fac2),
    "LOQ per sample mass [unit2]" = LOQ / (sample_mass * fac2),
    check.names = FALSE
  )

  if (all(mass_fraction2==1)) out <- out[,!(colnames(out) %in% c("Mass fraction", "LOD as analyte [unit]", "LOQ as analyte [unit]")),drop=FALSE]

  # substitute unit in colnames
  colnames(out) <- gsub("unit2", unit2, colnames(out))
  colnames(out) <- gsub("unit", unit, colnames(out))

  return(out)
}
