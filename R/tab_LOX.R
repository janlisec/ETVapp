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
#' @param unit The measurement unit of the workflow to correctly format the output.
#'
#' @return A data.frame.
#'
#' @examples#'
#' tab_LOX(x = runif(2))
#' tab_LOX(x = runif(5), wf = "IDMS")
#' tab_LOX(x = runif(10), mass_fraction2 = 2, sample_mass = 3, unit = "ng")
#'
#' @export
tab_LOX <- function(x, cali_slope = 1, wf = c("ExtCal", "ExtGasCal", "IDMS", "oIDMS"), mass_fraction2 = 1, sample_mass = 1, unit = c("pg", "ng", "\u00b5g")) {
  # Checks
  wf <- match.arg(wf)
  unit <- match.arg(unit)

  # ensure that cali_slope = 1000 for workflows without cali
  # this reflects Vera's Code where LOD and LOQ where devided by 1000 in output table
  if (wf %in% c("IDMS", "oIDMS")) cali_slope <- 1000

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

  out <- data.frame(
    "LOD as element [unit]" = LOD,
    "LOQ as element [unit]" = LOQ,
    check.names = FALSE
  )

  # attach 'sample_mass' columns if different from 1
  if (!all(mass_fraction2==1)) {
    out <- cbind(out, data.frame(
      "Mass fraction" = mass_fraction2,
      "LOD as analyte [unit]" = LOD / mass_fraction2,
      "LOQ as analyte [unit]" = LOQ / mass_fraction2,
      check.names = FALSE
    ))
  }

  # substitute unit in colnames
  colnames(out) <- gsub("unit", unit, colnames(out))

  # attach 'Mass fraction' columns if different from 1
  if (!all(sample_mass==1)) {
    fac <- switch(unit, "pg" = 10^-3, "ng" = 1, "\u00b5g" = 10^3)
    out <- cbind(out, data.frame(
      "Sample mass [mg]" = sample_mass,
      "LOD per sample mass [ppm]" = LOD * fac / sample_mass,
      "LOQ per sample mass [ppm]" = LOQ * fac / sample_mass,
      check.names = FALSE
    ))
  }

  return(out)
}
