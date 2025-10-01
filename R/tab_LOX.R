#' @title tab_LOX
#' @description \code{tab_LOX} estimates the limit of detection (LOD) and
#'     quantification (LOQ) and returns a data.frame.
#' @details The LOD and LOQ will be calculated as three and ten times the
#'     standard deviation of at least three peak areas divided by the slope
#'     of a linear calibration curve (for workflow ExtCal). Results are provided
#'     as element, analyte and relative to a sample mass.
#' @param x Variable containing the numeric data to infer SD (either peak area (for ExtCal) or analyte mass (for IDMS)).
#' @param cali_slope A slope from the calibration data.
#' @param wf Calibration method/Workflow.
#' @param mass_fraction2 Mass fraction of the analyte element in the analyte component.
#' @param sample_mass Sample mass.
#'
#' @return A data.frame.
#'
#' @examples#'
#' tab_LOX(x = runif(2))
#' tab_LOX(x = runif(5), wf = "IDMS")
#' tab_LOX(x = runif(10), mass_fraction2 = 2, sample_mass = 3)
#'
#' @export
tab_LOX <- function(x, cali_slope = 1, wf = c("ExtCal", "ExtGasCal", "IDMS", "oIDMS"), mass_fraction2 = 1, sample_mass = 1) {
  # Checks
  wf <- match.arg(wf)

  # ensure that cali_slope = 1 for workflows without cali
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

  #LOD/LOQ table
  # $$JL this would be the default table
  # consider adding function parameters for u and fac or keep decision dependent on wf
  # u <- c("ng", "\u00b5g")
  # out <- as.data.frame(
  #   as.list(setNames(rep(as.numeric(NA),8),
  #   c("LOD as element [u]", "LOQ as element [u]", "Mass fraction", "Sample mass [mg]", "LOD as analyte [u]", "LOQ as analyte [u]", "LOD per sample mass [ppm]", "LOQ per sample mass [ppm]"))),
  #   check.names = FALSE
  # )
  # gsub("[[]u[]]", paste0("[",u,"]"), colnames(out))

  # $$JL func param 'unit' -> mug, ng, pg
  if (wf == "ExtCal") {
    out <- data.frame(
      "LOD as element [ng]" = LOD, #in ng
      "LOQ as element [ng]" = LOQ,
      "Mass fraction" = mass_fraction2,
      "Sample mass [mg]" = sample_mass,
      "LOD as analyte [ng]" = LOD / mass_fraction2,
      "LOQ as analyte [ng]" = LOQ / mass_fraction2,
      "LOD per sample mass (ppm)" = LOD / sample_mass,
      "LOQ per sample mass (ppm)" = LOQ / sample_mass,
      check.names = FALSE
    )
  }
  if (wf == "ExtGasCal") {
    out <- data.frame(
      "LOD as element [\u00b5g]" = LOD, #in µg
      "LOQ as element [\u00b5g]" = LOQ,
      "Mass fraction" = mass_fraction2,
      "Sample mass [mg]" = sample_mass,
      "LOD as analyte [\u00b5g]" = LOD / mass_fraction2,
      "LOQ as analyte [\u00b5g]" = LOQ / mass_fraction2,
      "LOD per sample mass (ppm)" = LOD * 1000 / sample_mass,
      "LOQ per sample mass (ppm)" = LOQ * 1000 / sample_mass,
      check.names = FALSE
    )
  }
  if (wf %in% c("IDMS", "oIDMS")) {
    out <- data.frame(
      "LOD as element [ng]" = LOD / 1000, #in ng
      "LOQ as element [ng]" = LOQ / 1000,
      "Mass fraction" = mass_fraction2,
      "Sample mass [mg]" = sample_mass,
      "LOD as analyte [ng]" = LOD / 1000 / mass_fraction2,
      "LOQ as analyte [ng]" = LOQ / 1000 / mass_fraction2,
      "LOD per sample mass [ppm]" = LOD / 1000 / sample_mass,
      "LOQ per sample mass [ppm]" = LOQ / 1000 / sample_mass,
      check.names = FALSE
    )
  }
  return(out)
}
