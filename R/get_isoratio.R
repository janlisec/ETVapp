#' $$VS: Peak picking method "Peak (height)" returns peak max as peak start and end.
#' @title get_isoratio
#' @description \code{get_isoratio} calculate an isotope ratio based on peak areas.
#' @details Time-resolved ICP-MS data will be integration and optionally baseline corrected. 
#' Peak boundaries will be checked regarding the similarity beween the isotopes.
#' @param data A data.frame containing at least two columns.
#' @param iso1_col Spike isotope.
#' @param iso2_col Sample isotope.
#' @param PPmethod Peak picking method.
#' @param peak_start Value which is taken as peak start point, when manual peak picking is chosen.
#' @param peak_end Value which is taken as peak end point, when manual peak picking is chosen.
#' @param minpeakheight A threshold value for peak picking via peakheight.
#' @param BLmethod Method for baseline correction.
#' @param deg Degree of polynomial for baseline correction.
#' @param cf A correction value for cutting the area around the detected peak.
#' @param fl Filter length of smoother, has to be odd and >=3.
#' @return A data.frame.
#' @examples
#' # for sample measurements
#' lapply(ETVapp::ETVapp_testdata[["IDMS"]], function(x) {
#'   print(ldply_base(1:length(x), function(i) {
#'     get_isoratio(
#'      x[[i]], iso1_col = "113Cd", iso2_col = "111Cd", PPmethod = "Peak (manual)",
#'      peak_start = 72, peak_end = 132
#'     )
#'   }))
#' })
#' @export

get_isoratio <- function (data, iso1_col, iso2_col, PPmethod = c("Peak (height)", "Peak (manual)"),
                          peak_start, peak_end, minpeakheight = 1000,
                          BLmethod = c("modpolyfit", "none"), deg = 1, cf = 50, fl = 5) {

  # process data with IDMS workflow
  pro_data <- process_data(data = data, wf = "IDMS", c1 = iso1_col, c2 = iso2_col, fl = fl)

  iso_peak <- ldply_base(c(iso1_col, iso2_col), function(i) {
    get_peakdata(pro_data, PPmethod = PPmethod, int_col = i, peak_start = peak_start, peak_end = peak_end,
                 minpeakheight = minpeakheight, BLmethod = BLmethod, deg = deg, cf = cf)
  })
  ##Checks
  if (diff(c(iso_peak[1,2], iso_peak[2,2])) > 5 & diff(c(iso_peak[1,3], iso_peak[2,3])) > 5) {
    warning("Different peak boundaries for the isotopes found. Please check the integration.
            Complete peak integration is necessary for an accurate isotope ratio determination.")
  }

  #Isotope table
  iso_table <- data.frame(
    "Spike isotope" = iso1_col,
    "Sample isotope" = iso2_col,
    "R_m" = iso_peak[1,4] / iso_peak[2,4],
    check.names = FALSE
  )

  return(iso_table)
}
