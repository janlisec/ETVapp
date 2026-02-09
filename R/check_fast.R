#' @keywords internal
#' @noRd
check_fast <- function() {
  devtools::check(
    vignettes = FALSE,
    args = "--no-manual"
  )
}
