#' @title style_tab_peaks.
#' @description \code{style_tab_peaks} will create an HTML table widget.
#' @param data peak tab.
#' @return A datatable object.
#' @keywords internal
#' @noRd
style_tab_peaks <- function(data) {
  editable <- list(target = "cell", disable = list(columns = c(0:1,4:5)), numeric = 2:3)
  DT::datatable(
    data = limit_digits(data),
    "options" = list(
      "server" = FALSE,
      "dom" = "t",
      "autoWidth" = TRUE,
      "paging" = FALSE,
      "ordering" = FALSE,
      "pageLength" = -1
    ),
    "selection" = list(mode="single", target="row"),
    "editable" = editable,
    "rownames" = NULL
  )
}

