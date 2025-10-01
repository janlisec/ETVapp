#' @title style_tab_peaks.
#' @description \code{style_tab_peaks} will .
#' @param data peak tab (works for both, IR and ID workflow).
#' @param IDMS Set true for ID workflow.
#' @return A datatable object.
#' @keywords internal
#' @noRd
style_tab_peaks <- function(data, IDMS = FALSE) {
  btn_list <- list(
    list(
      extend = 'csv',
      title = NULL,
      text = '<i class="fa fa-file-csv"></i>',
      titleAttr = 'Download table as .csv',
      filename = "Peaktable"
    ),
    list(
      extend = 'excel',
      title = NULL,
      text = '<i class="fa fa-file-excel-o"></i>',
      titleAttr = 'Download table as Excel',
      filename = "Peaktable"
    ),
    # list(
    #   extend = "pdf",
    #   text = 'change peak type',
    #   action = DT::JS("function ( e, dt, node, config ) { Shiny.setInputValue('ic_btn_peak_type', 1, {priority: 'event'}); }")
    # ),
    list(
      extend = "pdf",
      text = '<i class="fa fa-question"></i>',
      titleAttr = 'Get Help on table',
      action = DT::JS(paste0("function ( e, dt, node, config ) { Shiny.setInputValue('ic_help0", ifelse(IDMS, 9, 6), "', 1, {priority: 'event'}); }"))
    )
  )
  # editable <- list(target = "column", disable = list(columns = c(0:8,10)), numeric = 9)
  # if (IDMS) {
  #   btn_list <- btn_list[-c(3,4)]
  #   editable <- FALSE
  # }
  editable <- FALSE
  DT::datatable(
    data = limit_digits(data),
    "extensions" = "Buttons",
    "options" = list(
      "server" = FALSE,
      "dom" = "Bt",
      "autoWidth" = TRUE,
      # "columnDefs" = list(
      #   list(width = '80px', targets = 0:(ncol(data)-2))
      # ),
      "paging" = FALSE,
      "ordering" = FALSE,
      "pageLength" = -1,
      "buttons" = btn_list
    ),
    "selection" = list(mode="single", target="row"),
    "editable" = editable,
    "rownames" = NULL
  )
}

