#' @title read_clipboard
#' @description \code{read_clipboard} is a Shiny module which provides
#'     tabular copy paste from Excel to a Shiny-App via a textAreaInput
#'     element.
#' @details The module will render a button initially. This button (when
#'     clicked) will open a textAreaInput. Here the user can paste a
#'     tabular range from i.e. Excel and either upload this data as
#'     data.frame to the app or cancel the operation.
#' @param btn_txt The label for the button which opens the textAreaInput.
#' @param value Default value of textAreaInput.
#' @param check_dims Set TRUE to preserve the dimensions of the default value.
#' @return A data.frame containing the converted string from the textAreaInput.
#' @examples
#' \dontrun{
#' shinyApp(
#'   ui = shiny::fluidPage(read_clipboard_UI(id = "test")),
#'   server = function(input, output, session) {
#'     rv <- shiny::reactiveValues(df = data.frame(x=1:3, y=4:6))
#'     tmp <- read_clipboard_Server(id = "test", value=shiny::reactive(rv$df))
#'     observeEvent(tmp$d, {
#'       rv$df <- tmp$d
#'       print(rv$df)
#'     })
#'   }
#' )
#' }
#' @keywords internal
#' @noRd
read_clipboard_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::actionButton(inputId = ns("btn_main"), label = "btn_main"),
    shiny::uiOutput(outputId = ns("area_input"))
  )
}

read_clipboard_Server <- function(id, btn_txt="Paste from clipboard<br>(show textArea Input)", value=shiny::reactiveVal(""), check_dims = FALSE) {
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    shinyjs::hide(id = "area_input")
    shiny::updateActionButton(inputId = "btn_main", label = btn_txt)
    out <- reactiveValues(d = NULL, counter = 0)

    Err_Msg <- function(test=FALSE, message="Open Error Modal when test==FALSE", type=c("Error", "Info")[1]) {
      if (!test) {
        shiny::showModal(shiny::modalDialog(HTML(message), title = type, easyClose = TRUE))
        if (type=="Error") shiny::validate(shiny::need(expr = test, message = message, label = "Err_Msg"))
      } else {
        invisible(NULL)
      }
    }

    output$area_input <- shiny::renderUI({
      nr <- nrow(value())
      nc <- ncol(value())
      shiny::tagList(
        shiny::textAreaInput(
          inputId = ns("txt_textAreaInput"),
          label = "",
          value = "",
          placeholder = paste0(
            "copy/paste a numeric Excel range",
            ifelse(is.null(nc), "", paste(" of", nc, "columns")),
            ifelse(!is.null(nc) & !is.null(nr), " and ", ""),
            ifelse(is.null(nr), "", paste(" of", nr, "rows")),
            " (or upload empty to reset)"
          ),
          width = "100%",
          rows = ifelse(is.null(nr), 6, min(nr, 12))
        ),
        bslib::layout_columns(
          actionButton(ns("btn_textAreaInput"), "Upload"),
          actionButton(ns("btn_textAreaInput2"), "Cancel")
        )
      )
    })

    shiny::observeEvent(input$btn_main, {
      shiny::updateTextAreaInput(inputId = "txt_textAreaInput", value = ifelse(!is.null(dim(value())), paste(apply(value(),1,paste,collapse="\t"),collapse="\n"), ""))
      shinyjs::hide(id = "btn_main")
      shinyjs::show(id = "area_input")
    })

    shiny::observeEvent(input$btn_textAreaInput2, {
      shinyjs::hide(id = "area_input")
      shinyjs::show(id = "btn_main")
    })

    shiny::observeEvent(input$btn_textAreaInput, {
      nr <- nrow(value())
      nc <- ncol(value())
      # read clipboard
      tmp <- strsplit(input$txt_textAreaInput, "\n")[[1]]
      if (length(tmp)==0) {
        # reset output to default value ""
        tmp <- ""
      } else {
        # correct potential error for last col being empty
        tmp <- gsub("\t$", "\t\t", tmp)

        # remove empty rows
        tmp <- tmp[tmp != ""]

        if (!is.null(nr) & check_dims) {
          Err_Msg(test = length(tmp)==nr, message = paste("The pasted data appears to have not exactly", nr, "rows"))
        }

        # split at "\t" and/or " " and ensure equal length
        tmp <- strsplit(tmp, "[\t ]")
        Err_Msg(test = length(unique(sapply(tmp, length)))==1, message = "The clipboard content appears to have differing number of columns. Each row should contain the same number of tabulators or empty character spaces.")

        # convert to numeric (what is expected by downstream functions)
        # tmp <- plyr::laply(tmp, function(x) {
        #   x <- try(as.numeric(x))
        # }, .drop = FALSE)
        tmp <- laply_base(tmp, function(x) {
          x <- try(as.numeric(x))
        }, .drop = FALSE)
        Err_Msg(test = all(is.finite(tmp)), message = "The clipboard content did contain missing values or non-numeric cells<br>(now converted to NA)", type="Info")

        # ensure n columns
        if (!is.null(nc) & check_dims) {
          Err_Msg(test = ncol(tmp)==nc, message = paste("The pasted data appears to have not exactly", nc, "columns"))
        }
      }
      out$d <- tmp
      out$counter <- out$counter+1
      shinyjs::hide(id = "area_input")
      shinyjs::show(id = "btn_main")
    }, ignoreInit = TRUE)

    return(out)

  })
}
