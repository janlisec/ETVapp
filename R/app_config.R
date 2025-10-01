#' Access files in the current app
#'
#' @param ... character vectors, specifying sub directory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @keywords internal
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "ETVapp")
}

#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config R_CONFIG_ACTIVE, if unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file
#'
#' @keywords internal
#' @noRd
get_app_config <- function(
  value,
  config = Sys.getenv("R_CONFIG_ACTIVE", "default"),
  file = app_sys("app-config.yml"),
  use_parent = TRUE
) {
  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @keywords internal
#' @noRd
add_external_resources <- function() {
  # set pointer to www
  shiny::addResourcePath('www', app_sys('app/www'))
  # return Head information
  shiny::tags$head(
    shiny::tags$title(get_app_config("app_name")),
    shiny::tags$link(rel = "shortcut icon", href = "www/BAMLogo.ico"),
    # shiny::tags$style(
    #   shiny::HTML("
    #     .card-body {
    #       overflow: visible !important;
    #     }
    #     .form-group {
    #       overflow: visible !important;
    #     }
    #   ")
    # ),
    if (get_app_config("bam_server")) {
      list(
        shiny::HTML('<noscript><p><img src="https://agw1.bam.de/piwik/matomo.php?idsite=24&amp;rec=1" style="border:0;" alt="" /></p></noscript>'),
        shiny::HTML('<script type="text/javascript" src="https://agw1.bam.de/piwik/piwik.js" async defer></script>'),
        shiny::includeScript(app_sys("app/www/js/tracking-live.js"))
      )
    }
  )
}

#' @title app_status_line
#' @description Returns a status line for the bottom of the app.
#' @return HTML with info regarding app version.
#' @keywords internal
#' @noRd
app_status_line <- function() {
  shiny::HTML(
    "ver.", get_app_config("app_version"),
    " | ", get_app_config("app_date"),
    " | <a href='mailto:jan.lisec@bam.de'>jan.lisec@bam.de</a>",
    ifelse(get_app_config("bam_server"), '| <a href="https://www.bam.de/Navigation/EN/Services/Privacy-Policy/privacy-policy.html" target="_blank" rel="noopener noreferrer">BAM Privacy policy</a>', '')
  )
}
