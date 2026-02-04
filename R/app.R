#' @title app.
#'
#' @description \code{app} will start a shiny app that allows to upload raw
#'  data, process selectively and analyze different methods of ratio calculation
#'  between two intensity traces.
#'
#' @details The app is described in detail in \doi{10.1039/D2JA00208F}.
#'
#' @return A shiny app object. This will effectively launch a browser and start
#'   the app on local port 7462.
#'
#' @import shiny
#' @export
app <- function() {
  # these options to shinyApp() could be made available to app() in the future if required
  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    onStart = NULL,
    options = list("port" = 7462, "display.mode" = "normal", "launch.browser" = TRUE),
    enableBookmarking = "disable",
    uiPattern = "/"
  )
}

# ================================
# set up app UI ----
app_ui <- function() {

  # card components
  # data source
  card_data_source <- shiny::tagList(
    bslib::card(
      bslib::card_header(
        shiny::actionLink(inputId = "ic_help02", label = "Data"),
      ),
      bslib::layout_column_wrap(
        width = 120,
        shinyjs::disabled(radioButtons(inputId = "ic_par_libsource", label = "Data source", choices = c("Upload files", "Testdata"), selected = "Testdata")),
        radioButtons(inputId = "par_wf", label = "Workflow", choices = c("ExtCal","ExtGasCal","IDMS","oIDMS"), selected = c("ExtCal","ExtGasCal","IDMS","oIDMS")[1]),
        radioButtons(
          inputId = "par_filetype", label = "File Type",
          choices = list("Cali" = "Cali", "Mass bias" = "Massbias", "Samples" = "Samples", "Blanks" = "Blanks", "sp: Ionic" = "sp_ionic", "sp: Particles" = "sp_particle"), selected = "Cali")
      ),
      uiOutput(outputId = "ic_par_path_expfiles")
    )
  )
  # import parameters
  ExtIDMS_import <- shiny::div(
    id = "ExtIDMS_import",
    bslib::layout_column_wrap(
      width = 120, gap = "5px",
      shiny::HTML("Isotopes:"),
      selectInput(inputId = "ic_par_mi_col", label = "1", choices = c("")), #|> bslib::tooltip("Select Spike Isotope column."),
      selectInput(inputId = "ic_par_si_col", label = "2", choices = c("")), #|> bslib::tooltip("Select Sample Isotope column."),
      #shinyjs::hidden(textInput(inputId = "ic_par_mi_rt_unit", label = "RT unit", value = "s")),
      #shinyjs::hidden(selectInput(inputId = "ic_par_rt_col", label = "", choices = "Time")), #|> bslib::tooltip("Select RT column."),
      shiny::HTML("Labels:"),
      textInput(inputId = "ic_par_mi_col_name", label = ""),
      textInput(inputId = "ic_par_si_col_name", label = ""),
    )
  )

  IDMS_import <- shiny::div(
    id = "IDMS_import",
    bslib::layout_column_wrap(
      width = 120, gap = "5px",
      shiny::HTML("Natural abundances:"),
      numericInput(inputId = "ic_par_mi_amu", label = "", value = 0, max = 1, step = 0.0001),
      numericInput(inputId = "ic_par_si_amu", label = "", value = 0, max = 1, step = 0.0001)
    )
  )

  card_import <- shiny::tagList(
    bslib::card(
      id = "Import_par_section",
      bslib::card_header(shiny::actionLink(inputId = "ic_help03", label = "Import")),
      ExtIDMS_import,
      IDMS_import
    )
  )

  #card_import <- shiny::tagList(
    #bslib::card(
      #id = "Import_par_section",
      #bslib::card_header(shiny::actionLink(inputId = "ic_help03", label = "Import")),
      #bslib::layout_column_wrap(
        #width = 120, gap = "5px",
        #shinyjs::hidden(selectInput(inputId = "ic_par_rt_col", label = "RT column", choices = "Time")) |> bslib::tooltip("Select RT column."),
        #shiny::HTML("Isotopes:"),
        #selectInput(inputId = "ic_par_mi_col", label = "1", choices = c("")), #|> bslib::tooltip("Select Spike Isotope column."),
        #selectInput(inputId = "ic_par_si_col", label = "2", choices = c("")), #|> bslib::tooltip("Select Sample Isotope column."),
        #shinyjs::hidden(textInput(inputId = "ic_par_mi_rt_unit", label = "RT unit", value = "s")),
        #shiny::HTML("Labels:"),
        #textInput(inputId = "ic_par_mi_col_name", label = ""),
        #textInput(inputId = "ic_par_si_col_name", label = ""),

      #)
    #)
  #)
  card_processing <- shiny::tagList(
    bslib::card(
      id = "Processing_par_section",
      bslib::card_header(shiny::actionLink(inputId = "ic_help04", label = "Processing")),
      bslib::layout_column_wrap(
        width = 120, gap = "5px",
        numericInput(inputId = "smoothing_fl", label = "Smoothing", value = 9, min=1, max=151, step=2) |> bslib::tooltip("Smoothing parameter 'filter length'. Set to '1' to omit this processing step. Set to '151' to use smooth.spline()."),
        selectInput(inputId = "peak_method", label = "Peak Method", choices = c("Peak (height)", "Peak (manual)", "mean signal"), selected = "Peak (manual)"), #|> bslib::tooltip("Select method for Peak picking."),
        shiny::HTML(""),
        shiny::HTML("Peak (height):"),
        numericInput(inputId = "peak_height", label = "Threshold", value = 1000, min=0, step=1) |> bslib::tooltip("Peak picking parameter: peak_height."),
        shiny::HTML(""),
        shiny::HTML("Peak(manual)/ mean signal:"),
        numericInput(inputId = "peak_start", label = "Start", value = 70, min=0, step=1) |> bslib::tooltip("Peak picking parameter: peak_start."),
        numericInput(inputId = "peak_end", label = "End", value = 105, min=1, max=1000, step=1) |> bslib::tooltip("Peak picking parameter: peak_end."),
        shiny::HTML("Baseline correction:"),
        selectInput(inputId = "baseline_method", label = "Method", choices = c("none", "modpolyfit"), selected = "modpolyfit"), #|> bslib::tooltip("Select method for baseline estimation or 'none' to omit this processing step."),
        numericInput(inputId = "cf", label = "Correction factor", value = 50, min=0, max=1000, step=1) #|> bslib::tooltip("Peak picking parameter: peak_end."),
      )
    )
  )
  IDMS_pars_common <- shiny::div(
    id = "IDMS_pars_common",
    bslib::layout_column_wrap(
      width = 120, gap = "5px",
      #shinyjs::hidden(numericInput(inputId = "As_iso1", label = "As_iso1", value = 12.22)),
      #shinyjs::hidden(numericInput(inputId = "As_iso2", label = "As_iso2", value = 12.8)),
      #shinyjs::hidden(numericInput(inputId = "K", label = "K", value = 1)),
      shiny::HTML("Abundances:"),
      numericInput(inputId = "Asp_iso1", label = "Isotope 1", value = 92.61),
      numericInput(inputId = "Asp_iso2", label = "Isotope 2", value = 0.22),
      shiny::HTML("Stock solution:"),
      numericInput(inputId = "c_sp", label = "Concentration", value = 2.5),
      selectInput(inputId = "c_sp_unit", label = "Unit", choices = c("\u00b5g/L", "mg/L", "g/L"))
    )
  )
  IDMS_pars <- shiny::div(
    id = "IDMS_pars",
    bslib::layout_column_wrap(
      width = 120, gap = "5px",
      shiny::HTML("Added spike solution:"),
      numericInput(inputId = "VF1", label = "Dilution factor", value = 1000),
      numericInput(inputId = "V_sp", label = "Volume", value = 6),
      shiny::HTML("Molar Masses:"),
      numericInput(inputId = "M_sp", label = "Spike", value = 113.01),
      numericInput(inputId = "M_sa", label = "Sample", value = 112.41),
      shiny::HTML(""),
      shinyjs::disabled(numericInput(inputId = "N_sp", label = "Spike amount", value = 1))
    )
  )
  oIDMS_pars <- shiny::div(
    id = "oIDMS_pars",
    bslib::layout_column_wrap(
      width = 120, gap = "5px",
      shiny::HTML("On-line spike:"),
      numericInput(inputId = "DF", label = "Dilution factor", value = 20),
      numericInput(inputId = "V_fl", label = "Volume flow", value = 0.0075),
      shiny::HTML("Particle standard:"),
      selectInput(inputId = "part_mat", label = "Material", choices = c("Au", "Ag"), selected = "Au"),
      numericInput(inputId = "dia_part", label = "Diameter", value = 60),
      shiny::HTML("sp-Parameter:"),
      numericInput(inputId = "t_fltr", label = "Time filter", value = 60),
      numericInput(inputId = "LFD", label = "Detection limit", value = 20000),
      shiny::HTML("Transport efficieny [%]"),
      shinyjs::disabled(numericInput(inputId = "trans_eff", label = "", value = 0))
    )
  )
  ExtIDMS_pars_common <- shiny::div(
    id = "ExtIDMS_pars_common",
    bslib::layout_column_wrap(
      width = 120, gap = "5px",
      shinyjs::hidden(numericInput(inputId = "mass_fraction2", label = "Mass fraction", value = 1, min = 10^-6, max = 1, step = 10^-6)),
      shinyjs::hidden(numericInput(inputId = "sample_mass", label = "Sample mass [mg]", value = 1, min = 1)),
      shiny::HTML("")
    )
  )
  ExtCal_pars <- shiny::div(
    id = "ExtCal_pars",
    bslib::layout_column_wrap(
      width = 120, gap = "5px",
      shiny::HTML("Standard:"),
      selectInput(inputId = "ExtCal_unit", label = "Unit", choices = c("pg","ng","\u00b5g")),
      shiny::HTML("")
    )
  )
  ExtGasCal_pars <- shiny::div(
    id = "ExtGasCal_pars",
    bslib::layout_column_wrap(
      width = 120, gap = "5px",
      shiny::HTML("Calibration gas:"),
      numericInput(inputId = "cali_fac", label = "Conversion factor", value = 0.005002692),
      selectInput(inputId = "ExtGasCal_unit", label = "Unit", choices = c("nL/min", "\u00b5L/min", "mL/min"))
    )
  )
  card_workflow_pars <- shiny::tagList(
    bslib::card(
      id = "wf_pars",
      bslib::card_header(shiny::actionLink(inputId = "ic_help10", label = "Workflow Parameters")),
      IDMS_pars_common,
      IDMS_pars,
      oIDMS_pars,
      ExtCal_pars,
      ExtGasCal_pars,
      ExtIDMS_pars_common
    )
  )

  main_menu_ui <- function() {
    shiny::tagList(
      shiny::div(style = "display: flex; flex-direction: column; height: calc(100vh - 114px);",
        shiny::div(
          style = "flex-grow: 0;",
          card_data_source,
          card_import,
          card_workflow_pars,
          card_processing
        ),
        div(style = "flex-grow: 1;"),
        bslib::card_footer(class = "d-flex justify-content-bottom", app_status_line())
      )
    )
  }

  ic_plot_card <- function() {
    bslib::card(
      id = "ic_plot_card",
      min_height = "450px",
      bslib::card_body(padding = 0, style = "resize: vertical;",
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            position = "right", open = "open", width = "280px", gap = "10px",
            bslib::input_switch("ic_par_isotope", "Show isotopes"),
            shinyWidgets::pickerInput(inputId = "ic_par_focus_sample", label = "", choices = "", multiple = TRUE, options = list(`actions-box` = TRUE)),
            shinyWidgets::pickerInput(inputId = "ic_par_focus_iso", label = "", choices = "", multiple = TRUE, options = list(`actions-box` = TRUE)),
            checkboxGroupInput(
              inputId = "ic_par_specplot",
              label = shiny::actionLink(inputId = "ic_help05", label = "Plot options"),
              choices = list(
                "show peak boundaries"="overlay_pb",
                "show sample IDs" = "overlay_legend",
                "show Temp program" = "overlay_Temp"
              ),
              selected = c("overlay_pb", "overlay_legend", "overlay_Temp")
            ),
            read_clipboard_UI(id = "T_prog") |> bslib::tooltip("Specify Temp program.")
          ),
          plotOutput(
            outputId = "ic_specplot",
            dblclick = dblclickOpts(id = "ic_specplot_dblclick"),
            brush = brushOpts(id = "ic_specplot_brush", direction = "xy", resetOnNew = TRUE)
          ) |> bslib::tooltip("You may select a time range [Click and Drag] with the cursor to zoom. Use [Double Click] to unzoom.", placement = "bottom")
        )
      )
    )
  }

  ic_tables_card <- function() {
    bslib::card(
      fill = FALSE,
      id = "ic_tables_card",
      bslib::card_header("Workflow results"),
      bslib::card_body(fillable = FALSE, fill = FALSE,
        div(
          style = "display: inline-block; width: auto; max-width: 100%; min-width: 400px; min-height: 3rem;",
          bslib::card_title("Mass bias/Cali peaks"),
          DT::DTOutput("table_cali")
        )
      ),
      bslib::card_body(fillable = FALSE, fill = FALSE,
        div(
          style = "display: inline-block; width: auto; max-width: 100%; min-width: 400px; min-height: 3rem;",
          bslib::card_title("Calibration model"),
          DT::DTOutput("table_cm")
        )
      ),
      bslib::card_body(fillable = FALSE, fill = FALSE,
        div(
          style = "display: inline-block; width: auto; max-width: 100%; min-width: 400px; min-height: 3rem;",
          bslib::card_title("Sample peaks"),
          DT::DTOutput("table_sam")
        )
      ),
      bslib::card_body(fillable = FALSE, fill = FALSE,
        div(
          style = "display: inline-block; width: auto; max-width: 100%; min-width: 400px; min-height: 3rem;",
          bslib::card_title("LOD/LOQ table"),
          DT::DTOutput("table_lox")
        )
      )
    )
  }

  shiny::tagList(
    add_external_resources(),
    shinyjs::useShinyjs(),
    bslib::page_sidebar(
      sidebar = bslib::sidebar(
        position = "left", open = "open", width = "520px",
        main_menu_ui()
      ),
      bslib::layout_columns(col_widths = c(9,3),
        ic_plot_card(),
        bslib::card(
          bslib::card_header("Current peaks"),
          DT::DTOutput("ic_table_peaks")
        ),
        ic_tables_card(),
        bslib::card(fill = FALSE,
          bslib::card_header("Additional plots"),
          bslib::card_body(fillable = FALSE, fill = FALSE,
            plotOutput(outputId = "cali_plot", height= "600px"),
            plotOutput(outputId = "sp_particle_plot", height= "350px"),
            plotOutput(outputId = "sp_particle_size_distribution_plot", height= "350px")
          )
        )
      ),
      title = bslib::card_title(
        style = "width: 100%; margin: 0px;",
        class = "d-flex justify-content-between align-items-center",
        shiny::div(
          img(src = "www/bam_logo_20pt.gif", alt="BAM Logo"),
          strong("BAM"), em("ETVapp"), " - automatic processing of ICP-MS data"
        ),
        shiny::actionLink(inputId = "ic_help01", label = NULL, icon = shiny::icon(name = "question-circle"))
      )
    )
  )

}
# ================================

# ================================
# Define server logic ----
app_server <- function(input, output, session) {

  ### setup Options ######################################################----
  # increase maximum file size for upload
  old_options <- options()
  on.exit(options(old_options))
  options(shiny.maxRequestSize=30*1024^2) # BrukerFlex Files are >5MB

  # store par() results
  # This is of course not useful in a shiny app, but was required from CRAN which
  # in turn led to problems on ShinyServer as par() opens the standard graphics device
  # which made the hack of pdf(NULL) neccessary... :(
  if (!get_app_config("bam_server")) {
    grDevices::pdf(NULL)
    old_par <- par(no.readonly = TRUE)
    grDevices::dev.off()
    on.exit(expr = { par(old_par) }, add = TRUE)
  }

  # load app data on app start
  testdata <- ETVapp::ETVapp_testdata
  isotopes <- ETVapp::isotopes

  output$ic_par_path_expfiles <- renderUI({
    # file input as renderUI to allow a reset in case that the upload method is changed
    message("output$ic_par_path_expfiles")
    fileInput(inputId = "ic_par_path_expfiles_inner", label = "Select Files", multiple = TRUE)
  })

  ### setup reactive Values ##############################################----
  # the editable peak table
  ic_table_peaks_edit <- shiny::reactiveVal()
  # setup initial plot range (min, max)
  spec_plots_xmin <- reactiveVal(0)
  spec_plots_xmax <- reactiveVal(320)
  spec_plots_ymin <- reactiveVal(0)
  spec_plots_ymax <- reactiveVal(300000)
  # the time range if cutting is applied
  #cut_range <- reactiveValues("min"=NULL, "max"=NULL)
  # the rt shift applied to samples for alignment
  #rt_shift <- reactiveVal(0)
  # indicator if range cut is currently applied
  #status_range_cut <- reactiveVal("off")

  pars <- shiny::reactiveValues(
    "smoothing_fl" = 7,
    "mass_frac" = 1,
    "std_info" = NULL,
    "amae" = NULL,
    "K" = NULL,
    "ExtCal_cali" = NULL,
    "ExtCal_cm" = NULL,
    "ExtCal_sam" = NULL,
    "ExtCal_lox" = NULL,
    "ExtGasCal_cali" = NULL,
    "ExtGasCal_cm" = NULL,
    "ExtGasCal_sam" = NULL,
    "ExtGasCal_lox" = NULL,
    "oIDMS_cali" = NULL,
    "oIDMS_cm" = NULL,
    "oIDMS_sam" = NULL,
    "oIDMS_lox" = NULL,
    "IDMS_cali" = NULL,
    #"IDMS_cm" = NULL,
    "IDMS_sam" = NULL,
    "IDMS_lox" = NULL,
    "T_prog" = "",
    "Iso_labels" = stats::setNames("Time","Time")
  )

  T_prog <- read_clipboard_Server(id = "T_prog", btn_txt="Set Temp program", value=shiny::reactive(pars$T_prog))

  observeEvent(T_prog$d, {
    if (!is.null(dim(T_prog$d))) {
      pars$T_prog <- stats::setNames(as.data.frame(T_prog$d), c("Time", "Temp"))
    } else {
      pars$T_prog <- ""
    }
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  shiny::observeEvent(input$smoothing_fl, {
    if (ensure_that(input$smoothing_fl %in% seq(1,151,2), "Smoothing parameter should be odd integer >=3 (or use '1' to omit smoothing).", opt = "warn")) {
      pars$smoothing_fl <- input$smoothing_fl
    } else {
      shiny::updateNumericInput(inputId = "smoothing_fl", value = pars$smoothing_fl)
    }
  })

  ### show/hide section ##################################################----
  # modify UI depending on workflow (IR-Delta or IDMS)
  observeEvent(input$par_wf, {
    shinyjs::toggleElement(id = "ExtIDMS_import", condition = input$par_wf %in% c("ExtCal","ExtGasCal","IDMS","oIDMS"))
    shinyjs::toggleElement(id = "IDMS_import", condition = input$par_wf %in% c("IDMS","oIDMS"))
    shinyjs::toggleElement(id = "ExtCal_pars", condition = input$par_wf=="ExtCal")
    shinyjs::toggleElement(id = "ExtGasCal_pars", condition = input$par_wf=="ExtGasCal")
    shinyjs::toggleElement(id = "ExtIDMS_pars_common", condition = input$par_wf %in% c("ExtCal","ExtGasCal","IDMS","oIDMS"))
    shinyjs::toggleElement(id = "IDMS_pars", condition = input$par_wf=="IDMS")
    shinyjs::toggleElement(id = "oIDMS_pars", condition = input$par_wf=="oIDMS")
    shinyjs::toggleElement(id = "IDMS_pars_common", condition = input$par_wf %in% c("IDMS","oIDMS"))
    tmp_listames <- list("Cali" = "Cali", "Mass bias" = "Massbias", "Samples" = "Samples", "Sample" = "Sample", "Blanks" = "Blanks", "sp: Ionic" = "sp_ionic", "sp: Particles" = "sp_particle")
    tmp <- list(
      "ExtCal"=tmp_listames[c(1,4,5)],
      "ExtGasCal"=tmp_listames[c(1,3,5)],
      "IDMS"=tmp_listames[c(2,3,5)],
      "oIDMS"=tmp_listames[c(2,6,7,3,5)]
    )
    shiny::updateRadioButtons(inputId = "par_filetype", choices = tmp[[input$par_wf]])
    if (input$par_wf %in% c("ExtCal", "ExtGasCal")) {
      updateSelectInput(inputId = "ic_par_mi_col", label = "Analyte")
      updateSelectInput(inputId = "ic_par_si_col", label = "Standard")
      updateSelectInput(inputId = "ic_par_mi_col_name", label = "")
      updateSelectInput(inputId = "ic_par_si_col_name", label = "")
      shinyjs::hide(id = "ic_par_mi_amu")
      shinyjs::hide(id = "ic_par_si_amu")
    } else {
      updateSelectInput(inputId = "ic_par_mi_col", label = "1")
      updateSelectInput(inputId = "ic_par_si_col", label = "2")
      updateSelectInput(inputId = "ic_par_mi_col_name", label = "")
      updateSelectInput(inputId = "ic_par_si_col_name", label = "")
      shinyjs::show(id = "ic_par_mi_amu")
      shinyjs::show(id = "ic_par_si_amu")
    }
  })

  shiny::observeEvent(input$par_filetype, {
    shinyjs::toggleElement(id = "sp_particle_size_distribution_plot", condition = input$par_filetype %in% c("sp_particle"))
    shinyjs::toggleElement(id = "sp_particle_plot", condition = input$par_filetype %in% c("sp_particle"))
    shinyjs::toggleElement(id = "cali_plot", condition = input$par_filetype %in% c("Cali", "sp_ionic"))
  })

  ### reactives ########################################################### ----
  # get input data as list of tables
  file_in <- reactive({
    req(input$ic_par_libsource)
    message("file_in() is updated")
    out <- NULL
    if (input$ic_par_libsource=="Upload files") {
      if (!is.null(input$ic_par_path_expfiles_inner)) {
        out <- try(import_data(file_path = input$ic_par_path_expfiles_inner$datapath, simplify = FALSE))
        if (inherits(x = out, what = "try-error")) {
          out <- NULL
        } else {
          names(out) <- input$ic_par_path_expfiles_inner$name
        }
      } else {
        out <- NULL
      }
    } else {
      if (input$ic_par_libsource=="Testdata") {
        out <- ETVapp::ETVapp_testdata[[input$par_wf]][[input$par_filetype]]
      }
    }
    if (!is.null(out)) {
      # try to guess concentrations from filenames for Cali and spionic
      if (input$par_filetype %in% c("Cali", "sp_ionic")) {
        pars$std_info <- extract_unique_number(names(out))
      } else {
        pars$std_info <- NULL
      }
      nval <- paste("Sample", 1:length(out))
      message("Update Sample Picker Input to ", paste(nval, collapse = ","))
      shinyWidgets::updatePickerInput(inputId = "ic_par_focus_sample", choices = nval, selected = nval)

    } else {
      ic_table_peaks_edit(NULL)
    }
    validate(need(out, message = "No valid data"))
    return(out)
  })

  shiny::observeEvent(input$ic_par_isotope, {
    if (input$ic_par_isotope) {
      shinyjs::hide("ic_par_specplot")
      shinyjs::show("ic_par_focus_iso")
    } else {
      shinyjs::show("ic_par_specplot")
      shinyjs::hide("ic_par_focus_iso")
    }
  })

  # register the file_in reactive for app testing
  shiny::exportTestValues(
    file_in = file_in
  )

  # check table headers for consistency and to get colnames to allow user column selection
  file_in_cols <- reactive({
    req(file_in())
    headers <- sapply(lapply(file_in(), colnames), paste, collapse="")
    validate(need(length(unique(headers))==1, message = "Files contain different headers"))
    return(colnames(file_in()[[1]]))
  })

  # convert input tables into spectra format for selected MI trace and RT column ----
  ic_mi_spectra_raw <- reactive({
    ensure_that(all(sapply(file_in(), function(x) { input$ic_par_mi_col %in% colnames(x) })), "Selected column not found in input data")
    req(file_in(), input$ic_par_mi_col, input$par_wf)
    #req(file_in(), input$ic_par_mi_col, cut_range$min, cut_range$max, rt_shift(), input$par_wf)
    message("ic_mi_spectra_raw")
    extract_shift_and_cut(
      data = file_in(),
      rt_col = "Time",
      c1 = input$ic_par_mi_col,
      c2 = ifelse(input$ic_par_si_col!=input$ic_par_mi_col, input$ic_par_si_col, "")
    )
  })

  # provide spectra based on processed raw data ----
  ic_mi_spectra <- reactive({
    req(ic_mi_spectra_raw(), pars$smoothing_fl, input$par_wf)
    message("ic_mi_spectra")
    # wrap processing in try to account for extreme parameter selections
    if (pars$smoothing_fl==1) fl <- NULL else fl <- pars$smoothing_fl
    if (input$par_filetype %in% c("sp_particle", "sp_ionic")) {
      # skip pre-processing and apply smoothing only to c1
      ## $$VS: No smoothing for sp data.
      out <- lapply(ic_mi_spectra_raw(), function(x) { smooth_col(df = x, nm = input$ic_par_mi_col, fl = fl, amend = "_smooth") })
    } else {
      out <- try(spec_pre_process(data = ic_mi_spectra_raw(), c1 = input$ic_par_mi_col, c2 = input$ic_par_si_col, fl = fl, wf = input$par_wf))
    }
    validate(need(!inherits(out, "try-error"), "Could not preprocess ic_mi_spectra_raw()"))
    # attach m_fs for oIDMS workflow
    if (input$par_wf == "oIDMS" & input$par_filetype %in% c("Samples", "Blanks")) {
      ensure_that(pars$K>0, "Please determine 'K' first using 'Massbias' files")
      ensure_that(input$trans_eff>0, "Please determine 'trans_eff' first using 'sp_particle' files")
      out <- lapply(out, function(x) {
        x[,"R_corr"] <- x[,"R_m"]*pars$K
        x[,"mf_s"] <- calc_massflow(x = x[,"R_corr"], n_trans = input$trans_eff, As_iso1 = input$ic_par_mi_amu, As_iso2 = input$ic_par_si_amu, Asp_iso1 = input$Asp_iso1, Asp_iso2 = input$Asp_iso2, V_fl = input$V_fl, c_sp = input$c_sp, DF = input$DF)
        return(x)
      })

    }
    return(out)
  })

  # identify peaks in processed mi spectra ----
  ic_mi_peaks <- reactive({
    req(ic_mi_spectra(), input$peak_start, input$peak_end, input$baseline_method)
    message("ic_mi_peaks")
    # wrap peak detection in try to account for extreme parameter selections
    pks <- try(lapply(ic_mi_spectra(), function(x) {
      get_peakdata(
        pro_data = x,
        int_col = paste0(input$ic_par_mi_col, "_smooth"),
        time_col = "Time",
        peak_start = input$peak_start,
        peak_end = input$peak_end,
        minpeakheight = input$peak_height,
        PPmethod = input$peak_method,
        BLmethod = input$baseline_method,
        deg = 1,
        cf = input$cf
      )
    }), silent = TRUE)
    validate(need(!(inherits(pks, "try-error")), "Can't obtain peaks from MI spectra"))
    return(pks)
  })

  # mi peak table ----
  ic_table_peaks_pre <- reactive({
    req(ic_mi_peaks())
    message("ic_table_peaks_pre")
    n <- length(ic_mi_peaks())
    pn <- unname(sapply(ic_mi_peaks(), nrow))
    out <- data.frame("Sample" = rep(1:n, times=pn), "Peak ID" = sapply(pn, function(x) {1:x}), ldply_base(ic_mi_peaks()), check.names = FALSE)
    return(out)
  })

  # table of peaks of 'new sample' ----
  shiny::observeEvent(ic_table_peaks_pre(), {
    tmp <- ic_table_peaks_pre()
    # if (nrow(tmp)>=1 && all(table(tmp[,"Peak ID"])==max(tmp[,"Sample"]))) {
    #   np <- max(tmp[,"Peak ID"])
    #   if (length(np)==1 && np>=2) {
    #     type <- c("standard", rep("sample", np-2), "standard")
    #     if (length(type)==2) type[2] <- "sample"
    #     tmp[,"Type"] <- sapply(tmp[,"Peak ID"], function(i) {type[i]})
    #   }
    # }
    ic_table_peaks_edit(tmp)
  })

  # change plot range upon user mouse interaction (click and drag) ----
  observeEvent(input$ic_specplot_brush, {
    spec_plots_xmin(input$ic_specplot_brush$xmin)
    spec_plots_xmax(input$ic_specplot_brush$xmax)
    spec_plots_ymin(input$ic_specplot_brush$ymin)
    spec_plots_ymax(input$ic_specplot_brush$ymax)
  })

  # change plot range upon user mouse interaction (double click) ----
  observeEvent(input$ic_specplot_dblclick, {
    req(ic_mi_spectra())
    c1 <- input$ic_par_mi_col
    c2 <- input$ic_par_si_col
    xrng <- range(sapply(ic_mi_spectra(), function(x) { range(x[,"Time"], na.rm=TRUE) }))
    yrng <- range(sapply(ic_mi_spectra(), function(x) { range(x[,c(c1, c2)], na.rm=TRUE) }))
    spec_plots_xmin(xrng[1])
    spec_plots_xmax(xrng[2])
    spec_plots_ymin(yrng[1])
    spec_plots_ymax(yrng[2])
  })

  # show fileUpload only when data source is set to 'upload files' ----
  observeEvent(input$ic_par_libsource, {
    shinyjs::toggle(id = "ic_par_path_expfiles", condition = input$ic_par_libsource=="Upload files")
  })

  # reset time windows (upon new data or new RT column)
  reset_times <- function() {
    req(file_in(), file_in_cols())
    if ("Time" %in% file_in_cols()) {
      # reset range cut
      rng <- sapply(file_in(), function(x) { range(x[,"Time"], na.rm=TRUE) })
    }
  }

  # update column selectors when input columns change
  observeEvent(file_in_cols(), {
    message("file_in_cols() did change to ", paste(file_in_cols(), collapse=", "))
    fic <- file_in_cols()
    n <- length(fic)
    mi_old <- shiny::isolate(input$ic_par_mi_col)
    si_old <- shiny::isolate(input$ic_par_si_col)
    iso_old <- shiny::isolate(input$ic_par_focus_iso)
    rt_selected <- ifelse("Time" %in% fic, "Time", fic[1])
    mi_sel <- ifelse(mi_old %in% fic, mi_old, fic[2])
    si_sel <- ifelse(si_old %in% fic, si_old, ifelse(length(fic)>=3, fic[3], fic[2]))
    iso_sel <- ifelse(iso_old %in% fic, iso_old, fic[2])
    updateSelectInput(inputId = "ic_par_mi_col", choices = I(fic[-1]), selected = mi_sel)
    updateSelectInput(inputId = "ic_par_si_col", choices = I(fic[-1]), selected = si_sel)
    shinyWidgets::updatePickerInput(inputId = "ic_par_focus_iso", choices = I(fic[-1]), selected = I(fic[-1]))
    reset_times()
  })

  mass_frac_debounce <- shiny::debounce(reactive(input$mass_fraction2), millis = 5000)
  observeEvent(mass_frac_debounce(), {
    # @Vera Ich speichere jetzt den Wert in einer Parameter-Liste zwischen. Er wird nur
    # geändert, wenn ein korrekter Wert im Input gewählt wurde, d.h. der letzte valide Wert
    # des Inputs wird genutzt. Die Warnung erscheint 1x
    val <- as.numeric(mass_frac_debounce())
    if (is.finite(val) && val<=1 & val>0) pars$mass_frac <- mass_frac_debounce() else showModal(modalDialog("Please correct the mass fraction value to be within 0<x<=1"))
  })

  # update MI/SI name inputs when input columns change
  observeEvent(input$ic_par_mi_col, {
    if (input$ic_par_mi_col %in% names(pars$Iso_labels)) {
      val <- unname(pars$Iso_labels[input$ic_par_mi_col])
    } else {
      val <- input$ic_par_mi_col
      pars$Iso_labels <- c(pars$Iso_labels, stats::setNames(input$ic_par_mi_col, input$ic_par_mi_col))
    }
    updateTextInput(inputId = "ic_par_mi_col_name", value = val)
    updateNumericInput(inputId = "ic_par_mi_amu", value = get_iso_info(x=input$ic_par_mi_col, isotopes=isotopes, info="abundance"))
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$ic_par_mi_col_name, {
    if (!identical(unname(pars$Iso_labels[input$ic_par_mi_col]), input$ic_par_mi_col_name)) pars$Iso_labels[input$ic_par_mi_col] <- input$ic_par_mi_col_name
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  observeEvent(input$ic_par_si_col_name, {
    if (!identical(unname(pars$Iso_labels[input$ic_par_si_col]), input$ic_par_si_col_name)) pars$Iso_labels[input$ic_par_si_col] <- input$ic_par_si_col_name
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # update MI/SI name inputs when input columns change
  observeEvent(input$ic_par_si_col, {
    if (input$ic_par_si_col %in% names(pars$Iso_labels)) {
      val <- unname(pars$Iso_labels[input$ic_par_si_col])
    } else {
      val <- input$ic_par_si_col
      pars$Iso_labels <- c(pars$Iso_labels, stats::setNames(input$ic_par_si_col, input$ic_par_si_col))
    }
    updateTextInput(inputId = "ic_par_si_col_name", value = val)
    updateNumericInput(inputId = "ic_par_si_amu", value = get_iso_info(x=input$ic_par_si_col, isotopes=isotopes, info="abundance"))
  }, ignoreNULL = TRUE, ignoreInit = TRUE)


  shiny::observe({
    R_m <- switch(
      input$par_wf,
      "IDMS" = if (is.null(pars$IDMS_cali)) NA else pars$IDMS_cali[,"R_m"],
      "oIDMS" = if (is.null(pars$oIDMS_cali)) NA else pars$oIDMS_cali[,"R_m"],
      NA
    )
    req(R_m, input$ic_par_mi_amu, input$ic_par_si_amu)
    K <- mean(calc_massbias(R_m, As_iso1 = input$ic_par_mi_amu, As_iso2 = input$ic_par_si_amu), na.rm=TRUE)
    pars$K <- ifelse(is.finite(K), K, 0)
  })

  shiny::observe({
    N_sp <- calc_N_sp(c_sp = input$c_sp, V_sp = input$V_sp, VF1 = input$VF1, M_sp = input$M_sp, M_sa = input$M_sa)
    shiny::updateNumericInput(inputId = "N_sp", value = N_sp)
  })

  # peak table output and associated action buttons ----
  output$ic_table_peaks <- DT::renderDT({
    req(ic_table_peaks_edit())
    pks <- ic_table_peaks_edit()
    # remove '_smooth' amendment in column 'Isotope' if user omitted smoothing step
    if (pars$smoothing_fl==1) {
      if ("Isotope" %in% colnames(pks)) {
        pks[,"Isotope"] <- gsub("_smooth", "", pks[,"Isotope"])
      }
    }
    message("output$ic_table_peaks")
    cm <- switch (
      input$par_wf,
      "ExtCal" = pars$ExtCal_cm,
      "ExtGasCal" = pars$ExtGasCal_cm,
      "oIDMS" = pars$oIDMS_cm,
      NULL
    )
    # compute and store cali table
    if (input$par_filetype %in% c("Cali")) {
      df <- tab_cali(
        peak_data = pks[,-c(1:2)],
        wf = input$par_wf,
        std_info = pars$std_info,
        fac = input$cali_fac
      )
      req(df)
      if (input$par_wf=="ExtCal") pars$ExtCal_cali <- df
      if (input$par_wf=="ExtGasCal") pars$ExtGasCal_cali <- df
    }
    if (input$par_filetype %in% c("Massbias")) {
      df <- ldply_base(1:length(ic_mi_spectra()), function(i) {
        get_isoratio(
          data = ic_mi_spectra()[[i]],
          iso1_col = input$ic_par_mi_col,
          iso2_col = input$ic_par_si_col,
          PPmethod = input$peak_method,
          peak_start = input$peak_start,
          peak_end = input$peak_end,
          BLmethod = input$baseline_method,
          fl = input$smoothing_fl
        )
      })
      req(df)
      if (input$par_wf=="IDMS") pars$IDMS_cali <- df
      if (input$par_wf=="oIDMS") pars$oIDMS_cali <- df
    }
    # compute and store samples table
    if (input$par_filetype %in% c("Sample","Samples")) {
      if (input$par_wf == "IDMS") {
        IDMS_pks <- ldply_base(1:length(ic_mi_spectra()), function(i) {
          get_isoratio(
            data = ic_mi_spectra()[[i]],
            iso1_col = input$ic_par_mi_col,
            iso2_col = input$ic_par_si_col,
            PPmethod = input$peak_method,
            peak_start = input$peak_start,
            peak_end = input$peak_end,
            BLmethod = input$baseline_method,
            fl = input$smoothing_fl
          )
        })
        amae <- calc_analyte_mass_as_element(
          R_m = IDMS_pks[,"R_m"], K = pars$K, Asp_iso1 = input$Asp_iso1, Asp_iso2 = input$Asp_iso2, As_iso1 = input$ic_par_mi_amu, As_iso2 = input$ic_par_si_amu, N_sp = input$N_sp
        )
      } else {
        amae <- 0
      }
      if (input$par_wf == "oIDMS") {
        x <- ic_mi_spectra()
        IDMS_pks <- ldply_base(1:length(x), function(i) {
          get_peakdata(x[[i]], int_col = "mf_s", PPmethod = input$peak_method, peak_start = input$peak_start, peak_end = input$peak_end, minpeakheight = input$peak_height)
        })
        # $$JL: sample_mass seems to be sample specific parameter; needs to get another input type and clearified if this is the case for all workflows
        #sample_mass <- c(1.0119, 0.9042, 0.9151)
        amae <- IDMS_pks[,4]
      }
      #if (input$par_filetype=="Samples") browser()
      df <- tab_result(
        peak_data = if (input$par_wf %in% c("ExtCal", "ExtGasCal")) pks[,-c(1:2)] else IDMS_pks,
        wf = input$par_wf,
        a = if (is.null(cm)) 0 else cm[1,3],
        b = if (is.null(cm)) 1 else cm[1,1],
        K = pars$K,
        amae = amae,
        mass_fraction2 = pars$mass_frac,
        sample_mass = input$sample_mass
      )
      req(df)
      if (input$par_wf=="ExtCal") pars$ExtCal_sam <- df
      if (input$par_wf=="ExtGasCal") pars$ExtGasCal_sam <- df
      if (input$par_wf=="IDMS") pars$IDMS_sam <- df
      if (input$par_wf=="oIDMS") pars$oIDMS_sam <- df
    }
    # compute LOx table
    if (input$par_filetype %in% c("Blanks")) {
      if (input$par_wf=="IDMS") {
        IDMS_pks <- ldply_base(1:length(ic_mi_spectra()), function(i) {
          get_isoratio(
            data = ic_mi_spectra()[[i]],
            iso1_col = input$ic_par_mi_col,
            iso2_col = input$ic_par_si_col,
            PPmethod = input$peak_method,
            peak_start = input$peak_start,
            peak_end = input$peak_end
          )
        })
        amae <- calc_analyte_mass_as_element(
          R_m = IDMS_pks[,"R_m"], K = pars$K, Asp_iso1 = input$Asp_iso1, Asp_iso2 = input$Asp_iso2, As_iso1 = input$ic_par_mi_amu, As_iso2 = input$ic_par_si_amu, N_sp = input$N_sp
        )
        tmp <- tab_result(IDMS_pks, wf = input$par_wf, K = pars$K, amae = amae, mass_fraction2 = pars$mass_frac, sample_mass = input$sample_mass)[,"R_corr"]
      } else if (input$par_wf=="oIDMS") {
        x <- ic_mi_spectra()
        pk <- ldply_base(1:length(x), function(i) {
          get_peakdata(x[[i]], int_col = "mf_s", PPmethod = input$peak_method, peak_start = input$peak_start, peak_end = input$peak_end, minpeakheight = input$peak_height)
        })
        tmp <- tab_result(pk, wf = "oIDMS", K = pars$K, amae = pk[,4], mass_fraction2 = pars$mass_frac, sample_mass = input$sample_mass)[,4]
      } else {
        tmp <- pks[,-c(1:2)][,4]
      }
      df <- tab_LOX(
        x = tmp,
        cali_slope = if (is.null(cm)) 1 else cm[1,1],
        wf = input$par_wf,
        ExtCal_unit = input$ExtCal_unit,
        ExtGasCal_unit = input$ExtGasCal_unit,
        c_sp_unit = input$c_sp_unit,
        mass_fraction2 = pars$mass_frac,
        sample_mass = input$sample_mass
      )
      req(df)
      if (input$par_wf=="ExtCal") pars$ExtCal_lox <- df
      if (input$par_wf=="ExtGasCal") pars$ExtGasCal_lox <- df
      if (input$par_wf=="IDMS") pars$IDMS_lox <- df
      if (input$par_wf=="oIDMS") pars$oIDMS_lox <- df
    }
    out <- pks
    # remove column 'Peak ID' if all IDs are unique
    if ("Peak ID" %in% colnames(out) && length(unique(out[,"Peak ID"]))==1) out <- out[,!colnames(out) %in% "Peak ID",drop=FALSE]
    # remove smooth annotation from ion name
    if ("Isotope" %in% colnames(out) && length(grep("_smooth", out[,"Isotope"]))>=1) out[,"Isotope"] <- gsub("_smooth", "", out[,"Isotope"])
    style_tab_peaks(data = limit_digits(out))
  })

   # enables manual editing of the peak borders in the peak table
  shiny::observeEvent(input$ic_table_peaks_cell_edit, {
    # convert column values to numeric
    x <- as.numeric(gsub("[^[[:digit:]-].]", "", input$ic_table_peaks_cell_edit$value))
    # replace in correct column and update 'ic_table_peaks_edit'
    tmp <- ic_table_peaks_edit()
    tmp[, input$ic_table_peaks_cell_edit$col[1] + 1] <- x
    ic_table_peaks_edit(tmp)
  })

  # cali peaks table ----
  output$table_cali <- DT::renderDT({
    message("output$table_cali")
    df <- switch (
      input$par_wf,
      "ExtCal" = pars$ExtCal_cali,
      "ExtGasCal" = pars$ExtGasCal_cali,
      "oIDMS" = pars$oIDMS_cali,
      "IDMS" = pars$IDMS_cali,
      NULL
    )
    ensure_that(!is.null(df), msg = paste("Please process Cali/Massbias Files"))
    DT::datatable(data = limit_digits(df), options = list(dom = "t"), rownames = FALSE)
  })

  # cali model table ----
  output$table_cm <- DT::renderDT({
    message("output$table_cm")
    df <- switch (
      input$par_wf,
      "ExtCal" = pars$ExtCal_cm,
      "ExtGasCal" = pars$ExtGasCal_cm,
      "oIDMS" = pars$oIDMS_cm,
      NULL
    )
    ensure_that(input$par_wf!="IDMS", msg = paste("No calibration model for IDMS workflow"))
    ensure_that(!is.null(df), msg = paste("Please process calibration model"))
    DT::datatable(data = limit_digits(df), options = list(dom = "t"), rownames = FALSE)
  })

  # sample table ----
  output$table_sam <- DT::renderDT({
    message("output$table_sam")
    df <- switch (
      input$par_wf,
      "ExtCal" = pars$ExtCal_sam,
      "ExtGasCal" = pars$ExtGasCal_sam,
      "oIDMS" = pars$oIDMS_sam,
      "IDMS" = pars$IDMS_sam,
      NULL
    )
    ensure_that(!is.null(df), msg = paste("Please process Samples"))
    DT::datatable(data = limit_digits(df), options = list(dom = "t"), rownames = FALSE)
  })

  # LOx table ----
  output$table_lox <- DT::renderDT({
    message("output$table_lox")
    df <- switch (
      input$par_wf,
      "ExtCal" = pars$ExtCal_lox,
      "ExtGasCal" = pars$ExtGasCal_lox,
      "oIDMS" = pars$oIDMS_lox,
      "IDMS" = pars$IDMS_lox,
      NULL
    )
    ensure_that(!is.null(df), msg = paste("Please process Blanks"))
    DT::datatable(data = limit_digits(df), options = list(dom = "t"), rownames = FALSE)
  })

  # spectrum plot ----
  output$ic_specplot <- shiny::renderPlot({
    req(ic_mi_spectra(), input$ic_par_mi_col_name)
    validate(need(input$ic_par_focus_sample, "Please select a sample..."))
    validate(need(length(ic_mi_spectra())>=max(as.numeric(gsub("[^[:digit:]]", "", input$ic_par_focus_sample))), "Sample selection and current spectra number do not match"))
    message("output$ic_specplot")
    if (input$ic_par_isotope) {
      # this is a small iosotope overview plot function
      idx_all <- as.numeric(gsub("[^[:digit:]]", "", input$ic_par_focus_sample))
      # !!! $$JL:$$ strong assumption that Time column is always column 1 (not guaranteed for user imported data)
      x_rng <- range(sapply(file_in()[idx_all], function(x) { range(x[,1], na.rm=TRUE) }), na.rm=TRUE)
      y_rng <- c(0, max(sapply(file_in()[idx_all], function(x) { range(x[,input$ic_par_focus_iso], na.rm=TRUE) }), na.rm=TRUE))
      par("mar"=c(4,4,0,0)+0.1, "cex"=1.4)
      plot(x = x_rng, y = y_rng, type="n", "xaxs"="i", xlab="Time [s]", ylab="Intensity [cts]")
      for (i in idx_all) {
        for (j in input$ic_par_focus_iso) {
          lines(x = file_in()[[i]][,1], y = file_in()[[i]][,j], lty = i, col = which(input$ic_par_focus_iso %in% j))
        }
      }
    } else {
      c1 <- input$ic_par_mi_col
      bl <- FALSE
      ylab <- "Intensity [cps]" #input$ic_par_mi_col_name
      if (pars$smoothing_fl >= 3) {
        c1 <- paste0(c1, "_smooth")
      }
      if (input$par_wf == "oIDMS" & input$par_filetype %in% c("Samples", "Blanks")) {
        c1 <- "mf_s"
        ylab <- "mf_s"
      }
      opt <- input$ic_par_specplot
      ic_specplot(
        opt = opt,
        xrng = c(spec_plots_xmin(), spec_plots_xmax()),
        yrng = c(spec_plots_ymin(), spec_plots_ymax()),
        mi_spec = ic_mi_spectra(),
        c1 = c1,
        xlab = paste0("Time [s]"),
        ylab = ylab,
        T_prog = pars$T_prog,
        s_focus = input$ic_par_focus_sample,
        pks = ic_table_peaks_edit(),
        BLmethod = input$baseline_method,
        sel_pk = input$ic_table_peaks_rows_selected
      )
    }
  })

  # cali plot ----
  output$cali_plot <- shiny::renderPlot({
    shiny::req(pars$std_info, ic_table_peaks_edit())
    shiny::req(length(pars$std_info)==nrow(ic_table_peaks_edit()))
    message("output$caliplot")
    cali_peaks <- cbind("std_info" = pars$std_info, ic_table_peaks_edit()[,6,drop=FALSE])
    cm <- calc_cali_mod(df = cali_peaks, wf = input$par_wf)
    if (input$par_wf == "ExtCal") {
      pars$ExtCal_cm <- cm
      colnames(cali_peaks)[1] <- paste0("Analyte mass [", input$ExtCal_unit, "]")
    }
    if (input$par_wf == "ExtGasCal") {
      pars$ExtGasCal_cm <- cm
      colnames(cali_peaks)[1] <- paste0("Analyte mass [", input$ExtGasCal_unit, "]")
    }
    if (input$par_wf == "oIDMS") {
      pars$oIDMS_cm <- cm
    }
    par(cex = 1.4)
    plot(cali_peaks)
    abline(a = cm[1,3], b = cm[1,1])
  })

  # signal distribution plot ----

  oIMDS_particle_flt <- shiny::reactive({
    shiny::req(input$par_wf == "oIDMS", input$par_filetype == "sp_particle", input$t_fltr, ic_mi_spectra())
    shiny::req(input$ic_par_mi_col %in% colnames(ic_mi_spectra()[[1]]))
    message("oIMDS_particle_flt")
    rt <- ic_mi_spectra()[[1]][,"Time"]
    flt <- 1:length(rt)
    if (input$t_fltr > min(rt, na.rm=TRUE) & input$t_fltr < max(rt, na.rm=TRUE)) {
      flt <- which(rt<input$t_fltr)
    }
    ic_mi_spectra()[[1]][flt,c("Time", input$ic_par_mi_col)]
  })

  trans_eff <- shiny::reactive({
    shiny::req(oIMDS_particle_flt(), input$LFD)
    message("trans_eff")
    ensure_that(!is.null(pars$oIDMS_cm), "Please calculate a slope first using 'sp_ionic' files.")
    calc_transeff(data = oIMDS_particle_flt(), int_col = input$ic_par_mi_col, LFD = input$LFD, cali_slope = pars$oIDMS_cm[1,1], V_fl = input$V_fl, part_mat = input$part_mat, dia_part = input$dia_part)
  })
  shiny::observeEvent(trans_eff(), {
    shiny::updateNumericInput(inputId = "trans_eff", value = trans_eff()[,4])
  })

  output$sp_particle_plot <- shiny::renderPlot({
    shiny::req(oIMDS_particle_flt(), input$LFD)
    message("output$sp_particle_plot")
    plot_signal_distribution(x = oIMDS_particle_flt()[,input$ic_par_mi_col], LFD = input$LFD)
  })

  output$sp_particle_size_distribution_plot <- shiny::renderPlot({
    shiny::req(oIMDS_particle_flt(), input$LFD, pars$oIDMS_cm)
    message("output$sp_particle_size_distribution_plot")
    plot_particle_diameter(x = oIMDS_particle_flt(), cali_slope = pars$oIDMS_cm[1,1], V_fl = input$V_fl, part_mat = input$part_mat, dia_part = input$dia_part, LFD = input$LFD)
  })

  # help modals ----
  shiny::observeEvent(input$ic_help01, { help_the_user(filename = "01_general") })
  shiny::observeEvent(input$ic_help02, { help_the_user(filename = "02_file_upload") })
  shiny::observeEvent(input$ic_help03, { help_the_user(filename = "03_import_params") })
  shiny::observeEvent(input$ic_help04, { help_the_user(filename = "04_processing_params") })
  shiny::observeEvent(input$ic_help05, { help_the_user(filename = "05_plot_options") })
  shiny::observeEvent(input$ic_help06, { help_the_user(filename = "06_peak_table") })
  shiny::observeEvent(input$ic_help07, { help_the_user(filename = "07_ratio_table") })
  shiny::observeEvent(input$ic_help08, { help_the_user(filename = "08_delta_table") })
  shiny::observeEvent(input$ic_help09, { help_the_user(filename = "09_IDMS_table") })
  shiny::observeEvent(input$ic_help10, { help_the_user(filename = "10_processing_params_IDMS") })

}
# ================================
