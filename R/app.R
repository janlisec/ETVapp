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
    options = list("port" = 7462, "display.mode" = "normal"),
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
        radioButtons(inputId = "par_wf", label = "Workflow", choices = c("IDMS","oIDMS","ExtCal","ExtGasCal"), selected = c("IDMS","oIDMS","ExtCal","ExtGasCal")[2]),
        radioButtons(inputId = "par_filetype", label = "File Type", choices = c("Cali", "Massbias", "Samples", "Blanks", "sp_ionic", "sp_particle"), selected = "Massbias")
      ),
      uiOutput(outputId = "ic_par_path_expfiles")
    )
  )
  # import parameters
  card_import <- shiny::tagList(
    bslib::card(
      bslib::card_header(shiny::actionLink(inputId = "ic_help03", label = "Import")),
      bslib::layout_column_wrap(
        width = 120, gap = "5px",
        shinyjs::hidden(selectInput(inputId = "ic_par_rt_col", label = "RT column", choices = "Time")) |> bslib::tooltip("Select RT column."),
        selectInput(inputId = "ic_par_mi_col", label = "Iso1 column", choices = c("")) |> bslib::tooltip("Select Spike Isotope column."),
        selectInput(inputId = "ic_par_si_col", label = "Iso2 column", choices = c("")) |> bslib::tooltip("Select Sample Isotope column."),
        shinyjs::hidden(textInput(inputId = "ic_par_mi_rt_unit", label = "RT unit", value = "s")),
        textInput(inputId = "ic_par_mi_col_name", label = "Iso1 Name"),
        textInput(inputId = "ic_par_si_col_name", label = "Iso2 Name"),
        shiny::HTML(""),
        numericInput(inputId = "ic_par_mi_amu", label = "Iso1 abundance", value = 0, step = 0.0001),
        numericInput(inputId = "ic_par_si_amu", label = "Iso2 abundance", value = 0, step = 0.0001)
      )
    )
  )
  card_processing <- shiny::tagList(
    bslib::card(
      id = "Processing_par_section",
      bslib::card_header(shiny::actionLink(inputId = "ic_help04", label = "Processing")),
      bslib::layout_column_wrap(
        width = 120, gap = "5px",
        numericInput(inputId = "smoothing_fl", label = "Smoothing", value = 9, min=1, max=151, step=2) |> bslib::tooltip("Smoothing parameter 'filter length'. Set to '1' to omit this processing step. Set to '151' to use smooth.spline()."),
        selectInput(inputId = "baseline_method", label = "BL Method", choices = c("none", "modpolyfit"), selected = "modpolyfit") |> bslib::tooltip("Select method for baseline estimation or 'none' to omit this processing step."),
        selectInput(inputId = "peak_method", label = "Peak Method", choices = c("Peak (height)", "Peak (manual)", "mean_signal"), selected = c("Peak (height)", "Peak (manual)", "mean_signal")[3]) |> bslib::tooltip("Select method for Peak picking."),
        numericInput(inputId = "peak_start", label = "Peak start [s]", value = 70, min=0, step=1) |> bslib::tooltip("Peak picking parameter: peak_start."),
        numericInput(inputId = "peak_end", label = "Peak end [s]", value = 105, min=1, max=1000, step=1) |> bslib::tooltip("Peak picking parameter: peak_end."),
        shinyjs::disabled(numericInput(inputId = "peak_height", label = "Threshold height [cps]", value = 1000, min=0, step=1)) |> bslib::tooltip("Peak picking parameter: peak_height.")
      )
    )
  )
  IDMS_pars_common <- shiny::div(
    id = "IDMS_pars_common",
    bslib::layout_column_wrap(
      width = 120, gap = "5px",
      numericInput(inputId = "As_iso1", label = "As_iso1", value = 12.22),
      numericInput(inputId = "As_iso2", label = "As_iso2", value = 12.8),
      shinyjs::disabled(numericInput(inputId = "K", label = "K", value = 1)),
      numericInput(inputId = "Asp_iso1", label = "Asp_iso1", value = 92.61),
      numericInput(inputId = "Asp_iso2", label = "Asp_iso2", value = 0.22),
      numericInput(inputId = "c_sp", label = "Spike concentration", value = 2.5),
      selectInput(inputId = "c_sp_unit", label = "Spike unit", choices = c("\u00b5g/L", "mg/L"))
    )
  )
  IDMS_pars <- shiny::div(
    id = "IDMS_pars",
    bslib::layout_column_wrap(
      width = 120, gap = "5px",
      numericInput(inputId = "V_sp", label = "V_sp", value = 6),
      numericInput(inputId = "VF1", label = "Dilution factor", value = 1000),
      numericInput(inputId = "M_sp", label = "Molar Mass Spike", value = 113.01),
      numericInput(inputId = "M_sa", label = "Molar Mass Sample", value = 112.41),
      shinyjs::disabled(numericInput(inputId = "N_sp", label = "Spike amount", value = 1))
    )
  )
  oIDMS_pars <- shiny::div(
    id = "oIDMS_pars",
    bslib::layout_column_wrap(
      width = 120, gap = "5px",
      numericInput(inputId = "t_fltr", label = "t_fltr", value = 60),
      numericInput(inputId = "LFD", label = "LFD", value = 20000),
      numericInput(inputId = "V_fl", label = "V_fl", value = 0.0075),
      numericInput(inputId = "dia_part", label = "dia_part", value = 60),
      selectInput(inputId = "part_mat", label = "part_mat", choices = c("Au", "Ag"), selected = "Au"),
      shinyjs::disabled(numericInput(inputId = "trans_eff", label = "trans_eff [%]", value = 0)),
      numericInput(inputId = "Df", label = "Df", value = 20)
    )
  )
  ExtIDMS_pars_common <- shiny::div(
    id = "ExtIDMS_pars_common",
    bslib::layout_column_wrap(
      width = 120, gap = "5px",
      numericInput(inputId = "mass_fraction2", label = "Mass fraction", value = 1, min = 10^-6, max = 1, step = 10^-6),
      numericInput(inputId = "sample_mass", label = "Sample mass [mg]", value = 1, min = 1),
      shiny::HTML("")
    )
  )
  ExtCal_pars <- shiny::div(
    id = "ExtCal_pars",
    bslib::layout_column_wrap(
      width = 120, gap = "5px",
      selectInput(inputId = "ExtCal_unit", label = "Analyte mass unit", choices = c("pg","ng","\u00b5g")),
      shiny::HTML(""),
      shiny::HTML("")
    )
  )
  ExtGasCal_pars <- shiny::div(
    id = "ExtGasCal_pars",
    bslib::layout_column_wrap(
      width = 120, gap = "5px",
      numericInput(inputId = "cali_fac", label = "Conversion factor", value = 0.005002692),
      selectInput(inputId = "ExtGasCal_unit", label = "Gas flow unit", choices = c("\u00b5L/min", "mL/min")),
      shiny::HTML("")
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
            selectInput(inputId = "ic_par_focus_sample", label = "Focus sample", choices = list("Sample 1"=1)),
            checkboxGroupInput(
              inputId = "ic_par_specplot",
              label = shiny::actionLink(inputId = "ic_help05", label = "Plot options"),
              choices = list(
                "show all samples" = "overlay_mi",
                "show peak boundaries"="overlay_pb",
                "show sample IDs" = "overlay_legend",
                "show Temp program" = "overlay_Temp"
              ),
              selected = c("overlay_pb", "overlay_mi", "overlay_legend", "overlay_Temp")
            ),
            actionButton(inputId = "ic_par_cut_range", label = "cut range") |> bslib::tooltip("Cut samples to currently visible range."),
            read_clipboard_UI(id = "T_prog") |> bslib::tooltip("Specify Temp program.")
            #actionButton(inputId = "T_prog", label = "Set Temp Prog") |> bslib::tooltip("Specify Temp program.")
          ),
          plotOutput(
            outputId = "ic_specplot",
            dblclick = dblclickOpts(id = "ic_specplot_dblclick"),
            brush = brushOpts(id = "ic_specplot_brush", direction = "x", resetOnNew = TRUE)
          ) |> bslib::tooltip("You may select a time range [Click and Drag] with the cursor to zoom. Use [Double Click] to unzoom.", placement = "bottom")
        )
      )
    )
  }

  ic_tables_card <- function() {
    bslib::card(
      id = "ic_tables_card",
      bslib::card_body(
        fill = TRUE,
        bslib::layout_column_wrap(
          width = "620px",
          height = "auto",
          fill = FALSE,
          gap = "1rem",
          shiny::div(
            style = "width: 600px;",
            DT::DTOutput("ic_table_peaks")
          ),
          shiny::div(
            style = "width: 600px;",
            shiny::div(style = "min-height: 2rem;", DT::DTOutput("table_cali")),
            shiny::div(style = "min-height: 2rem;", DT::DTOutput("table_cm")),
            shiny::div(style = "min-height: 2rem;", DT::DTOutput("table_sam")),
            shiny::div(style = "min-height: 2rem;", DT::DTOutput("table_lox"))
          ),
          shiny::div(
            style = "width: 600px;",
            plotOutput(outputId = "cali_plot", height= "600px"),
            plotOutput(outputId = "sp_particle_plot", height= "350px"),
            plotOutput(outputId = "sp_particle_size_distribution_plot", height= "350px")
          )
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
      ic_plot_card(),
      ic_tables_card(),
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
  spec_plots_xmax <- reactiveVal(10000)
  # the time range if cutting is applied
  cut_range <- reactiveValues("min"=NULL, "max"=NULL)
  # the rt shift applied to samples for alignment
  rt_shift <- reactiveVal(0)
  # indicator if range cut is currently applied
  status_range_cut <- reactiveVal("off")

  pars <- shiny::reactiveValues(
    "smoothing_fl" = 7,
    "std_info" = NULL,
    "amae" = NULL,
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
    shinyjs::toggleElement(id = "ExtCal_pars", condition = input$par_wf=="ExtCal")
    shinyjs::toggleElement(id = "ExtGasCal_pars", condition = input$par_wf=="ExtGasCal")
    shinyjs::toggleElement(id = "ExtIDMS_pars_common", condition = input$par_wf %in% c("ExtCal","ExtGasCal","IDMS","oIDMS"))
    shinyjs::toggleElement(id = "IDMS_pars", condition = input$par_wf=="IDMS")
    shinyjs::toggleElement(id = "oIDMS_pars", condition = input$par_wf=="oIDMS")
    shinyjs::toggleElement(id = "IDMS_pars_common", condition = input$par_wf %in% c("IDMS","oIDMS"))
    tmp <- list("ExtCal"=c("Cali","Sample","Blanks"),"ExtGasCal"=c("Cali","Samples","Blanks"),"IDMS"=c("Massbias","Samples","Blanks"),"oIDMS"=c("Massbias","sp_ionic","sp_particle","Samples","Blanks"))
    shiny::updateRadioButtons(inputId = "par_filetype", choices = tmp[[input$par_wf]])
    if (input$par_wf %in% c("ExtCal", "ExtGasCal")) {
      updateSelectInput(inputId = "ic_par_mi_col", label = "Analyte column")
      updateSelectInput(inputId = "ic_par_si_col", label = "Standard column")
      updateSelectInput(inputId = "ic_par_mi_col_name", label = "Analyte Label")
      updateSelectInput(inputId = "ic_par_si_col_name", label = "Standard Label")
      shinyjs::hide(id = "ic_par_mi_amu")
      shinyjs::hide(id = "ic_par_si_amu")
    } else {
      updateSelectInput(inputId = "ic_par_mi_col", label = "Iso1 column")
      updateSelectInput(inputId = "ic_par_si_col", label = "Iso2 column")
      updateSelectInput(inputId = "ic_par_mi_col_name", label = "Iso1 Label")
      updateSelectInput(inputId = "ic_par_si_col_name", label = "Iso2 Label")
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
    message("file_in")
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
      rt_shift(rep(0, length(out)))
      updateSelectInput(inputId = "ic_par_focus_sample", choices = paste("Sample", 1:length(out)))
      if (length(out)>1) {
        shinyjs::enable(selector = "#ic_par_specplot input[value='overlay_mi']")
      } else {
        updateCheckboxGroupInput(inputId = "ic_par_specplot", selected = c("overlay_pb"))
        shinyjs::disable(selector = "#ic_par_specplot input[value='overlay_mi']")
        shinyjs::hide(id = "ic_par_focus_sample")
      }
    } else {
      ic_table_peaks_edit(NULL)
    }
    validate(need(out, message = "No valid data"))
    return(out)
  })

  # register the file_in reactive for app testing
  shiny::exportTestValues(
    file_in = file_in
  )

  observeEvent(input$ic_par_specplot, {
    shinyjs::toggle(id = "ic_par_focus_sample", condition = !("overlay_mi" %in% input$ic_par_specplot))
  }, ignoreNULL = FALSE)

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
    req(file_in(), input$ic_par_rt_col, input$ic_par_mi_col, cut_range$min, cut_range$max, rt_shift(), input$par_wf)
    message("ic_mi_spectra_raw")
    extract_shift_and_cut(
      data = file_in(),
      rt_col = input$ic_par_rt_col,
      c1 = input$ic_par_mi_col,
      c2 = ifelse(input$ic_par_si_col!=input$ic_par_mi_col, input$ic_par_si_col, ""),
      cut_range = shiny::reactiveValuesToList(cut_range),
      rt_shift = rt_shift()
    )
  })

  # provide spectra based on processed raw data ----
  ic_mi_spectra <- reactive({
    req(ic_mi_spectra_raw(), pars$smoothing_fl, input$par_wf)
    message("ic_mi_spectra")
    # wrap processing in try to account for extreme parameter selections
    if (input$par_filetype %in% c("sp_particle", "sp_ionic")) {
      # skip pre-processing and apply smoothing only to c1
      out <- lapply(ic_mi_spectra_raw(), function(x) { smooth_col(df = x, nm = input$ic_par_mi_col, fl = pars$smoothing_fl, amend = "_smooth") })
    } else {
      out <- try(spec_pre_process(data = ic_mi_spectra_raw(), c1 = input$ic_par_mi_col, c2 = input$ic_par_si_col, fl = pars$smoothing_fl, wf = input$par_wf))
    }
    validate(need(!inherits(out, "try-error"), "Could not preprocess ic_mi_spectra_raw()"))
    # attach m_fs for oIDMS workflow
    if (input$par_wf == "oIDMS" & input$par_filetype %in% c("Samples", "Blanks")) {
      ensure_that(input$K>0, "Please determine 'K' first using 'Massbias' files")
      ensure_that(input$trans_eff>0, "Please determine 'trans_eff' first using 'sp_particle' files")
      out <- lapply(out, function(x) {
        x[,"R_corr"] <- correct_ratio(x = x[,"R_m"], K = input$K, As_iso1 = input$As_iso1, As_iso2 = input$As_iso2)
        x[,"mf_s"] <- calc_massflow(x = x[,"R_corr"], n_trans = input$trans_eff, As_iso1 = input$As_iso1, As_iso2 = input$As_iso2, Asp_iso1 = input$Asp_iso1, Asp_iso2 = input$Asp_iso2, V_fl = input$V_fl, c_sp = input$c_sp, DF = input$DF)
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
        cf = 50
      )
    }), silent = TRUE)
    validate(need(!(inherits(pks, "try-error")), "Can't obtain peaks from MI spectra"))
    return(pks)
  })

  # mi peak table ----
  ic_table_peaks_pre <- reactive({
    req(ic_mi_peaks())
    message("ic_table_peaks_pre")
    #browser()
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
  })

  # change plot range upon user mouse interaction (double click) ----
  observeEvent(input$ic_specplot_dblclick, {
    req(ic_mi_spectra())
    rng <- range(sapply(ic_mi_spectra(), function(x) { range(x[,"Time"], na.rm=TRUE) }))
    spec_plots_xmin(rng[1])
    spec_plots_xmax(rng[2])
  })

  # show fileUpload only when data source is set to 'upload files' ----
  observeEvent(input$ic_par_libsource, {
    shinyjs::toggle(id = "ic_par_path_expfiles", condition = input$ic_par_libsource=="Upload files")
  })

  # reset time windows (upon new data or new RT column)
  reset_times <- function() {
    req(file_in(), file_in_cols(), input$ic_par_rt_col)
    if (input$ic_par_rt_col %in% file_in_cols()) {
      # reset range cut
      rng <- sapply(file_in(), function(x) { range(x[,input$ic_par_rt_col], na.rm=TRUE) })
      cut_range$min <- min(rng[1,])
      cut_range$max <- max(rng[2,])
      status_range_cut("off")
      updateActionButton(inputId = "ic_par_cut_range", label = "cut range")
      # reset alignment
      rt_shift(rep(0, length(file_in())))
      # ...reset display range
      spec_plots_xmin(cut_range$min)
      spec_plots_xmax(cut_range$max)
    }
  }

  # update column selectors when input columns change
  observeEvent(file_in_cols(), {
    fic <- file_in_cols()
    n <- length(fic)
    mi_old <- shiny::isolate(input$ic_par_mi_col)
    si_old <- shiny::isolate(input$ic_par_si_col)
    #rt_selected <- ifelse("Time" %in% fic, "Time", fic[1])
    mi_sel <- ifelse(mi_old %in% fic, mi_old, fic[2])
    si_sel <- ifelse(si_old %in% fic, si_old, ifelse(length(fic)>=3, fic[3], fic[2]))
    #updateSelectInput(inputId = "ic_par_rt_col", choices = I(fic), selected = rt_selected)
    updateSelectInput(inputId = "ic_par_mi_col", choices = I(fic[-1]), selected = mi_sel)
    updateSelectInput(inputId = "ic_par_si_col", choices = I(fic[-1]), selected = si_sel)
    reset_times()
  })

  # check and update time range filters when time column is changed
  observeEvent(input$ic_par_rt_col, {
    req(file_in())
    reset_times()
  }, ignoreInit = TRUE)

  observeEvent(input$mass_fraction2, {
    if (input$mass_fraction2>1 | input$mass_fraction2<=0) {
      updateNumericInput(inputId = "mass_fraction2", value = 1)
    }
  }, ignoreInit = TRUE)

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

  # set cut range to displayed spectrum range when user triggers this action button
  observeEvent(input$ic_par_cut_range, {
    req(cut_range$min, input$ic_par_rt_col, spec_plots_xmin(), spec_plots_xmax())
    if (status_range_cut()=="off") {
      cut_range$min <- spec_plots_xmin()
      cut_range$max <- spec_plots_xmax()
      updateActionButton(inputId = "ic_par_cut_range", label = "undo cut")
      status_range_cut("on")
    } else {
      rng <- sapply(file_in(), function(x) { range(x[,input$ic_par_rt_col], na.rm=TRUE) })
      cut_range$min <- min(rng[1,])
      cut_range$max <- max(rng[2,])
      updateActionButton(inputId = "ic_par_cut_range", label = "cut range")
      status_range_cut("off")
    }
  })
  observeEvent(status_range_cut(), {
    btn_col <- if (status_range_cut()=="on") {
      shinyjs::runjs('document.getElementById("ic_par_cut_range").style.backgroundColor = "#FFA500";')
    } else {
      shinyjs::runjs('document.getElementById("ic_par_cut_range").style.backgroundColor = "#FFFFFF";')
    }
  })

  shiny::observe({
    R_m <- switch(
      input$par_wf,
      "IDMS" = if (is.null(pars$IDMS_cali)) NA else pars$IDMS_cali[,"R_m"],
      "oIDMS" = if (is.null(pars$oIDMS_cali)) NA else pars$oIDMS_cali[,"R_m"],
      NA
    )
    req(R_m, input$As_iso1, input$As_iso2)
    K <- calc_massbias(R_m, As_iso1 = input$As_iso1, As_iso2 = input$As_iso2)
    shiny::updateNumericInput(inputId = "K", value = mean(K, na.rm=TRUE))
  })

  shiny::observe({
    N_sp <- calc_N_sp(c_sp = input$c_sp, V_sp = input$V_sp, VF1 = input$VF1, M_sp = input$M_sp, M_sa = input$M_sa)
    shiny::updateNumericInput(inputId = "N_sp", value = N_sp)
  })

  # peak table output and associated action buttons ----
  output$ic_table_peaks <- DT::renderDT({
    req(ic_table_peaks_edit())
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
        peak_data = ic_table_peaks_edit()[,-c(1:2)],
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
          R_m = IDMS_pks[,"R_m"], K = input$K, Asp_iso1 = input$Asp_iso1, Asp_iso2 = input$Asp_iso2, As_iso1 = input$As_iso1, As_iso2 = input$As_iso2, N_sp = input$N_sp
        )
      } else {
        amae <- 0
      }
      if (input$par_wf == "oIDMS") {
        x <- ic_mi_spectra()
        IDMS_pks <- ldply_base(1:length(x), function(i) {
          get_peakdata(x[[i]], int_col = "mf_s", PPmethod = input$peak_method, peak_start = input$peak_start, peak_end = input$peak_end)
        })
        # $$JL: sample_mass seems to be sample specific parameter; needs to get another input type and clearified if this is the case for all workflows
        #sample_mass <- c(1.0119, 0.9042, 0.9151)
        amae <- IDMS_pks[,4]
      }
      df <- tab_result(
        peak_data = if (input$par_wf %in% c("ExtCal", "ExtGasCal")) ic_table_peaks_edit()[,-c(1:2)] else IDMS_pks,
        wf = input$par_wf,
        a = if (is.null(cm)) 0 else cm[1,3],
        b = if (is.null(cm)) 1 else cm[1,1],
        K = input$K,
        amae = amae,
        mass_fraction2 = input$mass_fraction2,
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
          R_m = IDMS_pks[,"R_m"], K = input$K, Asp_iso1 = input$Asp_iso1, Asp_iso2 = input$Asp_iso2, As_iso1 = input$As_iso1, As_iso2 = input$As_iso2, N_sp = input$N_sp
        )
        tmp <- tab_result(IDMS_pks, wf = input$par_wf, K = input$K, amae = amae, mass_fraction2 = input$mass_fraction2, sample_mass = input$sample_mass)[,"R_corr"]
      } else if (input$par_wf=="oIDMS") {
        x <- ic_mi_spectra()
        pk <- ldply_base(1:length(x), function(i) {
          get_peakdata(x[[i]], int_col = "mf_s", PPmethod = input$peak_method, peak_start = input$peak_start, peak_end = input$peak_end)
        })
        tmp <- tab_result(pk, wf = "oIDMS", K = input$K, amae = pk[,4], mass_fraction2 = input$mass_fraction2, sample_mass = input$sample_mass)[,4]
      } else {
        tmp <- ic_table_peaks_edit()[,-c(1:2)][,4]
      }
      df <- tab_LOX(
        x = tmp,
        cali_slope = if (is.null(cm)) 1 else cm[1,1],
        wf = input$par_wf,
        mass_fraction2 = input$mass_fraction2,
        sample_mass = input$sample_mass,
        unit = switch(input$par_wf, "ExtCal" = input$ExtCal_unit, "ExtGasCal" = input$ExtGasCal_unit, "ng")
      )
      req(df)
      if (input$par_wf=="ExtCal") pars$ExtCal_lox <- df
      if (input$par_wf=="ExtGasCal") pars$ExtGasCal_lox <- df
      if (input$par_wf=="IDMS") pars$IDMS_lox <- df
      if (input$par_wf=="oIDMS") pars$oIDMS_lox <- df
    }
    out <- ic_table_peaks_edit()
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
    message("output$ic_specplot")
    c1 <- input$ic_par_mi_col
    bl <- FALSE
    ylab <- input$ic_par_mi_col_name
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
      mi_spec = ic_mi_spectra(),
      c1 = c1,
      xlab = paste0("Time [", input$ic_par_mi_rt_unit, "]"),
      ylab = ylab,
      T_prog = pars$T_prog,
      s_focus = input$ic_par_focus_sample,
      pks = ic_table_peaks_edit(),
      BLmethod = input$baseline_method,
      sel_pk = input$ic_table_peaks_rows_selected
    )
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
      #browser()
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
    rt <- ic_mi_spectra()[[1]][,input$ic_par_rt_col]
    flt <- 1:length(rt)
    if (input$t_fltr > min(rt, na.rm=TRUE) & input$t_fltr < max(rt, na.rm=TRUE)) {
      flt <- which(rt<input$t_fltr)
    }
    ic_mi_spectra()[[1]][flt,c(input$ic_par_rt_col, input$ic_par_mi_col)]
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
