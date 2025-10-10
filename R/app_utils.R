#' exported functions that solve minor problems
#'
#' # function reimplementations (to avoid dependencies)
#' - ldply_base
#' - str_sort_num
#'
#' perform simple calculations
#' - calc_N_sp
#' - calc_analyte_mass_as_element
#' - correct_ratio
#' - calc_massbias
#' - calc_massflow
#' - calc_cali_mod
#' - calc_transeff
#'
#' imported from IsoCor
#' - get_iso_amu
#' - help_the_user
#' - get_spectrum
#' - spec_pre_process

#' @title ldply_base
#' @param .data list.
#' @param .fun fun.
#' @export
ldply_base <- function(.data, .fun = identity) {
  result <- lapply(.data, .fun)
  df <- do.call(rbind, result)
  df <- data.frame(df, row.names = NULL, check.names = FALSE)
  return(df)
}

#' @title str_sort_num
#' @param x character vector.
#' @examples
#' x <- paste0("File", c(20, 1, 9, 11, 32, 100))
#' str_sort_num(x)
#' x <- c("Z1", x, "a", "Z10")
#' str_sort_num(x)
#' x <- c("b1_3", "b2_1_3", x)
#' str_sort_num(x)
#' @export
str_sort_num <- function(x) {
  # Zerlege die Strings in numerische und nicht-numerische Teile
  split_parts <- regmatches(x, gregexpr("\\d+|\\D+", x))

  # Konvertiere numerische Teile in Zahlen, nicht-numerische bleiben Strings
  key_list <- lapply(split_parts, function(parts) {
    lapply(parts, function(part) {
      if (grepl("^\\d+$", part)) as.numeric(part) else part
    })
  })

  # Finde die maximale Anzahl an Teilen
  max_len <- max(sapply(key_list, length))

  # Padding: kürzere Listen auffüllen mit NA
  padded_keys <- lapply(key_list, function(k) {
    if (length(k) < max_len) {
      k <- c(k, rep(NA, max_len - length(k)))
    }
    k
  })

  # In Data Frame umwandeln und Listen entpacken
  key_df <- as.data.frame(do.call(rbind, padded_keys), stringsAsFactors = FALSE)
  for (i in seq_along(key_df)) {
    key_df[[i]] <- unlist(key_df[[i]])
  }

  # Sortieren
  x[do.call(order, key_df)]
}

#' @title calc_N_sp.
#' @param c_sp Concentration of spike isotope in spike stock solution [mg/L].
#' @param V_sp Volume of spike solution [µL].
#' @param VF1 Dilution factor of spike solution.
#' @param M_sp Molar mass of spike.
#' @param M_sa Molar mass of sample.
#'
#' @export
calc_N_sp <- function(c_sp, V_sp, VF1, M_sp, M_sa) {
  stopifnot(is.finite(c(c_sp, V_sp, VF1, M_sp, M_sa)))
  N_sp <- ((c_sp / VF1 * V_sp) / M_sp) * M_sa #c_sp in mg/L, V_sp in µL
  return(N_sp)
}

#' @title calc_analyte_mass_as_element.
#' @param R_m A vector of isotope ratios.
#' @param K Massbias correction factor.
#' @param As_iso1 Natural abundance of the spike isotope.
#' @param As_iso2 Natural abundance of the sample isotope.
#' @param Asp_iso1 Abundance of the spike isotope in the spike.
#' @param Asp_iso2 Abundance of the sample isotope in the spike.
#' @param N_sp Amount of spike.
#'
#' @export
calc_analyte_mass_as_element <- function(R_m, K, Asp_iso1, Asp_iso2, As_iso1, As_iso2, N_sp) {
  as <- c("R_m", "K", "Asp_iso1", "Asp_iso2", "As_iso1", "As_iso2", "N_sp")
  for (a in as) {
    if (!exists(a)) { message("Argument '", a, "' was not provided."); stop() }
    if (!all(is.finite(get(a)))) { message("Argument '", a, "' was not finite."); stop() }
  }
  R_As <- As_iso1 / As_iso2
  stopifnot(is.finite(R_As))
  if (all(R_m >= R_As)) {
    R_corr <- R_m * K
    return((Asp_iso1 - R_corr * Asp_iso2)/(As_iso2 * R_corr - As_iso1) * N_sp)
  } else {
    message("The isotope ratio is below the limit for IDMS calculation. Increase the spike amount or select a more abundant sample isotope.")
  }
}

#' @title correct_ratio.
#' @param x Numeric vector of mass ratios (R_m).
#' @param K Massbias correction factor.
#' @param As_iso1 Natural abundance of the spike isotope.
#' @param As_iso2 Natural abundance of the sample isotope.
#'
#' @export
correct_ratio <- function(x, K = 1, As_iso1 = 7.68, As_iso2 = 4.63) {
  out <- K * x
  if (min(out) < (As_iso1 / As_iso2)) {
    message("The minimum isotope ratio is below the required value for IDMS calculation. Increasing spike amount or selection of different isotopes is necessary.")
  }
  return(out)
}

#' @title calc_massbias
#'
#' @description A mass bias correction factor will be determined from measurements
#'     without added spike.
#'
#' @details The factor can be entered manually, selected as mean value of several
#'     measurements or a specific value can be selected.
#'
#' @param R_m R_m.
#' @param As_iso1 Natural abundance of the spike isotope.
#' @param As_iso2 Natural abundance of the sample isotope.
#'
#' @return Numeric values K for each R_m.
#'
#' @examples
#' mb_imp <- ETVapp::ETVapp_testdata[["oIDMS"]][["Massbias"]]
#'
#' mb_peaks <- ldply_base(1:length(mb_imp), function(i) {
#'   get_isoratio(mb_imp[[i]], iso1_col = "117Sn", iso2_col = "122Sn",
#'     PPmethod = "Peak (manual)", peak_start = 70, peak_end = 105)
#' })
#'
#' calc_massbias(mb_peaks[,"R_m"], As_iso1 = 7.68, As_iso2 = 4.63)
#'
#' @export
#'
calc_massbias <- function (R_m, As_iso1, As_iso2) {
  K <- NULL
  if (length(R_m) > 0 & all(is.finite(c(R_m, As_iso1, As_iso2)))) {
    K <- (As_iso1 / As_iso2) / R_m
  } else {
    message("Data of the IDMS type 'w/o spike' is required for mass bias determination.")
  }
  return(K)
}

#' @title calc_massflow.
#' @description Transforms one column in a data.frame with respect to parameters.
#' @param x Input data.
#' @param n_trans Transport efficiency.
#' @param As_iso1 Natural abundance of the spike isotope.
#' @param As_iso2 Natural abundance of the sample isotope.
#' @param Asp_iso1 Abundance of the spike isotope in the spike.
#' @param Asp_iso2 Abundance of the sample isotope in the spike.
#' @param V_fl Sample inlet flow in mL/min.
#' @param c_sp Concentration of spike isotope in spike stock solution.
#' @param DF Dilution factor of spike solution.
#'
#' @details
#' xx
#' @return A numeric vector.
#'
#' @examples
#' calc_massflow(1:10)
#'
#' @export
calc_massflow <- function(x, n_trans = 16.64236, As_iso1 = 7.68, As_iso2 = 4.36, Asp_iso1 = 91.06, Asp_iso2 = 0.08, V_fl = 0.0075, c_sp = 19581.71, DF = 20) {

  mf_sp <- V_fl * n_trans * (1 / 60) * (c_sp / DF)
  return(mf_sp * ((Asp_iso1 - (x * Asp_iso2)) / ((As_iso2 * x) - As_iso1)))

}

#' @title calc_cali_mod.
#' @description \code{calc_cali_mod} will provide calibration results based on a linear
#'     regression model.
#' @details A calibration curve is provided by a linear fit of peak areas or mean
#'     signal intensities against analyte masses or gas flows from a data.frame
#'     containing at least two entries in rows.
#' @param df Data.frame with two columns.
#' @param wf Calibration method/Workflow (used to make assumptions regarding units in df).
#'
#' @return A data.frame containing slope and intercept with errors and R square.
#'
#' @examples
#' df <- data.frame(x=1:5, y=sort(runif(5)))
#' calc_cali_mod(df = df, wf = "ExtCal")
#' calc_cali_mod(df = df, wf = "ExtGasCal")
#' calc_cali_mod(df = df, wf = "oIDMS")
#' @export
calc_cali_mod <- function (df, wf = c("ExtCal", "ExtGasCal", "oIDMS")){
  #Check for missing values
  stopifnot(exprs = {
    "At least two finite entries are needed for linear regression." = exists("df") && is.data.frame(df) && sum(stats::complete.cases(df))>=2
  })

  if (any(!stats::complete.cases(df))) {
    message("Entries with missing values will be deleted.")
    df <- df[!stats::complete.cases(df),]
  }

  wf <- match.arg(wf)
  df.lm <- stats::lm(df[,2] ~ df[,1])
  df.lm.sum <- summary(df.lm)

  out <- data.frame(
    "Slope" = df.lm.sum$coefficients[2, 1],
    "Slope error" = df.lm.sum$coefficients[2, 2],
    "Intercept" = df.lm.sum$coefficients[1, 1],
    "Intercept error" = df.lm.sum$coefficients[1, 2],
    "R square" = df.lm.sum$r.squared,
    check.names = FALSE
  )

  if (wf == "ExtCal") colnames(out) <- paste(colnames(out), c("[cts/ng]", "[cts/ng]", "[cts]", "[cts]", ""))
  if (wf == "ExtGasCal") colnames(out) <- paste(colnames(out), c("[cts/\u00b5g]", "[cts/\u00b5g]", "[cps]", "[cps]", ""))
  if (wf == "oIDMS") colnames(out) <- paste(colnames(out), c("[cps L/\u00b5g]", "[cps L/\u00b5g]", "[cps]", "[cps]", ""))

  return(out)
}

#' @title calc_transeff
#' @description The transport efficiency will be calculated based on particle
#'     size approach from single particle-ICP-MS data.
#' @details Determination of the transport efficiency from measurements with
#'     gold or silver nano particle standards.
#' @param data A data.frame containing at least two columns.
#' @param int_col Intensity column.
#' @param LFD Intensity limit for the detection of particle signals.
#' @param cali_slope Calibration lm slope result for ionic standards.
#' @param V_fl Sample inlet flow in mL/min.
#' @param part_mat Particle material.
#' @param dia_part Particle diameter in nm.
#'
#' @return A data.frame containing single particle data.
#'
#' @examples
#' spion_imp <- ETVapp::ETVapp_testdata[["oIDMS"]][["sp_ionic"]]
#'
#' head(spion_imp[[1]])
#' names(spion_imp)
#' sp_cali <- ldply_base(1:length(spion_imp), function(i) {
#'   get_peakdata(spion_imp[[i]], "197Au", PPmethod = "mean_signal", peak_start = 0.003, peak_end = 60)
#' })
#' conc_ion <- c(20, 50, 100, 200, 500)
#' fac <- 1.661 * 0.01104347 * 12/44
#' sp_cali <- tab_cali(sp_cali, wf = "oIDMS", std_info = conc_ion, gas_flow = 49.6512, fac = fac)
#'
#' (cali_slope <- calc_cali_mod(df = sp_cali[,c(5,4)], wf = "oIDMS")[,1])
#'
#' sp_data <- ETVapp::ETVapp_testdata[["oIDMS"]][["sp_particle"]][[1]]
#' # filter time if required
#' sp_data_flt <- sp_data[sp_data[,"Time"]<60,]
#'
#' (n_trans <- calc_transeff(sp_data, int_col = "197Au", LFD = 20000, cali_slope,
#'   V_fl = 0.0075, part_mat = c("Au", "Ag"), dia_part = 60))
#' @export
calc_transeff <- function (data, int_col, LFD = 100, cali_slope = 1, V_fl, part_mat = c("Au", "Ag"), dia_part) {

  # ensure time_col
  time_col <- check_Time_col(data)

  # Additional parameter
  t_dw <- stats::median(diff(data[,time_col]),na.rm=TRUE) * 1000 #in ms
  fac <- (V_fl * t_dw * (100/6)) / cali_slope

  part_mat <- match.arg(part_mat) #in g/mL
  den_mat <- switch(
    part_mat,
    "Au" = 19.30,
    "Ag" = 10.49
  )
  part_mass <- 4/3 * pi * (dia_part / 2 / 10^7)^3 * den_mat * 10^15

  # mean background signal
  sig_back <- mean(data[data[,int_col]<LFD, int_col], na.rm=TRUE)

  # s particle data
  data <- data[data[,int_col]>LFD, ]
  data[,"Particle"] <- data[,int_col] - sig_back

  # $$JL: outcommented as not used below
  # (4) ist ein weiterer Kontrollplot für die App
  #data[,"Mass"] <- data[,"Particle"] / mean(data[,"Particle"]) * part_mass
  #data[,"Diameter"] <- pracma::nthroot(6 * data[,"Mass"] / (10^15 * pi * 19.30), 3) * 10^7

  part_mass_fg <- stats::median(data[,"Particle"]) * fac
  out <- data.frame(
    "Signal response [cps*L/\u00b5g]" = cali_slope, #in cps*L/µg
    "Detected particle number [/min]" = nrow(data) * 60 / diff(range(data[,time_col])), #in /min
    "Detected particle mass [fg]" = part_mass_fg, #in fg
    "Calculated trans_eff [%]" = part_mass / part_mass_fg * 100, #in %
    check.names = FALSE
  )

  return(out)
}

#' @title get_iso_amu.
#' @description \code{get_iso_amu} will take a string and try to identify an
#'   isotope name contained within. It will return the amu for this isotope.
#' @param x character.
#' @param isotopes Two column dataframe with isotope definitions.
#' @examples
#' \dontrun{
#' get_iso_amu(x="198Hg")
#' get_iso_amu(x="198Hg_corr")
#' get_iso_amu(x="X_32S_corr")
#' get_iso_amu(x="15S")
#' }
#' @return A single numeric value (0 in case that no isotope could be identified).
#' @keywords internal
#' @noRd
get_iso_amu <- function(x, isotopes=data.frame("isotope"=c("198Hg","32S"), "mass"=c(197.999,31.995))) {
  x <- as.character(x[1])
  val <- 0
  l <- which(isotopes[,"isotope"] == x)[1]
  if (!is.na(l)) {
    val <- isotopes[l,"mass"]
  } else {
    l <- unlist(sapply(isotopes[,"isotope"], function(i) {grep(i, x)}))
    if (length(l)>=1) val <- isotopes[isotopes[,"isotope"] == names(l),"mass"][1]
  }
  return(val)
}

#' @title help_the_user
#' @description Help Window: opens a modal with respective Help text
#'     (rendered from Rmd source file) for users.
#' @param filename Name of the file as string (if necessary, containing also path).
#' @return Returns the help text as HTML (opens a modal with helpt text as side effect).
#' @keywords internal
#' @noRd
#' @importFrom markdown markdownToHTML
help_the_user <- function(filename) {
  file_in <- list.files(path = shiny::resourcePaths()["www"], pattern = paste0(filename, ".[Rr][Mm][Dd]$"), recursive = TRUE, full.names = TRUE)[1]
  help_text <- NULL
  if (!file.exists(file_in)) {
    message("[help_the_user] cant find help file: ", file_in)
  } else {
    help_text <-
      shiny::showModal(
        shiny::modalDialog(
          shiny::withMathJax(
            shiny::HTML(
              #markdown::markdownToHTML(file = file_in, fragment.only = TRUE, extensions = c("tables","autolink","latex_math","superscript"))
              markdown::markdownToHTML(file = file_in, fragment.only = TRUE)
            )
          ),
          size = "l",
          easyClose = TRUE,
          footer = NULL,
          title = NULL
        )
      )
  }
  invisible(NULL)
}

#' @title ensure_that.
#' @description ensure_that.
#' @param test Name of the file as string (if necessary, containing also path).
#' @param msg msg.
#' @param opt opt.
#' @return Returns error handling.
#' @keywords internal
#' @noRd
ensure_that <- function(test, msg = "Fehler", opt = c("stop", "warn", "message")) {

  if (test) return(invisible(TRUE))

  opt <- match.arg(opt)

  in_shiny <- requireNamespace("shiny", quietly = TRUE) && shiny::isRunning()

  if (opt == "stop") {
    if (in_shiny) {
      shiny::validate(shiny::need(FALSE, msg))
    } else {
      stop(msg, call. = FALSE)
    }
  } else if (opt == "warn") {
    if (in_shiny) {
      shiny::showModal(
        shiny::modalDialog(
          title = "Warning",
          msg,
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        )
      )
    } else {
      warning(msg, call. = FALSE)
    }
  } else if (opt == "message") {
    message(msg)
  } else {
    stop("Unknown value for 'opt'. Use 'stop', 'warn' or 'message'.", call. = FALSE)
  }

  invisible(FALSE)
}


#' @title extract_shift_and_cut.
#' @description Will extract_shift_and_cut objecst from a list of tables which
#'     contain #'     similar columns used as RT/mass and intensity source
#'     respectively.
#' @param data List of data.frames with at least two numeric columns.
#' @param rt_col Column name of the RT/mass column to be used.
#' @param c1 Column name of the intensity column to be used.
#' @param c2 Column name of the second intensity column to be used.
#' @param cut_range List with elements 'min' and 'max' to limit the RT/mass column.
#' @param rt_shift Vector of length(data) to align the RT/mass column.
#' @return A list of data.frames.
#' @keywords internal
#' @noRd
extract_shift_and_cut <- function(data, rt_col = "Minutes", c1, c2 = "", cut_range = NULL, rt_shift = NULL) {
  # checks
  shiny::validate(shiny::need(is.list(data), "[get_spectrum] data is not a list"))
  shiny::validate(shiny::need(length(names(data))>=1, "[get_spectrum] data is not named"))
  shiny::validate(shiny::need(all(sapply(data, function(x) { all(c(rt_col, c1) %in% colnames(x)) })), "[get_spectrum] data elements do not contain required columns"))
  shiny::validate(shiny::need(all(sapply(data, function(x) { all(diff(x[,rt_col])>0) })), message = "[get_spectrum] You selected a time column with non continuous values"))
  # potential defaults
  if (is.null(rt_shift)) rt_shift <- rep(0, length(data))
  if (is.null(cut_range)) cut_range <- list("min"=0, "max"=max(sapply(data, function(x) { max(x[,rt_col], na.rm=TRUE) })))
  # limit data
  stats::setNames(lapply(1:length(data), function(k) {
    x <- data[[k]]
    m <- x[, rt_col]
    m <- m-rt_shift[k]
    flt <- m>=cut_range$min & m<=cut_range$max
    m <- m[flt]
    if (c2 %in% colnames(data[[k]])) {
      stats::setNames(data.frame(m, x[flt, c1], x[flt, c2]), c(rt_col, c1, c2))
    } else {
      stats::setNames(data.frame(m, x[flt, c1]), c(rt_col, c1))
    }
  }), names(data))
}

#' @title spec_pre_process
#' @description Applies smoothing and BL correction..
#' @param data List of data.frames containing a "Time" and intensity columns.
#' @param wf Calibration method/Workflow.
#' @param c1 Column name of the intensity column to be used (ExtCal) or of the spike isotope (IDMS).
#' @param c2 Column name of the intensity column to be used as internal standard for Argon correction (ExtCal) or sample isotope (IDMS).
#' @param fl Filter length, has to be odd.
#' @return A list of data.frames.
#' @keywords internal
#' @noRd
spec_pre_process <- function(data, c1 = "13C", c2 = "80Se", fl = 7, wf = c("IDMS", "oIDMS", "ExtCal", "ExtGasCal")) {

  wf <- match.arg(wf)

  out <- stats::setNames(lapply(data, function(x) {
    process_data(data = x, wf = wf, c1 = c1, c2 = c2, fl = fl, amend = TRUE)
  }), names(data))

  return(out)
}

#' @keywords internal
#' @noRd
limit_digits <- function(df, n=6, cols=NULL) {
  if (is.null(cols)) {
    cols <- which(sapply(1:ncol(df), function(i) { is.numeric(df[,i]) }))
  } else {
    ensure_that(all(cols %in% 1:ncol(df)), "[limit_digits] Check parameters 'cols'.")
  }
  for (i in cols) df[,i] <- signif(x = df[,i], digits = n)
  return(df)
}

#' @title find_peak_boundaries
#' @description \code{find_peak_boundaries} will find the start and end point
#'     of a peak based on curve derivative.
#' @details This function provides a simple detection algorithm for peak boundaries.
#'     It will accept a numeric vector as input and determine relative to the global maximum
#'     (or a user provided local maximum) the left and right border where intensity decrease
#'     ends and intensity is increasing again.
#' @param int Numeric vector (of intensity values).
#' @param p Index of peak position (usually 'which.max(int)).
#' @param k Number of scans at peak boarder to confirm peak valley.
#' @param min_scans Minimum number of scans in front or tail.
#' @param noise A threshold value. All Values below or equal to noise will be set to zero.
#' @return A numeric vector of length 2 giving the indexes of peak start and peak end.
#' @examples
#' \dontrun{
#' x <- sin(seq(-pi,2*pi,0.01))+1
#' plot(x)
#' abline(v=find_peak_boundaries(x))
#' }
#' @keywords internal
#' @noRd
find_peak_boundaries <- function(int=NULL, p=which.max(int), k=3, min_scans=3, noise=0) {
  int[!is.finite(int)] <- 0
  int[int<=noise] <- 0
  idx <- 1:length(int)
  n <- length(idx)
  test_front <- diff(int[1:p])<=0
  test_front <- which(rev(cumsum(as.numeric(rev(test_front))))==k)
  test_front <- ifelse(length(test_front)>=1, max(test_front)+2, 1)
  lb <- idx[max(c(min(c(p-min_scans,test_front)),1))]
  test_tail <- diff(int[p:n])>=0
  test_tail <- which(cumsum(as.numeric(test_tail))==k)
  test_tail <- ifelse(length(test_tail)>=1, p+min(test_tail)-2, n)
  rb <- idx[min(c(max(c(p+min_scans,test_tail)),n))]
  return(idx[c(lb,rb)])
}
