#' internal function that are used within exported functions
#'
#' functions to manipulate data.frame columns
#' - blcorr_col
#' - scale_col
#' - smooth_col
#'
#' functions to check input parameters
#' - check_Time_col
#' - check_iso_cols
#' - check_std_info
#' - check_peak_boundaries
#'
#' specific helper functions
#' - get_peak
#' - extract_unique_number

#' @title blcorr_col.
#' @description Applies baseline correction to a specific column of a data.frame.
#' @param df Input data.frame with at least one numeric column `nm`.
#' @param nm Column name of the column to be smoothed or numeric index.
#' @param BLmethod Method for baseline correction.
#' @param deg Degree of polynomial for baseline correction.
#' @param rval Specify to return the 'baseline' itself instead of the 'corrected' values.
#' @param amend See details.
#'
#' @details
#' Parameter `amend` is NULL on default which will cause the smoothed data to
#'     replace the original data. If `amend` contains a character string a new
#'     column will be added to the df named accordingly (see Examples).
#'
#' @return A data.frame.
#'
#' @examples
#' df <- ETVapp::ETVapp_testdata[["ExtGasCal"]][["Samples"]][[1]]
#' head(df)
#' head(blcorr_col(df = df, nm = "80Se"))
#' head(blcorr_col(df = df, nm = 2, amend = "_BLcorr"))
#' head(blcorr_col(df = df, nm = 3, rval = "baseline", amend = "_BL"))
#'
#' @keywords internal
#' @noRd
blcorr_col <- function(df, nm = NULL, BLmethod = c("modpolyfit", "none"), deg = 1L, rval = c("corrected", "baseline"), amend = NULL) {
  BLmethod <- match.arg(BLmethod)
  rval <- match.arg(rval)
  if (BLmethod == "modpolyfit"){
    if (is.numeric(deg) && deg[1]>=1) {
      if (is.null(nm)) nm <- ncol(df)
      if (is.character(nm) && nm %in% colnames(df)) idx <- colnames(df)==nm
      if (is.numeric(nm) && nm %in% 1:ncol(df)) idx <- nm
      nv <- try(baseline::baseline(t(df[,idx,drop=FALSE]), method = BLmethod, deg = deg), silent = TRUE)
      # $$JL: baseline::baseline sometimes returns NA only for both slots, corrected and baseline
      # this could (!) be fixed but should it ?
      # browser()
      if (rval=="corrected") nv <- baseline::getCorrected(nv)[1,]
      if (rval=="baseline") nv <- baseline::getBaseline(nv)[1,]
      if (is.null(amend)) {
        df[,idx] <- nv
      } else {
        df[,paste0(colnames(df)[idx], amend)] <- nv
      }
    } else {
      message("Parameter 'deg' should be an odd integer >=1.")
    }
  }
  if (BLmethod == "none") {
    message("BLmethod 'none' sselected. BL correction step omitted.")
  }
  return(df)
}

#' @title scale_col.
#' @description Scales one column in a data.frame with respect to another.
#' @param df Input data.frame with at least two numeric columns `nm` and `std`.
#' @param nm Column name of the column to be scaled or numeric index.
#' @param std Column name of the column used for scaling or numeric index.
#' @param amend See details.
#'
#' @details
#' Parameter `amend` is NULL on default which will cause the scaled data to
#'     replace the original data. If `amend` contains a character string a new
#'     column will be added to the df named accordingly (see Examples).
#'
#' @return A data.frame.
#'
#' @examples
#' df <- ETVapp::ETVapp_testdata[["oIDMS"]][["Samples"]][[1]]
#' head(df)
#' head(scale_col(df = df, nm = "117Sn", std = "80Se"))
#' head(scale_col(df = df, nm = "117Sn", std = "80Se", amend = "_scaled"))
#' head(scale_col(df = df, nm = 2, std = 3, amend = "_scaled"))
#' head(scale_col(df = df))
#'
#' @keywords internal
#' @noRd
scale_col <- function(df, nm = NULL, std = NULL, amend = NULL) {
  if (!is.null(nm) & !is.null(std)) {
    if (is.character(nm) && nm %in% colnames(df)) idx1 <- which(colnames(df)==nm)
    if (is.numeric(nm) && nm %in% 1:ncol(df)) idx1 <- nm
    if (is.character(std) && std %in% colnames(df)) idx2 <- which(colnames(df)==std)
    if (is.numeric(std) && std %in% 1:ncol(df)) idx2 <- std
    nv <- try(df[,idx1]*mean(df[,idx2], na.rm=TRUE)/df[,idx2], silent = TRUE)
    if (is.null(amend)) {
      df[,idx1] <- nv
    } else {
      df[,paste0(colnames(df)[idx1], amend)] <- nv
    }
  } else {
    message("Parameters 'nm' and 'std' should indicate two columns from df.")
  }
  return(df)
}

#' @title smooth_col.
#' @description Applies Savitzky-Golay smoothing to a specific column of a data.frame.
#' @param df Input data.frame with at least one numeric column `nm`.
#' @param nm Column name of the column to be smoothed or numeric index.
#' @param fl Filter length of smoother, has to be odd and >=3.
#' @param amend See details.
#'
#' @details
#' Parameter `amend` is NULL on default which will cause the smoothed data to
#'     replace the original data. If `amend` contains a character string a new
#'     column will be added to the df named accordingly (see Examples).
#'
#' @return A data.frame.
#'
#' @examples
#' df <- ETVapp::ETVapp_testdata[["oIDMS"]][["Samples"]][[1]]
#' head(df)
#' head(smooth_col(df = df, nm = "125Te"))
#' head(smooth_col(df = df, nm = 4))
#' head(smooth_col(df = df, nm = "125Te", amend = "_smoothed"))
#' head(smooth_col(df = df, nm = "80Se", fl = 13))
#'
#' @keywords internal
#' @noRd
smooth_col <- function(df, nm = NULL, fl = 5, amend = NULL) {
  if (!is.null(fl)) {
    if (is.null(nm)) nm <- ncol(df)
    if (is.character(nm) && nm %in% colnames(df)) idx <- which(colnames(df)==nm)
    if (is.numeric(nm) && nm[1] %in% 1:ncol(df)) idx <- nm[1]
    if (is.numeric(fl) && fl>=3 && identical(fl %% 2, 1)) {
      # JL: smoothing should be substituted against smooth.spline
      nv <- try(pracma::savgol(T = df[,idx], fl = fl), silent = TRUE)
      if (fl==151) {
        # JL: for testing: use fl=151 to try spline smoothing
        nv <- try(stats::smooth.spline(x = df[,idx])$y, silent = TRUE)
      }
      if (is.null(amend)) {
        df[,idx] <- nv
      } else {
        df[,paste0(colnames(df)[idx], amend)] <- nv
      }
    } else {
      if (is.null(amend)) {
        message("Parameter 'fl' should be an odd integer >=3. Omitting smoothing step.")
      } else {
        message("Parameter 'fl' should be an odd integer >=3. Amending original data.")
        df[,paste0(colnames(df)[idx], amend)] <- df[,idx]
      }
    }
  }
  return(df)
}

#' @title check_iso_cols.
#' @param df df.
#'
#' @keywords internal
#' @noRd
check_iso_cols <- function(df, iso1_col, iso2_col) {
  ensure_that(is.data.frame(df), "[check_iso_cols] df is no data.frame")
  ensure_that(all(c(iso1_col, iso2_col) %in% colnames(df)), "[check_iso_cols] df does not contain specified columns")
  ensure_that(is.numeric(df[,iso1_col]), "[check_iso_cols] c1 in df is not numeric")
  ensure_that(is.numeric(df[,iso2_col]), "[check_iso_cols] c2 in df is not numeric")
  ensure_that(iso1_col != iso2_col, "Different isotopes for determining the isotope ratio necessary. Omitting data processing steps.", opt = "warn")
  if (gsub("[^[:alpha:]]", "", iso1_col) != gsub("[^[:alpha:]]", "", iso2_col)) message("Different elements are selected. Please check the entries.")
  invisible(df)
}

#' @title check_Time_col.
#' @param df df.
#'
#' @keywords internal
#' @noRd
check_Time_col <- function(df) {
  stopifnot(is.data.frame(df))
  if ("Time" %in% colnames(df) && is.numeric(df[,"Time"])) {
    time_col <- "Time"
  } else {
    message("Column 'Time' not found. Selecting column '", colnames(df)[1],"' as time information.")
    time_col <- colnames(df)[1]
  }
  invisible(time_col)
}

#' @title check_std_info.
#' @param std_info Concentration values.
#' @param n correct length
#'
#' @keywords internal
#' @noRd
check_std_info <- function(std_info, n=1) {
  ensure_that(is.numeric(std_info), "Please enter a numeric value as <<std_info>>.")
  ensure_that(length(std_info) == n, "Provide as many <<std_info>> values as you have peaks.")
  ensure_that(all(std_info >= 0), "Please enter only positive values in <<std_info>>.")
  invisible(std_info)
}

#' @title check_peak_boundaries.
#' @param peak_start peak_start.
#' @param peak_end peak_end.
#' @param time time.
#' @keywords internal
#' @noRd
check_peak_boundaries <- function(peak_start, peak_end, time) {
  ensure_that(is.numeric(peak_start), "Please enter a numeric value as <<Signalstart>>.")
  ensure_that(is.numeric(peak_end), "Please enter a numeric value as <<Signalend>>.")
  ensure_that(peak_start < peak_end, "Please enter a signal start before the signal end.")
  ensure_that(peak_start <= max(time, na.rm=TRUE) & peak_end >= min(time, na.rm=TRUE), "Please enter signal boundaries within the analysis time or change 'Peak Method'.")
  invisible(NULL)
}

#' @title get_peak.
#' @description Applies Savitzky-Golay smoothing to a specific column of a data.frame.
#' @param df Input data.frame with numeric data in the second column.
#' @param PPmethod Peak picking method.
#' @param peak_start Value which is taken as peak start point, when manual peak picking is chosen.
#' @param peak_end Value which is taken as peak end point, when manual peak picking is chosen.
#' @param minpeakheight A threshold value for peak picking via peak height.
#' @param cf A correction value for cutting the area around the detected peak.
#'
#' @details
#' tbd
#'
#' @return A single row data.frame containing peak information.
#'
#' @examples
#' raw_data <- ETVapp::ETVapp_testdata[["oIDMS"]][["Samples"]][[1]]
#' df <- process_data(raw_data, c1 = "117Sn", c2 = "80Se", fl = 7)
#' get_peak(df, peak_start = 80, peak_end = 130, minpeakheight = 1000)
#'
#' @keywords internal
#' @noRd
get_peak <- function(df, PPmethod = c("Peak (height)", "Peak (manual)", "mean_signal"), peak_start = NULL, peak_end = NULL, minpeakheight = 1000, cf = 50) {
  PPmethod <- match.arg(PPmethod)
  if (PPmethod == "Peak (height)") {
    #browser()
    x <- df[,2]
    ensure_that(is.numeric(minpeakheight) && minpeakheight > 0, "Please enter a minimum peakheight >0.", opt = "stop")
    # $$JL: substituted findpeaks call
    #peak_data <- pracma::findpeaks(x, minpeakheight = minpeakheight, npeaks=3, sortstr=TRUE)
    peak_data <- pracma::findpeaks(x, sortstr = TRUE)
    ensure_that(length(peak_data) >= 1, msg = "No peak found. Adjust the minimal peakheight or try manual peak detection.", opt = "stop")
    ps <- peak_data[1,3]
    pe <- peak_data[1,4]
    n <- length(x)
    peak_data <- data.frame(
      "Peakheight" = max(peak_data[,1]),
      "Peakstart" = ps, #as index
      "Peakend" = pe,
      "Start_corr" = ps - ifelse(ps < cf, ps - 1, cf),
      "End_corr" = pe + ifelse(pe > (n - cf), n - pe, cf),
      check.names = FALSE
    )
  }
  if (PPmethod == "Peak (manual)") {
    check_peak_boundaries(peak_start=peak_start, peak_end=peak_end, time=df[,1])
    ps <- as.numeric(min(which(df[,1] >= peak_start)))
    pe <- as.numeric(max(which(df[,1] <= peak_end)))
    n <- nrow(df)
    peak_data <- data.frame(
      "Peakheight" = NA,
      "Peakstart" = ps, #as index
      "Peakend" = pe,
      "Start_corr" = ps - ifelse(ps < cf, ps - 1, cf),
      "End_corr" = pe + ifelse(pe > (n - cf), n - pe, cf),
      check.names = FALSE
    )
  }
  if (PPmethod == "mean_signal") {
    check_peak_boundaries(peak_start=peak_start, peak_end=peak_end, time=df[,1])
    start_index<- as.numeric(min(which(df[,1] >= peak_start))) #as index
    end_index <- as.numeric(max(which(df[,1] <= peak_end)))  #as index
    peak_data <- data.frame(
      "Isotope" = colnames(df)[2],
      "Start [s]" = peak_start,
      "End [s]" = peak_end,
      "Mean Signal [cps]" = mean(df[start_index : end_index,2], na.rm=TRUE),
      check.names = FALSE
    )
  }
  return(peak_data)
}

#' @keywords internal
#' @noRd
extract_unique_number <- function(x) {
  # extract numbers
  number_matches <- regmatches(x, gregexpr("\\d+", x))
  max_len <- max(sapply(number_matches, length))
  # convert to matrix
  number_matrix <- t(sapply(number_matches, function(n) {
    length(n) <- max_len
    return(n)
  }))
  # test for col where all unique
  for (i in seq_len(ncol(number_matrix))) {
    col <- number_matrix[, i]
    if (length(unique(col)) == length(col)) {
      return(as.numeric(col))
    }
  }
  return(NULL)
}
