#' Spectra and peaks plot.
#'
#' Generate an annotated plot of one to several spectra.
#'
#' @param opt Vector of keywords to hide/show specific elements of the plot.
#' @param xrng Numeric vector of length 2 specifying plotting range for x.
#' @param yrng Numeric vector of length 2 specifying plotting range for y.
#' @param mi_spec Spectra (list of data.frame objects).
#' @param c1 Main isotope.
#' @param xlab Character to be used for bottom axis labeling.
#' @param ylab Character to be used for left axis labeling.
#' @param T_prog Temperature program (a data frame with columns Time and Temp).
#' @param s_focus Index of sample within focus.
#' @param pks Data frame of peaks with columns 'Sample', 'Peak ID', 'Scan start' and 'Scan end'.
#' @param BLmethod Hide or show baseline.
#' @param sel_pk Selected peak as numeric index of length one.
#'
#' @importFrom graphics abline axTicks axis box legend lines mtext par points segments
#' @importFrom grDevices grey
#' @return An annotated plot of one to several spectra.
#' @examples
#' if (interactive()) {
#'   wf <- "ExtGasCal"
#'   c1 <- "13C"
#'   td <- ETVapp::ETVapp_testdata[[wf]][["Samples"]]
#'   pro_data <- process_data(td, wf=wf, c1=c1, fl=9)
#'   plot_spec(mi_spec = pro_data, opt = "overlay_legend")
#'   peak_data <- cbind(
#'     "Sample"=1:length(pro_data),
#'     get_peakdata(pro_data, int_col = c1, minpeakheight = 10^6)
#'   )
#'   T_prog <- data.frame(
#'     "Time"=seq(min(pro_data[[1]][,"Time"]), max(pro_data[[1]][,"Time"]), length.out=7),
#'     "Temp"=c(20,20,50,50,150,170,170)
#'   )
#'   plot_spec(
#'     mi_spec = pro_data, opt=c("overlay_pb", "overlay_Temp"), BLmethod = "modpolyfit",
#'     T_prog = T_prog, pks = peak_data, s_focus = 2
#'   )
#' }
#' @export
plot_spec <- function(
  opt = "",
  xrng = NULL,
  yrng = NULL,
  mi_spec = NULL,
  c1 = "13C",
  xlab = paste0("Time [", "s", "]"),
  ylab = "Intensity [cps]",
  T_prog = NULL,
  s_focus = NULL,
  pks = NULL,
  BLmethod = "none",
  sel_pk = NULL
) {
  # determine data ranges to display
  if (is.null(s_focus)) {
    idx_all <- 1:length(mi_spec)
  } else {
    idx_all <- as.numeric(gsub("[^[:digit:]]", "", s_focus))
  }
  if (!all(idx_all %in% 1:length(mi_spec))) message("Error in ic_specplot -- check")
  if (length(idx_all) == 1) { cols <- rep(1, idx_all) } else { cols <- 2:(max(idx_all)+1) }
  if (is.null(xrng)) xrng <- range(sapply(mi_spec[idx_all], function(x) { range(x[,"Time"], na.rm=TRUE) }))
  if (is.null(yrng)) yrng <- range(sapply(mi_spec[idx_all], function(x) { c(0, max(x[,c1], na.rm=TRUE)) }))

  # modify plot margins
  par(mar = c(4, 4, 0, ifelse(is.null(T_prog) || all(T_prog==""), 0, 4)) + 0.1)
  par(cex = 1.4)
  # render base plot
  plot(x = xrng, y = yrng, type = "n", xaxs = "i", yaxs = "i", xlab = xlab, ylab = ylab)
  if (!is.null(dim(T_prog)) & "overlay_Temp" %in% opt) {
    Temp_ticks <- pretty(range(T_prog[,"Temp"]))
    Temp_ori <- seq(yrng[1], yrng[2], length.out=length(Temp_ticks))
    T_prog[,"Temp_rescaled"] <- stats::predict(stats::lm(Temp_ori ~ Temp_ticks), newdata = data.frame("Temp_ticks" = T_prog[,"Temp"]))
    lines(x=T_prog[,1], y = T_prog[,3])
    axis(side = 4, at = Temp_ori, labels = Temp_ticks)
    mtext(text = "Temperature [\u00b0C]", side = 4, line = 3, cex=par("cex.lab")*par("cex"))
  }
  for (idx in idx_all) {
    if ("overlay_legend" %in% opt) {
      f_in <- names(mi_spec)
      mtext(text = paste0("[",idx,"] ", f_in[idx]), side = 3, line = -1.15*idx, adj = 0.02, font = 1, col=cols[idx])
    }
    sm <- mi_spec[[idx]][,"Time"]
    si <- mi_spec[[idx]][,c1]
    flt <- sm>=xrng[1] & sm<=xrng[2]
    lines(x = sm[flt], y = si[flt], col=cols[idx])
    if (!is.null(pks) && nrow(pks)>=1) {
      # highlight peak selected in table
      if (!is.null(sel_pk) && pks[sel_pk,"Sample"]==idx) {
        flt <- sm>=pks[sel_pk,"Start [s]"] & sm<=pks[sel_pk,"End [s]"]
        lines(x = sm[flt], y = si[flt], col=cols[pks[sel_pk,"Sample"]], lwd=3)
      }
      if ("overlay_pb" %in% opt) {
        pks_sam <- pks[pks[,"Sample"]==idx,]
        for (j in 1:nrow(pks_sam)) {
          abline(v=pks_sam[j,c("Start [s]", "End [s]")], col=cols[idx])
          if (BLmethod != "none") {
            # plot baseline of peak
            check_peak_boundaries(peak_start = pks_sam[j,"Start [s]"], peak_end = pks_sam[j,"End [s]"], time = sm)
            cf <- 50
            p_left <- min(which(sm>=pks_sam[j,"Start [s]"]))
            p_right <- max(which(sm<=pks_sam[j,"End [s]"]))
            flt <- max(1, p_left-cf):min(length(sm), p_right+cf)
            BL <- blcorr_col(
              df = data.frame("Time"=sm[flt], "Int"=si[flt]),
              nm = "Int",
              BLmethod = BLmethod,
              rval = "baseline",
              amend = "_BL"
            )
            lines(x = BL[,"Time"], y = BL[,"Int_BL"], col=cols[idx])
          }
        }
      }
    }
  }
}

#' Histogram plot of particle size distribution.
#'
#' Plots a histogram of the particle size distribution of a single particle-ICP-MS measurement.
#'
#' @details Check the particle size distribution for the quality of the nano particle standard and single particle-ICP-MS measuremet.
#'     Densities for gold or silver nano particle standards are deposited.
#' @param x A data.frame containing at least two columns.
#' @param cali_slope Calibration lm slope result for ionic standards.
#' @param V_fl Sample inlet flow in mL/min.
#' @param part_mat Particle material.
#' @param dia_part Particle diameter in nm.
#' @param LFD Intensity limit for the detection of particle signals.
#'
#' @return A data.frame containing single particle data (invisible) and a plot.
#'
#' @examples
#' sp_data <- ETVapp::ETVapp_testdata[["oIDMS"]][["sp_particle"]][[1]]
#' head(plot_particle_diameter(x = sp_data))
#'
#' @export
plot_particle_diameter <- function(
    x, cali_slope = 1488, V_fl = 0.0075, part_mat = c("Au", "Ag"), dia_part = 60, LFD = 20000
) {
  par(mar=c(5,4,0,0)+0.5)
  par(cex = 1.4)
  t_dw <- stats::median(diff(x[,1]), na.rm=TRUE) * 1000
  fac <- (V_fl * t_dw * (100/6)) / cali_slope

  part_mat <- match.arg(part_mat)
  den_mat <- switch(
    part_mat,
    "Au" = 19.30,
    "Ag" = 10.49
  )
  part_mass <- 4/3 * pi * (dia_part / 2 / 10^7)^3 * den_mat * 10^15

  sig_bg <- mean(x[x[,2]<LFD, 2], na.rm=TRUE)

  x <- x[x[,2]>LFD, ]
  x[,"Particle"] <- x[,2] - sig_bg
  x[,"Mass"] <- x[,"Particle"] / mean(x[,"Particle"]) * part_mass
  x[,"Diameter"] <- pracma::nthroot(6 * x[,"Mass"] / (10^15 * pi * 19.30), 3) * 10^7
  graphics::hist(x[,"Diameter"], main="", xlab="Particle diameter [nm]")
  invisible(x)
}

#' Plots the signal distribution.
#'
#' Plots the signal distribution of a single particle-ICP-MS measurement.
#'
#' @details Determination of the intensity limit for the differentiation between particle and background signals.
#' @param x A data.frame containing at least two columns.
#' @param LFD Intensity limit for the detection of particle signals.
#' @param style Choose plotting style.
#' @param ylim Specify ylim for style = counts.
#'
#' @return A data.frame containing single particle data (invisible) and a plot.
#'
#' @examples
#' sp_data <- ETVapp::ETVapp_testdata[["oIDMS"]][["sp_particle"]][[1]]
#' plot_signal_distribution(x = sp_data[,2])
#' plot_signal_distribution(x = sp_data[,2], style="counts")
#' @export
plot_signal_distribution <- function(x, LFD = 20000, style = c("hist", "counts"), ylim = NULL) {
  style <- match.arg(style)
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  if (style=="hist") {
    par(mar=c(5,4,0,0)+0.5)
    par(cex = 1.4)
    rng <- floor(min(log10(x[x>0]))):floor(max(log10(x[x>0])))
    brks <- log10(unique(unlist(lapply(rng, function(i) { seq(10^i,10^(i+1),length.out=10) }))))
    graphics::hist(log10(x[x>0]), breaks=brks, axes = FALSE, xlab = "Intensity [cps]", main = "")
    axis(2)
    axis(1, at = c(rng, max(rng)+1), labels = 10^c(rng, max(rng)+1))
    axis(1, at = brks, labels = FALSE, tcl=-0.25)
    abline(v=log10(LFD), col=2)
  }
  if (style=="counts") {
    x <- table(x)
    plot(x = round(as.numeric(names(x))), y = x, log = "x", type="h", ylab = "Frequency", xlab = "Intensity [cps]", main = "Signal distribution", ylim = ylim)
    abline(v=LFD, col=2, lwd=3)
  }
  invisible(x)
}
