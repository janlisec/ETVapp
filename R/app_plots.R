#' @title ic_specplot
#' @description Generate an annotated plot of one to several spectra
#'     to visualize the processing results of ic_app().
#' @param opt Vector of keywords to hide/show specific elements of the plot.
#' @param xrng Numeric vector of length 2 specifying plotting range for x.
#' @param mi_spec Spectra (list of data.frame objects).
#' @param c1 Main isotope.
#' @param c2 Secondary isotope.
#' @param ylab Character to be used for left axis labeling.
#' @param ylab2 Character to be used for secondary axis labeling.
#' @param s_focus Index of sample within focus.
#' @param pks Data frame of peaks with columns 'Sample', 'Peak ID', 'Scan start' and 'Scan end'.
#' @param BLmethod Hide or show baseline.
#' @param sel_pk Selected peak as numeric index of length one.
#' @importFrom graphics abline axTicks axis box legend lines mtext par points segments
#' @importFrom grDevices grey
#' @return An annotated plot of one to several spectra.
#' @examples
#' if (interactive()) {
#'   testdata <- ETVapp::ETVapp_testdata[["ExtGasCal"]][["Samples"]]
#'   mi_spec <- lapply(testdata, function(x) {
#'     x[,c("Time", "13C", "80Se")]
#'   })
#'   ETVapp:::ic_specplot(mi_spec=mi_spec)
#'   ETVapp:::ic_specplot(
#'     opt = c("overlay_mi", "overlay_legend", "overlay_si", "overlay_drift"),
#'     mi_spec=mi_spec, c1="13C", c2="80Se"
#'   )
#' }
#' @keywords internal
#' @noRd
ic_specplot <- function(
  opt = "",
  xrng = NULL,
  mi_spec = NULL,
  c1 = "13C",
  c2 = "80Se",
  xlab = paste0("Time [", "min", "]"),
  ylab = "Intensity [V]",
  ylab2 = paste0("32S", "/", "34S"),
  s_focus = "Sample 1",
  pks = NULL,
  BLmethod = "none",
  sel_pk = NULL
) {
  par(cex = 1.4)
  if (is.null(xrng)) xrng <- range(sapply(mi_spec, function(x) { range(x[,"Time"], na.rm=TRUE) }))
  # get y range
  yrng <- c(0, max(sapply(mi_spec, function(x) { max(x[,c(c1, if (c2 %in% colnames(x)) c2 else NULL)], na.rm=TRUE) })))
  # modify plot margins
  par(mar = c(4.5, 4.5, 0.5, ifelse("overlay_drift" %in% opt, 4.5, 0.5)))
  # render base plot
  plot(x = xrng, y = yrng, type = "n", xaxs = "i", xlab = xlab, ylab = ylab)
  if ("overlay_mi" %in% opt) {
    idx_all <- 1:length(mi_spec)
    cols <- 2:(length(idx_all)+1)
  } else {
    idx_all <- as.numeric(gsub("[^[:digit:]]", "", s_focus))
    cols <- rep(1, idx_all)
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
    if ("overlay_si" %in% opt && c2 %in% colnames(mi_spec[[idx]])) {
      lines(
        x = mi_spec[[idx]][flt,"Time"],
        y = mi_spec[[idx]][flt,c2],
        col = cols[idx],
        lty = 2
      )
    }

    if (!is.null(pks) && nrow(pks)>=1) {
      # highlight peak selected in table
      if (!is.null(sel_pk) && pks[sel_pk,"Sample"]==idx) {
        flt <- sm>=pks[sel_pk,"Start [s]"] & sm<=pks[sel_pk,"End [s]"]
        lines(x = sm[flt], y = si[flt], col=cols[pks[sel_pk,"Sample"]], lwd=3)
      }
      if ("overlay_drift" %in% opt) {
        # tbd
      }
      if ("overlay_pb" %in% opt) {
        pks_sam <- pks[pks[,"Sample"]==idx,]
        for (j in 1:nrow(pks_sam)) {
          abline(v=pks_sam[j,c("Start [s]", "End [s]")], col=cols[idx])
          #browser()
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

#' @title plot_particle_size_distribution.
#' @description plot_particle_size_distribution.
#' @details Determination of the transport efficiency from measurements with
#'     gold or silver nano particle standards.
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
#' head(plot_particle_size_distribution(x = sp_data))
#'
#' @export
plot_particle_size_distribution <- function(
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
  graphics::hist(x[,"Diameter"], main="", xlab="Particle Diameter")
  invisible(x)
}

#' @title plot_particle_diameter
#' @description plot_particle_diameter
#' @details Determination of the transport efficiency from measurements with
#'     gold or silver nano particle standards.
#' @param x A data.frame containing at least two columns.
#' @param LFD Intensity limit for the detection of particle signals.
#'
#' @return A data.frame containing single particle data (invisible) and a plot.
#'
#' @examples
#' sp_data <- ETVapp::ETVapp_testdata[["oIDMS"]][["sp_particle"]][[1]]
#' plot_particle_diameter(x = sp_data[,2])
#'
#' @export
plot_particle_diameter <- function(x, LFD = 20000) {
  par(mar=c(5,4,0,0)+0.5)
  par(cex = 1.4)
  rng <- floor(min(log10(x[x>0]))):floor(max(log10(x[x>0])))
  brks <- log10(unique(unlist(lapply(rng, function(i) { seq(10^i,10^(i+1),length.out=10) }))))
  par(cex = 1.4)
  graphics::hist(log10(x[x>0]), breaks=brks, axes = FALSE, xlab = "Intensity [cps]", main = "")
  axis(2)
  axis(1, at = c(rng, max(rng)+1), labels = 10^c(rng, max(rng)+1))
  axis(1, at = brks, labels = FALSE, tcl=-0.25)
  abline(v=log10(LFD), col=2)
  invisible(x)
}
