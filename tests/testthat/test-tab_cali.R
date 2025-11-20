testthat::test_that(
  desc = "Test that tab_cali works",
  code = {
    cali_imp <- ETVapp::ETVapp_testdata[["ExtCal"]][["Cali"]]
    cali_pro <- lapply(cali_imp, function(x) {
      process_data(x, c1 = "157", fl = 7)
    })
    peak_start <- rep(145, length(cali_pro))
    peak_end <- seq(180, 230, length.out=length(cali_pro))
    cali_pks <- ldply_base(1:length(cali_pro), function(i) {
      pk <- get_peakdata(cali_pro[[i]], PPmethod = "Peak (manual)", int_col = "157",
                         peak_start = peak_start[i], peak_end = peak_end[i])
      })
    cali_peaks <- tab_cali(peak_data = cali_pks, wf = "ExtCal", ExtCal_unit = "pg", std_info = seq(0,50,10))
    cali_sig <- tab_cali(peak_data = cali_pks, wf = "ExtGasCal", ExtGasCal_unit = "\u00b5L/min", std_info = seq(0,50,10), fac = 2)
    cali_sp <- tab_cali(peak_data = cali_pks, wf = "oIDMS", ExtGasCal_unit = "\u00b5L/min", std_info = seq(0,50,10), fac = 2)

    testthat::expect_true(is.data.frame(cali_peaks))
    testthat::expect_equal(ncol(cali_peaks), 6)
    testthat::expect_true(is.numeric(cali_peaks[,c(6)]))
    testthat::expect_true(grepl("pg", names(cali_peaks)[6]))
    testthat::expect_equal(cali_peaks[, c(6)], seq(0,50,10))

    testthat::expect_true(is.data.frame(cali_sig))
    testthat::expect_equal(ncol(cali_sig), 7)
    testthat::expect_true(is.numeric(cali_sig[,c(7)]))
    testthat::expect_true(grepl("\u00b5L/min", names(cali_sig)[6]))
    testthat::expect_true(grepl("ng/s", names(cali_sig)[7]))
    testthat::expect_equal(cali_sig[,7], cali_sig[,6] * 2 * 1000/60)

    testthat::expect_true(is.data.frame(cali_sp))
    testthat::expect_equal(ncol(cali_sp), 6)
    testthat::expect_true(is.numeric(cali_sp[,c(6)]))
    testthat::expect_true(names(cali_sp)[6] %in% "Concentration [\u00b5g/L]")
  }
)
