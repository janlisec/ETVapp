testthat::test_that(
  desc = "Test that get_peakdata works",
  code = {
    raw_data <- ETVapp::ETVapp_testdata[["oIDMS"]][["Samples"]][[1]]
    pro_data <- process_data(raw_data, c1 = "117Sn", c2 = "80Se", fl = 151)
    peak_data <- get_peakdata(pro_data, int_col = "117Sn")

    testthat::expect_equal(ncol(peak_data), 5)
    testthat::expect_equal(colnames(peak_data), c("Isotope","Start [s]","End [s]","Area [cts]","BLmethod"))
  }
)

testthat::test_that(
  desc = "Test that get_peakdata error messages work",
  code = {
    raw_data <- ETVapp::ETVapp_testdata[["oIDMS"]][["Samples"]][[1]]
    pro_data <- process_data(raw_data, c1 = "117Sn", c2 = "80Se", fl = 151)
    peak_data <- get_peakdata(pro_data, int_col = "117Sn")

    testthat::expect_error(get_peakdata(pro_data, int_col = "117Sn", PPmethod = "mean signal"))
  }
)
