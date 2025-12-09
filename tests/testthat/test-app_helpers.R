testthat::test_that(
  desc = "Test that app_helpers works",
  code = {
    df <- ETVapp::ETVapp_testdata[["ExtGasCal"]][["Samples"]][[1]]
    test1 <- blcorr_col(df = df, nm = "13C")
    test2 <- blcorr_col(df = df, nm = 2, amend = "_BLcorr")
    test3 <- blcorr_col(df = df, nm = 3, rval = "baseline", amend = "_BL")

    testthat::expect_true(is.data.frame(test1))
    testthat::expect_equal(ncol(test1), 4)
    testthat::expect_true(is.data.frame(test2))
    testthat::expect_equal(ncol(test2), 5)
    testthat::expect_equal(colnames(test2)[5],"13C_BLcorr")
    testthat::expect_true(is.data.frame(test3))
    testthat::expect_equal(ncol(test3), 5)
    testthat::expect_equal(colnames(test3)[5],"80Se_BL")

    df <- ETVapp::ETVapp_testdata[["oIDMS"]][["Samples"]][[1]]
    test1 <- scale_col(df = df, nm = "117Sn", std = "80Se")
    test2 <- scale_col(df = df, nm = "117Sn", std = "80Se", amend = "_scaled")
    test3 <- scale_col(df = df, nm = 2, std = 4, amend = "_scaled")
    testthat::expect_message(test4 <- scale_col(df = df), "should indicate")

    testthat::expect_true(is.data.frame(test1))
    testthat::expect_equal(ncol(test1), 7)
    testthat::expect_true(is.data.frame(test2))
    testthat::expect_equal(ncol(test2), 8)
    testthat::expect_equal(colnames(test2)[8],"117Sn_scaled")
    testthat::expect_equal(test2, test3)
    testthat::expect_equal(test4, df)

    df <- ETVapp::ETVapp_testdata[["oIDMS"]][["Samples"]][[1]]
    test1 <- smooth_col(df = df, nm = "125Te")
    test2 <- smooth_col(df = df, nm = 7)
    test3 <- smooth_col(df = df, nm = "125Te", amend = "_smoothed")

    testthat::expect_true(is.data.frame(test1))
    testthat::expect_equal(ncol(test1), 7)
    testthat::expect_equal(test1, test2)
    testthat::expect_true(is.data.frame(test3))
    testthat::expect_equal(ncol(test3), 8)
    testthat::expect_equal(colnames(test3)[8], "125Te_smoothed")

    raw_data <- ETVapp::ETVapp_testdata[["oIDMS"]][["Samples"]][[1]]
    test <- check_Time_col(raw_data)
    testthat::expect_true(is.character(test))

    df <- process_data(raw_data, c1 = "117Sn", c2 = "80Se", fl = 7)
    peaks <- get_peak(df, peak_start = 80, peak_end = 130, PPmethod = "Peak (manual)")

    testthat::expect_true(is.data.frame(peaks))
    testthat::expect_equal(ncol(peaks), 5)
    testthat::expect_equal(colnames(peaks)[4], "Start_corr")
    testthat::expect_equal(colnames(peaks)[5], "End_corr")
    testthat::expect_equal(peaks[,2], 427)
    testthat::expect_equal(peaks[,3], 692)

    x <- paste("File123_conc", c(3, 21, 101), "endtext", sep="_")
    test <- extract_unique_number(x=x)

    testthat::expect_true(is.numeric(test))
    testthat::expect_equal(test, c(3, 21, 101))
  }
)

testthat::test_that(
  desc = "Test that app_helpers messages work",
  code = {
    df <- ETVapp::ETVapp_testdata[["oIDMS"]][["Samples"]][[1]]
    dft <- df[, -c(1)]
    x <- c(1)

    testthat::expect_message(blcorr_col(df = df, nm = "117Sn", deg = 0))
    testthat::expect_message(blcorr_col(df = df, nm = "117Sn", BLmethod = "none"))
    testthat::expect_message(scale_col(df = df, nm = "117Sn", std = NULL))
    testthat::expect_message(smooth_col(df = df, nm = 2, fl = 0, amend = "_smoothed"))
    testthat::expect_error(check_iso_cols(x))
    testthat::expect_error(check_iso_cols(df, "113Cd", "111Cd"))
    testthat::expect_message(check_iso_cols(df, "117Sn", "80Se"))
    testthat::expect_message(check_iso_cols(dft, "117Sn", "80Se"))
    testthat::expect_error(check_std_info(1:10))
    testthat::expect_error(check_std_info(c("a", "B"), n = 2))
    testthat::expect_error(check_std_info(c(-1, 2), n = 2))
    testthat::expect_error(check_peak_boundaries("a", 10, time))
    testthat::expect_error(check_peak_boundaries(2, "B", time))
    testthat::expect_error(get_peak(df, PPmethod = "Peak (height)", minpeakheight = "a"))
    testthat::expect_error(get_peak(df, PPmethod = "Peak (height)", minpeakheight = 10e+07))
  }
)
