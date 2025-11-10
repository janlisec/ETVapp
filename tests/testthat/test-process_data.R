testthat::test_that(
  desc = "Test that process_data works",
  code = {
    imp <- ETVapp::ETVapp_testdata[['ExtGasCal']][['Samples']][[1]]
    pro_data <- process_data(imp, c1 = "13C", c2 = "80Se", fl = 151)
    amend_data <- process_data(imp, c1 = "13C", c2 = "80Se", fl = 151, amend = TRUE)
    smooth_data <- process_data(imp, c1 = "13C", c2 = NULL, fl = 151, amend = TRUE)
    scale_data <- process_data(imp, c1 = "13C", c2 = "80Se", fl = 1, amend = TRUE)

    iso_imp <- ETVapp::ETVapp_testdata[["oIDMS"]][['Samples']][[1]]
    iso_data <- process_data(iso_imp, wf = "oIDMS", c1 = "117Sn", c2 = "122Sn", fl = 5, amend = TRUE)

    testthat::expect_true(is.data.frame(pro_data))
    testthat::expect_equal(names(pro_data), c("Time", "13C", "80Se"))
    testthat::expect_equal(names(amend_data), c("Time", "13C", "80Se", "13C_smooth", "80Se_smooth", "13C_smooth_scale"))
    testthat::expect_equal(names(smooth_data), c("Time", "13C", "13C_smooth"))
    # $$VS: This should be the output when the smoothing step is omitted.
    #       "13C_smooth_scale" is misleading, when the data is not smoothed.
    #       If amend is "TRUE" and smoothing is omnitted, original data should not be amended in smooth_col.
    # testthat::expect_equal(names(scale_data), c("Time", "13C", "80Se", "13C_scale"))
    testthat::expect_equal(names(iso_data), c("Time", "117Sn", "122Sn", "117Sn_smooth", "122Sn_smooth", "R_m"))

    testthat::expect_equal(amend_data[100,"13C_smooth_scale"], c(amend_data[100,"13C_smooth"]*mean(amend_data[100,"80Se_smooth"], na.rm=TRUE)/amend_data[100,"80Se_smooth"]), tolerance = 0.1)
    testthat::expect_equal(iso_data[100,"R_m"], c(iso_data[100,"117Sn_smooth"]/iso_data[100,"122Sn_smooth"]), tolerance = 0.01)
    })

testthat::test_that(
  desc = "Test that process_data message works",
  code = {
    testthat::expect_message(process_data(ETVapp::ETVapp_testdata[['ExtGasCal']][['Samples']][[1]], c1 = "13C", c2 = "12C", fl = 5))
    }
)
