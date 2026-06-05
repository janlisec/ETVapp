testthat::test_that(
  desc = "Test that get_isoratio works",
  code = {
    iso_peaks <- lapply(ETVapp::ETVapp_testdata[["IDMS"]], function(x) {
        get_isoratio(
          x, iso1_col = "113Cd", iso2_col = "111Cd", PPmethod = "Peak (manual)",
          peak_start = 70, peak_end = 132
        )
    })

    testthat::expect_true(is.list(iso_peaks))
    testthat::expect_equal(length(iso_peaks), 3)
    testthat::expect_true(all(names(iso_peaks) %in% c("Blanks","Massbias","Samples")))

    testthat::expect_equal(colnames(iso_peaks[[1]]), c("Spike isotope","Sample isotope","R_m"))
    testthat::expect_true(is.numeric(iso_peaks[[1]][,"R_m"]))
    testthat::expect_equal(iso_peaks[[2]][3,"R_m"], c(1.004), tolerance = 0.01)
  }
)

testthat::test_that(
  desc = "Test that get_isoratio warning message works",
  code = {
    testthat::expect_warning(get_isoratio(
      ETVapp::ETVapp_testdata[["oIDMS"]][[2]][[2]], iso1_col = "117Sn", iso2_col = "122Sn", PPmethod = "Peak (height)", minpeakheight = 100000
    ))
    }
)
