testthat::test_that(
  desc = "Test that tab_LOX works",
  code = {
    tab1 <- tab_LOX(x = runif(5), wf = "IDMS")
    tab2 <- tab_LOX(x = runif(10), mass_fraction2 = 0.5, sample_mass = 3, ExtCal_unit = "\u00b5g")

    testthat::expect_true(is.data.frame(tab1))
    testthat::expect_equal(ncol(tab1), 5)
    testthat::expect_true(all(grepl("pg", names(tab1)[c(1,2)])))
    testthat::expect_true(all(grepl("ppb", names(tab1)[c(4,5)])))
    testthat::expect_equal(tab1[, c(2)], tab1[, c(1)] / 3 * 10)
    testthat::expect_equal(tab1[, c(5)], tab1[, c(2)] / tab1[, c(3)])

    testthat::expect_true(is.data.frame(tab2))
    testthat::expect_equal(ncol(tab2), 8)
    testthat::expect_true(all(grepl("\u00b5g", names(tab2)[c(1,2,5,6)])))
    testthat::expect_true(all(grepl("g/100 g", names(tab2)[c(7,8)])))
    testthat::expect_equal(tab2[, c(5)], tab2[, c(1)] / tab2[, c(3)])
    testthat::expect_equal(tab2[, c(8)], tab2[, c(2)] / (tab2[, c(4)] * 10))
  }
)

testthat::test_that(
  desc = "Test that tab_LOX messages works",
  code = {
    testthat::expect_warning(tab_LOX(x = runif(2)))
    testthat::expect_message(tab_LOX(x = runif(5), wf = "IDMS"))
  }
)
