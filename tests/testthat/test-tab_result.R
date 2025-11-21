testthat::test_that(
  desc = "Test that tab_result works",
  code = {
    pd <- data.frame("A"=1:3,"B"=1:3,"R_m"=1:3,"col4"=1:3)
    pd2 <- data.frame("A"=1:3,"B"=1:3,"R_m"=10^(-4),"col4"=1:3)
    tab1 <- tab_result(peak_data=pd, wf="ExtCal")
    tab2 <- tab_result(peak_data=pd, wf="IDMS", c_sp_unit="g/L")
    tab3 <- tab_result(peak_data=pd, wf="IDMS", c_sp_unit="mg/L", mass_fraction2 = 10^(-3))
    tab4 <- tab_result(peak_data=pd2, wf="IDMS", c_sp_unit="mg/L", amae = 10^(-4), mass_fraction2 = 0.5)

    testthat::expect_true(is.data.frame(tab1))
    testthat::expect_equal(ncol(tab1), 7)
    testthat::expect_equal(tab1[,4], tab1[,5])
    testthat::expect_true(all(grepl("pg", names(tab1)[c(5)])))
    testthat::expect_true(all(grepl("ppb", names(tab1)[c(7)])))

    testthat::expect_true(is.data.frame(tab2))
    testthat::expect_equal(ncol(tab2), 8)
    testthat::expect_equal(tab2[,c("R_m")], tab2[,c("R_corr")])
    testthat::expect_true(all(grepl("\u00b5g", names(tab2)[c(6)])))
    testthat::expect_equal(tab2[,c(8)], tab2[,c(6)] / (tab2[,c(7)] * 10))

    testthat::expect_true(is.data.frame(tab3))
    testthat::expect_equal(ncol(tab3), 11)
    testthat::expect_equal(tab3[,c(7)], tab3[,c(6)] / 10^(-3))
    testthat::expect_true(all(grepl("ng", names(tab3)[c(6,7)])))
    testthat::expect_true(all(grepl("ppm", names(tab3)[c(10,11)])))

    testthat::expect_true(is.data.frame(tab4))
    testthat::expect_equal(ncol(tab4), 11)
    testthat::expect_equal(tab4[,c(6)], tab4[,c(5)] * 10^(3))
    testthat::expect_equal(tab4[,c(11)], tab4[,c(5)] / tab4[,c(8)] / tab4[,c(9)] * 10^(3))
    testthat::expect_true(all(grepl("pg", names(tab4)[c(6,7)])))
    testthat::expect_true(all(grepl("ppb", names(tab4)[c(10,11)])))
  }
)
