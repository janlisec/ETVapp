testthat::test_that(
  desc = "Test that import_data works",
  code = {
    pth <- system.file(package = "ETVapp", "extdata", 'fmt')
    imp <- import_data(dir(pth, full.names = TRUE))

    # import is a list
    testthat::expect_true(is.list(imp))

    # 4 example files are provided
    testthat::expect_equal(length(imp), 4)

    # all expected file names exist
    testthat::expect_true(all(names(imp) %in% c("Agilent_PTFE_0.23_mg_P19.csv","iCAP_BCR-646_0.8_mg_P20.csv","Multicollector_S-test7_006SMP.exp","OES_Cl_Br_I_1000ng_P14.csv")))

    # first column is always named 'Time'
    testthat::expect_true(all(sapply(imp, function(x) { colnames(x)[1] })=="Time"))

    # NULL and message are returned for non-existent file
    testthat::expect_message(
      testthat::expect_null(import_data("not_existent.file"))
    )

  }
)
