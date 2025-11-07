# Launch the ShinyApp (Do not remove this comment)
# Or use the blue button on top of this file

# To update renv:
# renv::status()
# renv::update()
# renv::snapshot()
# renv::install("janlisec/ETVapp")

# To deploy to shinyapps.io:
# rsconnect::deployApp(appDir = "C:/Users/jlisec/Documents/Rpackages/Rpackage_ETVapp/ETVapp/", appName = "test", forceUpdate = TRUE)

# To check for errors in shinyapps.io:
# rsconnect::showLogs(appName = c("test","eCerto")[1], account = "jali")

install.packages("pkgload", repos = "https://packagemanager.posit.co/cran/latest")
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
ETVapp::app() # add parameters here (if any)
