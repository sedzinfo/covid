##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check covid
# R CMD Rd2pdf covid
# R CMD build covid --resave-data
library(devtools)
library(roxygen2)
setwd("/mnt/WDRED_REMOTE/repositories/covid/")
# usethis::create_package("covid")
document()
install()
