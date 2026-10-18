##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check covid
# R CMD Rd2pdf covid
# R CMD build covid --resave-data
library(devtools)
library(roxygen2)
rm(list = ls())
graphics.off()
cat("\014")
##########################################################################################
# FUNCTIONS PACKAGE
##########################################################################################
directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(directory)
# usethis::create_package("covid")
document()
if ("covid" %in% rownames(installed.packages())) remove.packages("covid")
install()
library(covid)

pkgdown::build_site(getwd())
system("R CMD Rd2pdf . --force --no-preview --output=covid.pdf")
if (.Platform$OS.type == "windows") shell.exec("covid.pdf") else system("evince covid.pdf")
system("R CMD build . --resave-data")

covid() # starts the app: the console stays busy until you close it
