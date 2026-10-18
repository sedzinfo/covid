##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check covid
# R CMD Rd2pdf covid
# R CMD build covid --resave-data
library(devtools)
library(roxygen2)
directory<-paste0(gsub("generate_package.R","",rstudioapi::getActiveDocumentContext()$path))
setwd(directory)
rm(list=c("covid"))
# usethis::create_package("covid")
document()
install()
library(covid)
covid()

