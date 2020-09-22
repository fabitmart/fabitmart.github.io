###########################################################
#DOWNLOAD COMMENCING STUDENTS DATA BETWEEN YEARS 2004-2018#
###########################################################
#clear the environment
rm(list = ls())

# install.packages("rvest")
# install.packages("xml2")
# install.packages("R6")
# install.packages("magrittr")
library(rvest)

for (y in c(2004:2008)) {
  link <- paste0("https://docs.education.gov.au/system/files/doc/other/", y, "_commencing_student_load.xls")
  download.file(link, paste0("C:/Users/Fabio M/Documents/Research/AUS_edu/AU_edu_data/AU_edu_raw/commencing_", y, ".xls"),mode='wb')
}

for (y in c(2009:2011)) {
  link <- paste0("https://docs.education.gov.au/system/files/doc/other/", y, "_commencing_student_load_0.xls")
  download.file(link, paste0("C:/Users/Fabio M/Documents/Research/AUS_edu/AU_edu_data/AU_edu_raw/commencing_", y, ".xls"),mode='wb')
}

link <- "https://docs.education.gov.au/system/files/doc/other/2012_commencing_student_load_1.xls"
download.file(link, paste0("C:/Users/Fabio M/Documents/Research/AUS_edu/AU_edu_data/AU_edu_raw/commencing_2012.xls"),mode='wb')

for (y in c(2013)) {
  link <- paste0("https://docs.education.gov.au/system/files/doc/other/", y, "commencingstudentload.xls")
  download.file(link, paste0("C:/Users/Fabio M/Documents/Research/AUS_edu/AU_edu_data/AU_edu_raw/commencing_", y, ".xls"),mode='wb')
}

for (y in c(2014:2015)) {
  link <- paste0("https://docs.education.gov.au/system/files/doc/other/", y, "_commencing_student_load.xls")
  download.file(link, paste0("C:/Users/Fabio M/Documents/Research/AUS_edu/AU_edu_data/AU_edu_raw/commencing_", y, ".xls"),mode='wb')
}

for (y in c(2016:2018)) {
  link <- paste0("https://docs.education.gov.au/system/files/doc/other/", y, "_section_3_-_commencing_student_load.xls")
  download.file(link, paste0("C:/Users/Fabio M/Documents/Research/AUS_edu/AU_edu_data/AU_edu_raw/commencing_", y, ".xls"),mode='wb')
}

