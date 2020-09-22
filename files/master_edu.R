######### MASTER FILE #########
#clear the environment
rm(list = ls())

# scrape the enrolment data from govt edu website
source("scrape_enrolment.R")

#fees for disciplines btw 2005-2018
source("edu_pdf_proc.R")

#clean enrolment data
source("cleaning_enrol2.R")

#merge into single dataframe first with csv and the with edu_pdf_proc.R
source("edu_mergers.R")
