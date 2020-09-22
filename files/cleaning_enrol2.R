#clear the environment
rm(list = ls())

#EXTRACT DOMESTIC STUDENTS DATA BY TOPIC#
#XLConnect requires:
#   - Java 64 bit if you use R 64 bit -- the standard is Java 32 bit
#   - rtools (downloadable online)
#install.packages("XLConnect")
require(XLConnect)
library(dplyr)

#Loading an Excel workbook. Both .xls and .xlsx file formats can be used.
getwd()
setwd("C:/Users/Fabio M/Documents/Research/AUS_edu/AU_edu_data/AU_edu_raw")

#list of dataframe names 
#DF_names <- paste("DF", 2004:2008, sep = "_")

#start with first file, relative to 2004
wb <- loadWorkbook("commencing_2004.xls")
DF_2004 <- readWorksheet(wb, sheet = "5", startRow = 4)

DF_2004 <- DF_2004 %>% 
  mutate( year = 2004)   #create year var

var.names<-tolower(colnames(DF_2004))   #all variable names to lower case...
colnames(DF_2004)<-var.names           # in 2 steps

#load and clean 2005-2008
for (y in c(2005:2008)) {
  file <- loadWorkbook(paste0("commencing_", y, ".xls"))
  data <- readWorksheet(file, sheet = "5", startRow = 3)
  data <- data %>%
    mutate_(
      year = 'y' 
    )
  data <- data[,colSums(is.na(data))<nrow(data)]   #drop empty columns
  
  var.names<-tolower(colnames(data))   #all variable names to lower case...
  colnames(data)<-var.names           #... in 2 steps.  it takes the column names (e.g. variable names), puts them into a vector called var.names, then you assign that vector to the column names of the dataset "data".
  
  assign(paste0("DF_", y), data.frame(data))   #rename dataset dynamically
}

#load and clean 2009-2018
for (y in c(2009:2018)) {
  file <- loadWorkbook(paste0("commencing_", y, ".xls"))
  data <- readWorksheet(file, sheet = "5", startRow = 4)
  data <- data %>%
    mutate_(
      year = 'y' 
    )
  data <- data[,colSums(is.na(data))<nrow(data)]   #drop empty columns
  
  var.names<-tolower(colnames(data))   #all variable names to lower case...
  colnames(data)<-var.names           #... in 2 steps.  it takes the column names (e.g. variable names), puts them into a vector called var.names, then you assign that vector to the column names of the dataset "data".
  
  assign(paste0("DF_", y), data.frame(data))   #rename dataset dynamically
}

######################################################

#ls(DF_2004)   #list variables from this dataframe
#ls(DF_2005)

#append all year
colnames(DF_2004) == colnames(DF_2005)   #check if varnames are identical
colnames(DF_2004)[colnames(DF_2004) == "total.eftsu"] <- "total.eftsl"   #rename non-matching varname

dfappend1 <- rbind(DF_2004, DF_2005)   #append first 2 dataframes
dfappend2 <- rbind(DF_2006, DF_2007, DF_2008, DF_2009)   #append other dataframes
dfappend3 <- rbind(DF_2010, DF_2011, DF_2012, DF_2013, DF_2014, DF_2015, DF_2016, DF_2017, DF_2018)

##################### NOTE ON CROSS INSTITUTION PROGRAMMES ##########################################################################
# - 2004-2005: there is one "cross institution programme" variable, which does separate between undegrad and postgrad students;
# - 2006-2009: the variables "undergraduate cross institution programme" and "postgraduate cross institution programme" are intoduced;
# - 2010-2018: there are no "cross institution programme" variable;
######################################################################################################################################

#give to dfappend2 same variables as dfappend1, by summing undergrad and postgrad cross.institution programmes
dfappend2 <- 
  dfappend2 %>% mutate(
    cross.institution.programmes = postgraduate.cross.institution + undergraduate.cross.institution)
dfappend1 <- 
  dfappend1 %>% mutate(
    postgraduate.cross.institution = NA,
    undergraduate.cross.institution = NA) 

#merge some variables of 
#append into single dataframe
dfappend12 <- rbind(dfappend1, dfappend2)   #append first 2 dataframes

#ls() shows that I need to unite some variables in dfappend12 before dfappend3 can be appended to it
ls(dfappend12)
ls(dfappend3)

#modify append12 in separate temp dataframe (dft) before overwiting append12
dft<-dfappend12[!(is.na(dfappend12$bachelor)),]

str(dft)   #check variable types of all variables

#define variable removing commas from string
nocom <- function(x) { gsub(",", "", x, fixed=TRUE) }
#use new fnc on a subset of the dataframe, while attaching the complement of that dataset ---> i.e. split the dataset, modify one part, put it back together with data.frame()
dft1 <- data.frame(dft[,c(1,16), drop = FALSE], lapply(dft[,c(2:15, 17, 18)], nocom) )    #NB: option <drop=FALSE> prevents loss of varname in the 1st column. Issue emerging from mix of string a numeric variables

#make some variables numeric
dft1 <- data.frame(dft1[,1, drop = FALSE], lapply(dft1[,2:18], as.numeric, stringsAsFactors=FALSE))

#check whether destring ahas been successful
str(dft1)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!----ONLY MISSING OBS ARE CROSS-INSTITUTIONAL VALUES PRE-2006----!!!!
#!!!!!!!!THEY ARE NOT USED IN THER ANALYSIS AND ARE HENCE DROPPED LATER!!
#this is whyI can make NAs into zeros for my ease without causing issues!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
dft1 <- dft1 %>% replace(is.na(.), 0)
#______________!

#modify append12 (dft1) so that ti can be appended to the other enrolment dataframes
dft1 <- 
  dft1 %>%
    mutate(
      doctorate = doctorate.by.coursework + doctorate.by.research,
      master.s = master.s.by.coursework + master.s.by.research,
      sub.total.postgraduate = NULL,
      sub.total.undergraduate = NULL,
      other.undergraduate = other.undergraduate + undergraduate.cross.institution + associate.degree,   #not sure if this is true, but reasonable assumption
      other.postgraduate = other.postgraduate + postgraduate.cross.institution,   #ibid.
      associate.degree = NULL,
      doctorate.by.coursework = NULL,
      doctorate.by.research = NULL,
      undergraduate.cross.institution = NULL,
      postgraduate.cross.institution = NULL,
      master.s.by.coursework = NULL,
      master.s.by.research = NULL,
      cross.institution.programmes = NULL
    )
#note1: Cross-institutional students enrol in a course(s) through another institution which, on successful completion, can be credited to their program at their home institution.
#note2: Courses at Diploma, Advanced Diploma and Associate degree level take between one and three years to complete, and are generally considered to be equivalent to one to two years of study at degree level. Diploma and Advanced Diploma are titles given more practical courses, while Associate degree is given to more academic courses. Entry into Diploma and Advance Diploma courses requires the completion of Year 12 education. 

ls(dft1)
ls(dfappend3)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~CONTINUE

#remove comma from a character variable while you make it numeric  -----> now do to all variables!!! ONCE THEY ARE ALL NUMERIC, YOU CAN PERFORM MUTATE() OPERATIONS AND THEN APPEND 
#ALL DATASETS TOGETHER. 
#AFTER THAT, CREATE FACTOR VARIABLE TO GROUP NARROW DISCIPLINES BY BROADER DISCIPLINES (CHECK IF IMPT, OTHERWISE JUST FOLLOW THE 3 BANDS IN GRATTAN XLS)
#AFTER THAT, MERGE WITH GRATTAN XLS
#AFTER THAT, ORDER BY COURSE-YEAR
#PS dft IS A TEMP FILE CREATED NOT TO MESS UP dfappend12

#overwrite dftt1 on append12 
dfappend12 <- dft1

enrol <- rbind(dfappend12, dfappend3)


#remove empty rows and order by discipline-year
enrol_temp <- 
  enrol %>% rename(discipline1 = narrow.discipline.group) %>%
            filter(!is.na(bachelor),
              discipline1 != "Narrow Discipline Group",
              !grepl(pattern = "[2000-3000]", discipline1),
              !grepl(pattern = "Total", discipline1),
              !grepl(pattern = "TOTAL", discipline1),
              !grepl(pattern = "Sub-total", discipline1),
              ) %>%
            arrange(discipline1, year)

#overwrite enrol_temp on enrol
enrol <- enrol_temp 

#make all words lowercase
enrol$discipline1 <- tolower(enrol$discipline1) 

#remove all spaces from discipline to fix matching issues when merging
enrol$discipline1 <-gsub(" nfd","",enrol$discipline1)   #'ndf' was introduce to separate homonimous narrow and broad fields
enrol$disc <-gsub("[[:space:]]","",enrol$discipline1)
enrol$disc <-gsub(",","",enrol$disc)


#save dataset
setwd("C:/Users/Fabio M/Documents/Research/AUS_edu/AU_edu_data/")
save(enrol, file = "enrol.Rda")
save(enrol, file = "enrol.csv")

    
















