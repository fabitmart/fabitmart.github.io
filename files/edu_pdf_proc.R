#clear the environment
rm(list = ls())

#install.packages("tabulizer") 
library(tabulizer)

################################################## BIG LOOP (2005-2018 excluding 2008 and 2011 due to glitches) #################################################
url_list1 <- list("https://heimshelp.education.gov.au/sites/heimshelp/files/documents/resources/Documents/ACCVALtable20052007.pdf", 
                  "https://heimshelp.education.gov.au/sites/heimshelp/files/documents/resources/Documents/7_ACCVALtable2009_Finalcheck120309.pdf",
                  "https://heimshelp.education.gov.au/sites/heimshelp/files/documents/resources/Documents/ACCVALTables2010_220410.pdf")

url_list2 <- as.list(paste0("https://heimshelp.education.gov.au/sites/heimshelp/files/documents/resources/Documents/ACCVALTable", c(2012:2018), ".pdf"))

url_list <- c(url_list1, url_list2)

#loop to go through most of the years (except the buggy ones) and create dataframes with meaningful names
i = c(2005, 2009, 2010, 2012:2018)
j = 0
for (u in url_list) {
  j <- j+1
  z <- i[j]
  data <- extract_tables(u, output = "data.frame", method = "lattice", header = FALSE)
  datatot <- do.call(rbind, data)
  assign(paste0("accval",sep='_', z ), datatot)  
  }


#recover student contributions for year 2004 (and before, which I won't use)
accval_2004 <- accval_2005 %>%
  filter(V1=="2005") %>%
  mutate(V4 = 7,
         V1 = 2004) %>%
  select(V1, V2, V4, V3, V5, V6) %>%
  rename(V3 = V4, 
         V4 = V3)
#put accval_2004 in accval_2005 (for ease of data cleaning)
accval_2005 <- rbind(accval_2004, accval_2005)  
rm(accval_2004) #remove accval_2004

#recode V3 to match with same variable in other dataframes
accval_2005 <- mutate(accval_2005, V3 = 7)   #7 means: "Standard maximum student contribution for Commonwealth supported students." [for students starting that year]

#fix bug in discipline codes of years 2005-2007 (some leading zeros are missing, so I'm adding them when this is the case)
accval_2005$V2 <- ifelse( (grepl(pattern="^[1-9]",accval_2005$V2)&(nchar(accval_2005$V2, type = "chars")==5))==TRUE, paste("0", accval_2005$V2, sep = ""), accval_2005$V2)

#drop first column in accval_2009, which is extra wrt other dataframes
accval_2009 <- accval_2009[,-c(1)]
colnames(accval_2009) <- colnames(accval_2012)   #give same varnames as other datasets [order of vars is also the same]

################################################## 2008 ###########################################################################################################

#get ACCVAL tables pdf url
site <- "https://heimshelp.education.gov.au/sites/heimshelp/files/documents/resources/Documents/ACCVAL_table2008.pdf"

# get back the tables as data frames, keeping their headers. This creates a list of dataframes, one per page of the pdf (20)
df_2008a <- extract_tables(site, output = "data.frame", method = "stream", header = FALSE, pages=1)
df_2008b <- extract_tables(site, output = "data.frame", method = "stream", header = FALSE, pages= c(2:10))

df_2008a[[1]] <- df_2008a[[1]][,-5]
colnames(df_2008a[[1]]) <- colnames(df_2008b[[1]])

df_2008 <- c(df_2008a, df_2008b)
accval_2008 <- do.call(rbind, df_2008)

str(accval_2008)

#structure of this dataframe is a bit different form other years: drop 2 extra columns and recode one to make it match with other dataframes
accval_2008 <- accval_2008[,-c(3,4)]
colnames(accval_2008) <- colnames(accval_2012)   #give same varnames as other datasets [order of vars is also the same]
accval_2008 <- mutate(accval_2008, V3 = 7)   #7 means: "Standard maximum student contribution for Commonwealth supported students." [for students starting that year]

################################################## 2011 ###########################################################################################################
#2011 is messy, use the downloaded .xls file
require(XLConnect)
#set working directory
setwd("C:/Users/Fabio M/Documents/Research/AUS_edu/AU_edu_data/Bands_ACCVAL_tables")
#load spreadsheet
wb <- loadWorkbook("ACCVALTable2011.xls")
#save spreadsheet as dataframe
accval_2011 <- readWorksheet(wb, sheet = 1, startRow = 8)
#rename to be compatible with other dataframes
colnames(accval_2011)<-c("V1", "V2", "V3", "V4", "V5", "V6" )

########################################################## APPEND ALL TOGETHER ####################################################################################
#creates a list of all accaval dataframes by subsetting the list of all objects in the environment (based on condition: name starts with "accval") 
accval_list <- mget(str_subset(ls(), "^accval_2"))

#append all dataframes in the accval_list list
accval_all <- do.call(rbind, accval_list)


####################################################### CLEAN AND CHANGE VARIABLES TYPES ##########################################################################
#rename variables appropriately
varnames <- c("year", "disc_code_E336", "stud_contrib_index", "stud_contrib", "band", "discipline")
names(accval_all) <- varnames

#delete row if 
#    (i) the year column does not contain a string starting in "20" (e.g. 2005, 2006,...)
#    (ii) year does not end in ".", nor contain "ACCVAL"
#    (ii) stud_contrib and band are nonmissing (such observations are associated with empty rows, not real data)
accval_clean <-
  accval_all  %>% filter(grepl(pattern = "^20", year),
                         !grepl(pattern = "ACCVAL", year),
                         !grepl(pattern = "\\>\\.", year),
                         !is.na(stud_contrib),
                         !is.na(band)) #and ending in "." and ending in "Table" -------> \\> is end of a word

#remove commas from stud_contrib
accval_clean$stud_contrib <- gsub("[,\\$]", "", accval_clean$stud_contrib)

#put correct data types
accval_clean1 <- data.frame(lapply(accval_clean[,c(1, 4), drop = FALSE], as.numeric, stringsAsFactors=FALSE), 
                           lapply(accval_clean[,c(2,3,5)], as.factor),
                           accval_clean[,6])

str(accval_clean1)

#per each year, leave only fees associated to students starting uni in that year
accval_clean2 <-
  accval_clean1  %>% filter(
    (year>=2004 & year<=2007 & stud_contrib_index=="7")|
    (year==2008 & stud_contrib_index=="7")|
    (year==2009 & stud_contrib_index=="5")|
    (year==2010 & stud_contrib_index=="6")|
    (year==2011 & stud_contrib_index=="6")|
    (year==2012 & stud_contrib_index=="6")|
    (year==2013 & stud_contrib_index=="7")|
    (year==2014 & stud_contrib_index=="7")|
    (year==2015 & stud_contrib_index=="7")|
    (year==2016 & stud_contrib_index=="7")|
    (year==2017 & stud_contrib_index=="7")|
    (year==2018 & stud_contrib_index=="7")
    )

names(accval_clean2)[6] <- "disc"

#fix bug in discipline codes (some leading zeros are missing, so I'm adding them when this is the case)
accval_clean2$disc_code_E336 <- as.character(accval_clean2$disc_code_E336)   #make into character vector 
accval_clean2$disc_code_E336 <- ifelse( (grepl(pattern="^[1-9]",accval_clean2$disc_code_E336)&(nchar(accval_clean2$disc_code_E336, type = "chars")==5))==TRUE, paste("0", accval_clean2$disc_code_E336, sep = ""), accval_clean2$disc_code_E336)

accval_clean1 <- accval_clean2

#save dataset
setwd("C:/Users/Fabio M/Documents/Research/AUS_edu/AU_edu_data/")
save(accval_clean1, file = "accval_clean.Rda")


