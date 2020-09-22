###################### MERGERS ##########################################################################
#clear the environment
rm(list = ls())
library(dplyr) 

#wroking directory
setwd("C:/Users/Fabio M/Documents/Research/AUS_edu/AU_edu_data")

#load datasets
load("enrol.Rda")
load("edu_codes.Rda")
load("accval_clean.Rda")

#merge enrolment data with discipline codes
#enrol_plus <- left_join(enrol, csv, by = c("discipline" = "discipline"))

#clean from broad disc before merging
csv <- filter(csv, nchar(csv$disc_code, type = "chars")==6)

#merge! NB: enrol has 1212 obs
enrol_plus <- merge(enrol, csv, all.x = TRUE)   #match codes (from csv) to enrolment data (enrol)
#check NAs (sign of issues with merger)
#filter(enrol_plus, is.na(disc_code))

# enrol_inner <- enrol %>%
#   inner_join(csv, by=c("disc_code"))

#tidy up
enrol_plus <-
  enrol_plus %>%
  mutate(discipline = NULL) %>%
  rename(discipline = discipline1) %>%
  arrange(discipline, year) %>%
  select(disc_code, everything()) %>%
  select(discipline, everything())  #keep if 6 digits, i.e. only if it's a narrow discipline (broad disc will be recoded)

#spelling changed, make uniform
enrol_plus$disc <- gsub("programs", "programmes", enrol_plus$disc)

#fix NAs manually
enrol_plus1 <- enrol_plus %>% mutate(
  disc_code = case_when(
    disc=="bankingfinanceandrelatedfields" ~ "081100",
    disc=="agricultureenvironmentalandrelatedstudies" ~ "059999",
    disc=="agriculture" ~ "050100",
    disc=="architectureandbuilding" ~ "040100",   #architectureandurbanenvironment
    #disc=="creativeartsnfd	" ~ "",
    disc=="curriculumandeducationstudies" ~ "070300",
    disc=="economicsandeconometrics" ~ "091900",
    #disc=="educationnfd" ~ "",
    disc=="employmentskillsprograms" ~ "",   #missing, drop
    #disc=="engineeringandrelatedtechnologiesnfd" ~ "",
    disc=="foodhospitalityandpersonalservices" ~ "110100",   #not sure
    disc=="generaleducationprogrammes" ~ "120100",   #programmes
    #disc=="healthnfd" ~ "",
    disc=="horticultureandviticulture" ~ "050300",
    #disc=="informationtechnologynfd" ~ "",
    disc=="librarianshipinformationmanagementandcuratorialstudies" ~ "091300",
    #disc=="managementandcommercenfd" ~ "",
    #disc=="naturalandphysicalsciencesnfd" ~ "",
    disc=="otheragricultureenvironmentalandrelatedstudies" ~ "059900",
    disc=="othercreativearts" ~ "109900",
    disc=="othereducation" ~ "079900",
    disc=="otherengineeringandrelatedtechnologies" ~ "039900",
    disc=="otherhealth" ~ "069900",
    disc=="otherinformationtechnology" ~ "029900",
    disc=="othermanagementandcommerce" ~ "089900",
    disc=="othermixedfieldprogrammes" ~ "129900",    ## othermixedfieldprograms
    disc=="othernaturalandphysicalsciences" ~ "019900",
    disc=="othersocietyandculture" ~ "099900",
    disc=="philosophyandreligiousstudies" ~ "091700",
    disc=="physicsandastronomy" ~ "010300",
    disc=="politicalscienceandpolicystudies" ~ "090100",
    disc=="socialskillsprograms" ~ "",   #missing, drop
    #disc=="societyandculturenfd" ~ "",
    TRUE ~ disc_code
  )
)

#could not find codes for these => dropped
enrol_plus1 <- enrol_plus1[!(enrol_plus1$disc=="socialskillsprograms" | 
                               enrol_plus1$disc=="employmentskillsprograms" | 
                               enrol_plus1$disc=="socialskillsprogrammes"| 
                               enrol_plus1$disc=="employmentskillsprogrammes"),]

#zero NAs now
enrol_na <- filter(enrol_plus1, is.na(disc_code)==TRUE)

enrol_plus <- enrol_plus1
rm(enrol_plus1)

#this shows that there are no faulty codes with less than 6 characters
#enrol_char <- enrol_plus %>% filter(nchar(disc_code, type = "chars")!=6)
       
#prepare merger with accval_clean1
accval_clean1 <- 
  accval_clean1 %>% 
      rename(disc_code = disc_code_E336,
             disc_accval = disc) %>% 
      mutate(stud_contrib_index = NULL,
             disc_code=as.character(disc_code))

#merge!
all_edu <- merge(enrol_plus, accval_clean1, all.x = TRUE)   #matching by disc_code & year

#don't know why these values are missing, I am putting them in manually from accval tables
all_edu <- all_edu %>%
  mutate(
    stud_contrib = case_when(
      disc_code == "010199" | year == 2012 ~ 4520,
      disc_code == "010300" | year == 2012 ~ 4520,
      disc_code == "010500" | year == 2012 ~ 4520,
      TRUE ~ stud_contrib
    )
  )

#check for NAs: only general education programmes and mixed field programmes. Not of interest==>drop
all_edu_na <- filter(all_edu, !is.na(stud_contrib)==TRUE)

#"personal services" was intermittently merged with "food and hospitality" 
#   ==> make them a single field always
# all_edu_1 <- filter(all_edu, disc=="foodandhospitality" | disc=="personalservices")
# 
# all_edu_1$bachelor<-as.numeric(all_edu_1$bachelor)
# all_edu_2 <- aggregate(x = all_edu_1$bachelor,                # Specify data column
#                 by = list(all_edu_1$disc_code, all_edu_1$year),              # Specify group indicator
#                 FUN = sum)
##easire to add a couple of units manually. Am doing this in the Stata do-file.

#remove commas in soon-to-be numerical variables
all_edu[,5:12] <- lapply(all_edu[,5:12], function(x) gsub(",","",x))
str(all_edu$disc_code)

#drop "education", weird narrow field homonimous to the broad field which lasted 3 years.
#also, it is always has zero bachelor students
all_edu <- filter(all_edu, disc!="education")

#treatment variable is =1 if the field is a national priority during that year
#   Nursing and education 2005-2009
#   Mathematics, statistics and science 2009-2012

all_edu <- all_edu %>% mutate(
  treat = case_when(
    year >= 2005 & year<= 2009 & (   grepl("^0603", disc_code) | 
                                       grepl("^0701", disc_code) |
                                       grepl("^0703", disc_code) |
                                       grepl("^0799", disc_code) ) ~ 1,
    year >= 2009 & year<= 2012 &   ( grepl("^0101", disc_code) | 
                                       grepl("^0103", disc_code) |
                                       grepl("^0101", disc_code) |
                                       grepl("^0107", disc_code) |
                                       grepl("^0109", disc_code) |
                                       grepl("^0199", disc_code) ) ~ 1,
    TRUE ~ 0
  ),
  treat_group = case_when( (      #fields that were naitonal priorities at least once
    grepl("^0603", disc_code) | 
      grepl("^0701", disc_code) |
      grepl("^0703", disc_code) |
      grepl("^0799", disc_code) |
      grepl("^0101", disc_code) | 
      grepl("^0103", disc_code) |
      grepl("^0101", disc_code) |
      grepl("^0107", disc_code) |
      grepl("^0109", disc_code) |
      grepl("^0199", disc_code) ) ~ 1,
    TRUE ~ 0
  )
)

#save dataset
setwd("C:/Users/Fabio M/Documents/Research/AUS_edu/AU_edu_data/")
save(all_edu, file = "all_edu.Rda")
write.csv(all_edu, file = "all_edu.csv")

#made csv for stata in such a way that stata does not automatically convert my disc_code into numerical
all_edu_stata <- all_edu
all_edu_stata[nrow(all_edu_stata) + 1,] <- c("NA", 0, "", "", "0", "0", "0", "0", "0", "0", "0", "0", 0, NA, "0", 0, 0 )
write.csv(all_edu_stata, file = "all_edu_stata.csv")



