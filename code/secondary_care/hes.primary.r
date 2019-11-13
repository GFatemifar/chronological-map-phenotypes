#Ghazaleh Fatemifar
#clear all
ls()
remove(list = ls())

#load libraries 
library(data.table)   
library(tidyr)
library(plyr)
library(dplyr)
library(readr)
library(lubridate)

#############
##LOAD DATA##
############# 

HESprimary <- fread(file = 'S:/FPHS_IHI_DataLab_UKBiobank/application-992/HES/April2019/hesinApril2019.tsv', sep = '\t',na.strings = "", header = TRUE) 
filename <- "S:\\FPHS_IHI_DataLab_UKBiobank\\ghazaleh\\308\\phenotypes\\chronological-map-phenotypes-master\\chronological-map-phenotypes-master\\secondary_care" #map data for 308 phenotypes
csv_files <- lapply(dir(path = filename, pattern = ".csv", full.names = TRUE), fread)
names(csv_files) <- gsub(".csv", "", basename(dir(path = filename, pattern = ".csv")))

#######################################################################
###replace to missing if epistart data is missing for diagnosis codes##
#######################################################################

HESprimary$diag_icd10[is.na(HESprimary$epistart)] <- NA

              #################################
              ##CODE CASES WITH EPISTART DATE##
              #################################
##For purpose of counts just code cases as 1 and controls as 0##


for (x in names(csv_files)){
  if (grepl("OPCS", x, ignore.case = T)) next
  csv_files[[x]][,ICD10code := gsub("\\.", "", ICD10code)]
  grep_statement <- paste0(csv_files[[x]]$ICD10code, collapse = "|")
  
  value <- if_else(is.na(HESprimary$diag_icd10), NA_real_,
                   if_else(grepl(grep_statement, HESprimary$diag_icd10), 1, 0))
  
  set(HESprimary, j = paste0(x, "_HESp"), value = value)
}


#########################
#   SUMMARISE COHORT ####
##Primary diagnosis HES##
#########################
#creates 1 row per patient 

PrimarydiagnosisHES <- HESprimary %>% 
  group_by(eid) %>% 
  summarise_at(vars(starts_with("ICD_")), max, na.rm=T)
#report any warnings
warnings()

hes.cohort.primary <- PrimarydiagnosisHES

#write table
fwrite(hes.cohort.primary, file="S:\\FPHS_IHI_DataLab_UKBiobank\\ghazaleh\\308\\output\\counts\\hes.primary.cohort.csv")
