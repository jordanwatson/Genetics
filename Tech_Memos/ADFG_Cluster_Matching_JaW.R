#----------------------------------------------------------------------------------------------
#  R code to match ADFG stat areas with chum salmon spatial clusters in the Bering Sea
#  This code does not modify your input excel files but creates csv output files.
#  Developed: 1/24/2018
#  Last Modified: 04/30/2019 by Jackie Whittle
#  Author: Jordan Watson (jordan.watson@noaa.gov)
#----------------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------------
#  Load packages  
library(tidyverse)
library(readxl)
library(stringi)
library(forcats)
#----------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------
#  Prepare data
#----------------------------------------------------------------------------------------------

#  Under "Session" tab, select "Set Working Directory" and choose working folder.
#  Highlight all four lines of code for the library packages, copy, and Install.

#  This script uses 2 read-in files:
#  Read-in file #1:  "Genetic BSAI Salmon Bycatch Report.xlsx" downloaded from AKFIN.  
#  If the default Excel file name and sheet name  ("Genetic BSAI Salmon Bycatch Report" for both) 
#  are not changed, this script can be run with no changes to read-in file names at all.
#  If a change is made to working directory, AKFIN report file, or sheet name, just change the 
#  names in the first read-in file code to correspond exactly to the renaming.
#  Example of default names with no changes in script necessary:
#  "Data/Genetic BSAI Salmon Bycatch.xlsx",sheet="Genetic BSAI Salmon Bycatch"
#  Example of changes in working directory, AKFIN report file names, and sheet name:
#  "R Code/2018 AKFIN Genetic BSAI Salmon Bycatch Report",sheet="2018 Genetic BSAI Salmon Bycatch" 

#  QUESTION;  DOES SHEET NAME NEED TO BE WRITTEN INTO THE CODE IF THERE IS ONLY ONE SHEET IN THE EXCEL FILE?
#  Answer: No it (should) not.

#  Read-in file #2:   "ADFG and Cluster assignment.xlsx".
#  Sheet name is "All_areas" (note:  this may change to "Cluster_ADFG_areas")
#  This file was created by Jordan and Chris.  It includes all ADFG reporting areas and a single 
#  corresponding Cluster assigned to each area.  No changes need to be made to this line of code.
  
#  To run script, select all the code  (Ctrl+A) and run the whole file (Ctrl+Enter). 
#----------------------------------------------------------------------------------------------

#  Read-in Excel AKFIN report that has ADFG stat area codes assignments
akfin <- read_excel("Data/Genetic BSAI Salmon Bycatch.xlsx",sheet="Genetic BSAI Salmon Bycatch") %>% 
  mutate(mycluster=ADFG_STAT_AREA_CODES)

#  Read-in Excel file that has the Cluster area assignments corresponding to the ADFG stat areas
lkp <- read_excel("Data/ADFG and Cluster assignment.xlsx",sheet="All_Areas")

striHelper <- function(x) stri_c(x[stri_order(x)], collapse = "")

#  Perform a match that links adfg areas with their cluster
akfin <- akfin %>% 
  mutate(mycluster=stri_replace_all_fixed(akfin$mycluster, 
                                          lkp$`ADFG area`, 
                                          lkp$Cluster, 
                                          vectorize_all=FALSE),
         scluster=vapply(stri_split_boundaries(gsub(", ","",mycluster), type = "character"), striHelper, ""))

#  Some adfg area groups may all be in the same cluster (e.g., 1,1,1,1), so we can simplify these to just be cluster 1.
#  In such cases, the first and last characters are the same. So if the first and last character is the same, simplify. 
#  If the first and last characters are not the same (e.g., 1,1,1,4), then leave things as they are. 
#  We use scluster to make our conditions because cluster alphabetizes the characters within each string to avoid
#  accidentally consolidating clusters where, for example, cluster is 1,2,1 instead of 1,1,2.
akfin$ucluster <- ifelse(substr(akfin$scluster,1,1)==substr(akfin$scluster,
                                                            nchar(akfin$scluster),
                                                            nchar(akfin$scluster)),
                         as.vector(sapply(akfin$mycluster, function(txt){ 
                           paste(unique(unlist(strsplit(txt, ", "))), collapse=",")
                         })),akfin$mycluster)

# Check to make sure that cluster areas make sense.
unique(akfin$ucluster)

#  Inspect the areas and clusters (for QA/QC)
akfin %>% 
  dplyr::select(ADFG_STAT_AREA_CODES,ucluster) %>% 
  data.frame
#----------------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------------
#  Create Pivot Table summaries
#----------------------------------------------------------------------------------------------

#  Pivot table of areas by week.
#  If you want to create a pivot table of areas by week, delete each of the '#' at the beginning
#  of the subsequent coding lines.

#weekly <- tally(akfin %>% group_by(ucluster,WEEK_NUMBER)) %>% 
#mutate(WEEK_NUMBER=paste0("w",WEEK_NUMBER)) %>% 
#  spread(WEEK_NUMBER,n) %>% 
#  data.frame %>% 
#  replace(is.na(.), 0) %>%
  #mutate(sum=rowSums(.[,-1],na.rm=TRUE),
 #        ucluster=fct_relevel(factor(ucluster),"1","2","3","4")) 

#bind_rows(weekly,
  #        weekly %>% dplyr::select(-ucluster) %>% summarise_all(sum,na.rm=TRUE)) %>% 
  #mutate(ucluster=ifelse(is.na(ucluster),"column_sum",as.character(ucluster))) %>% 
  #arrange(ucluster) %>% 
  #write_csv("Data/Weekly_Pivot_Table.csv")


#  Pivot table of areas by vessel
#byvessel <- akfin %>% 
  #group_by(CATCHER_VESSEL_ADFG,ADFG_STAT_AREA_CODES) %>% 
  #summarise(n=n()) %>% 
  #spread(CATCHER_VESSEL_ADFG,n) %>% 
  #replace(is.na(.), 0) %>%
  #mutate(sum=rowSums(.[,-1],na.rm=TRUE)) 

#  Add a tally row to the bottom
#byvessel <- bind_rows(byvessel,byvessel %>% dplyr::select(-ADFG_STAT_AREA_CODES) %>% summarise_all(sum,na.rm=TRUE))

# Create a data.frame of unique vessels per stat area
#numvessel <- akfin %>% 
  #group_by(ADFG_STAT_AREA_CODES) %>% 
  #summarise(unique.vessels=length(unique(CATCHER_VESSEL_ADFG)))

#  Join the pivot table of vessels by stat area with the tally of number of vessels so we can easily see which
#  areas have more than 2 vessels.
#byvessel %>% 
  #inner_join(numvessel) %>% 
  #left_join(akfin %>% dplyr::select(ADFG_STAT_AREA_CODES,ucluster) %>% distinct) %>% 
  #dplyr::select(ADFG_STAT_AREA_CODES,ucluster,everything()) %>% 
  #write_csv("Data/Pivot_Table_by_vessel_and_statarea.csv")


#  This is the same output as the above code chunk but it filters out all stat areas with fewer than 3 vessels
#  If you want to create a pivot table of vessel and stat area for more than 2 vessels, delete each of the '#' at the beginning
#  of the subsequent lines.
#byvessel %>% 
 # inner_join(numvessel) %>% 
  #left_join(akfin %>% dplyr::select(ADFG_STAT_AREA_CODES,ucluster) %>% distinct) %>% 
  #dplyr::select(ADFG_STAT_AREA_CODES,ucluster,everything()) %>% 
  #filter(unique.vessels>2) %>% 
  #write_csv("Data/Pivot_Table_by_vessel_and_statarea_MoreThan2Vessels.csv")


