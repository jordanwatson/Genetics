#----------------------------------------------------------------------------------------------
#  R code to match ADFG stat areas with chum salmon spatial clusters in the Bering Sea
#  This code does not modify your input excel files but creates csv output files.
#  Developed: 1/24/2018
#  Last Modified: 05/02/2019 by Jackie Whittle (jackie.whittle@noaa.gov)
#  Author: Jordan Watson (jordan.watson@noaa.gov)

#----------------------------------------------------------------------------------------------

# This code generates one output file:  "Pivot_Table_by_vessel_and_statarea.csv"
#  which assigns each sample either:
#  1) a single cluster corresponding to ADFG reporting areas, if all reporting areas 
#  are from the same cluster.
#  OR 
#  2) multiple clusters, if reporting areas are from different clusters.
#  For our cluster analyses, we are interested in only those samples that fall within one cluster.
#----------------------------------------------------------------------------------------------
#  This file is an abbreviated version of Jordan Watson's original code.  If
#  "Pivot_Table_by_vessel_and_statarea_MoreThan2Vessels.csv" and/or
#  "Weekly_Pivot_Table.csv" are needed, use original code created by Jordan Watson:
#  "ADFG_Cluster_Matching_2017.R"
#----------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------
#  Set working directory
#----------------------------------------------------------------------------------------------
#  Under "Session" tab, select "Set Working Directory" and choose working folder.
#  For example, 2018 working directory can be found on Genetics shared drive:
#  Bycatch > Chum > 2018 > R code > Data
#----------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------
#  Load packages  
#----------------------------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(stringi)
library(forcats)
#----------------------------------------------------------------------------------------------
#  Highlight all four lines of code for the library packages, copy, and Install.
#----------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------
#  Prepare data
#----------------------------------------------------------------------------------------------
#  This script uses 2 read-in files:

#  Read-in file #1:  "Genetic BSAI Salmon Bycatch Report.xlsx" downloaded from AKFIN.  
#  Sheet name:  "Genetic BSAI Salmon Bycatch Report"
#  These file and sheet names are default names from AKFIN download.
#  If a change is made to the working directory, report file, or sheet name, just change
#  the names in the read-in file code to correspond exactly to the renaming.
#  FYI, sheet name does not need to be included in code if there is only one sheet in Excel file.

#  Read-in file #2:   "Cluster_ADFG_areas.xlsx"
#  Sheet name:   "Cluster_ADFG_areas"
#  This file was created by Jordan Watson and Chris Kondzela.  It includes all ADFG reporting areas
#   and a single corresponding cluster assigned to each area.  
  
#----------------------------------------------------------------------------------------------
#  To run script, select all the code  (Ctrl+A) and run the whole file (Ctrl+Enter). 
#----------------------------------------------------------------------------------------------

#  Read-in Excel AKFIN report that has ADFG stat area codes assignments

akfin <- read_excel("Data/Genetic BSAI Salmon Bycatch.xlsx",sheet="Genetic BSAI Salmon Bycatch") %>% 
  mutate(mycluster=ADFG_STAT_AREA_CODES)

#  Read-in Excel file that has the Cluster area assignments corresponding to the ADFG stat areas

lkp <- read_excel("Data/Cluster_ADFG_areas.xlsx",sheet="Cluster_ADFG_areas")

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
#  Create Pivot Table summary
#----------------------------------------------------------------------------------------------

#  Pivot table of areas by vessel
byvessel <- akfin %>% 
  group_by(CATCHER_VESSEL_ADFG,ADFG_STAT_AREA_CODES) %>% 
  summarise(n=n()) %>% 
  spread(CATCHER_VESSEL_ADFG,n) %>% 
  replace(is.na(.), 0) %>%
  mutate(sum=rowSums(.[,-1],na.rm=TRUE)) 

#  Add a tally row to the bottom
byvessel <- bind_rows(byvessel,byvessel %>% dplyr::select(-ADFG_STAT_AREA_CODES) %>% summarise_all(sum,na.rm=TRUE))

# Create a data.frame of unique vessels per stat area
numvessel <- akfin %>% 
  group_by(ADFG_STAT_AREA_CODES) %>% 
  summarise(unique.vessels=length(unique(CATCHER_VESSEL_ADFG)))

#  Join the pivot table of vessels by stat area with the tally of number of vessels so we can easily see which
#  areas have more than 2 vessels.
byvessel %>% 
  inner_join(numvessel) %>% 
  left_join(akfin %>% dplyr::select(ADFG_STAT_AREA_CODES,ucluster) %>% distinct) %>% 
  dplyr::select(ADFG_STAT_AREA_CODES,ucluster,everything()) %>% 
  write_csv("Data/Pivot_Table_by_vessel_and_statarea.csv")





