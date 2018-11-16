#----------------------------------------------------------------------------------------------
#  R code to match adfg stat areas with chum salmon spatial clusters in the Bering Sea
#  Developed: 1/24/2018
#  Last Modified: 1/24/2018
#  Author: Jordan Watson (jordan.watson@noaa.gov)
#----------------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------------
#  Load packages  
library(tidyverse)
library(readxl)
library(stringi)
#----------------------------------------------------------------------------------------------

data <- read_excel("rcode/Data/psc_all_catch_and_genetics.xlsx",sheet="Genetic BSAI Salmon Bycatch")

#----------------------------------------------------------------------------------------------
#  Prepare data
#----------------------------------------------------------------------------------------------

#  Read-in AKFIN data
akfin <- read_excel("Data/AKFIN_2013_chum_samples 2017-12-20.xlsx",sheet="Salmon Genetic Analysis") %>% 
  mutate(mycluster=ADFG_STAT_AREA_CODES)

#  Read-in excel file that has the ADFG / Cluster area assignment
lkp <- read_excel("Data/ADFG and Cluster assignment.xlsx")

#  Perform a match that links adfg areas with their cluster
akfin <- akfin %>% 
  mutate(mycluster=stri_replace_all_fixed(akfin$mycluster, lkp$`ADFG area`, lkp$Cluster, vectorize_all=FALSE))
                          
#  Some adfg area groups may all be in the same cluster (e.g., 1,1,1,1), so we can simplify these to just be cluster 1.
#  In such cases, the first and last characters are the same. So if the first and last character is the same, simplify. 
#  If the first and last characters are not the same (e.g., 1,1,1,4), then leave things as they are. 
akfin$ucluster <- ifelse(substr(akfin$mycluster,1,1)==substr(akfin$mycluster,nchar(akfin$mycluster),nchar(akfin$mycluster)),
                         as.vector(sapply(akfin$mycluster, function(txt){ 
                           paste(unique(unlist(strsplit(txt, ", "))), collapse=",")
                         })),akfin$mycluster)

#  Inspect the areas and clusters (for QA/QC)
akfin %>% 
  dplyr::select(ADFG_STAT_AREA_CODES,ucluster) %>% 
  data.frame

akfin %>% 
  dplyr::select(ADFG_STAT_AREA_CODES,ucluster) %>% 
  data.frame %>% 
  filter(ucluster=="1, 1, 1, 1, 2")

#----------------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------------
#  Create Pivot Table summaries
#----------------------------------------------------------------------------------------------

#  Pivot table of areas by week.
weekly <- tally(akfin %>% group_by(ucluster,WEEK_NUMBER)) %>% 
  spread(WEEK_NUMBER,n) %>% 
  data.frame %>% 
  replace(is.na(.), 0) %>%
  mutate(sum=rowSums(.[,-1],na.rm=TRUE)) 

bind_rows(weekly,weekly %>% dplyr::select(-ucluster) %>% summarise_all(sum,na.rm=TRUE))


#  Pivot table of areas by vessel
byvessel <- akfin %>% 
  group_by(DELIVERY_VESSEL_ADFG,ADFG_STAT_AREA_CODES) %>% 
  summarise(n=n()) %>% 
  spread(DELIVERY_VESSEL_ADFG,n) %>% 
  replace(is.na(.), 0) %>%
  mutate(sum=rowSums(.[,-1],na.rm=TRUE)) 

#  Add a tally row to the bottom
byvessel <- bind_rows(byvessel,byvessel %>% dplyr::select(-ADFG_STAT_AREA_CODES) %>% summarise_all(sum,na.rm=TRUE))

# Create a data.frame of unique vessels per stat area
numvessel <- akfin %>% 
  group_by(ADFG_STAT_AREA_CODES) %>% 
  summarise(unique.vessels=length(unique(DELIVERY_VESSEL_ADFG)))
  
byvessel.w.num=byvessel %>% 
  inner_join(numvessel)

byvessel.legal=byvessel %>% 
  inner_join(numvessel) %>% 
  filter(unique.vessels>2)




