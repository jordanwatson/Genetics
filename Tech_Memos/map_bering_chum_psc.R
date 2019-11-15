library(DBI)
library(odbc)
library(tidyverse)
library(broom)
library(rgdal)
library(maps)
library(mapdata)
library(PBSmapping)
library(rmapshaper)

#  Read all bycatch data from akfin database. To just add a year, you could use the previous year's file
#  Genetics_bsai_salmon_bycatch.RDS and run the sql query for just your new year. 
#con <- dbConnect(odbc::odbc(), "akfin", UID="jwatson", PWD= rstudioapi::askForPassword("Enter AKFIN Password"))
#my_tbl <- dbSendQuery(con,"SELECT * FROM AFSC.GENETIC_BSAI_SALMON_BYCATCH;")
#data <- dbFetch(my_tbl)

#saveRDS(data,file="Genetics_bsai_salmon_bycatch.RDS")


CrustaceanOrange2='#D65F00'
WavesTeal2='#008998'
SeagrassGreen2='#4C9C2E'
UrchinPurple2='#625BC4'
UrchinPurple4='#9A9A9A'

mapbackground <- UrchinPurple4
clusterpalette <- c(CrustaceanOrange2,WavesTeal2,SeagrassGreen2,UrchinPurple2)

#---------------------------------------------------------------------------------------
#  Make Alaska Basemap
#---------------------------------------------------------------------------------------
akmap <- readOGR(dsn="rcode/Data",layer="AKbasemap")
#  Convert the object to a ggmap object
ak <- tidy(akmap)

xmin <- -179
xmax <- -161
ymin <- 53.5
ymax <- 61.5

stat <- readOGR(dsn="rcode/Data",layer="adfg_stat_areas_simple") %>% ms_simplify()
mylabs <- data.frame(id=(as.character(rownames(stat@data))),stat_area=stat@data$STAT_AREA)

nmfs <- readOGR(dsn="rcode/Data",layer="simplenmfs") %>% tidy()
#nmfslabs <- data.frame(id=(as.character(rownames(stat@data))),stat_area=stat@data$STAT_AREA)


# extract centroids of the polygons and combine with stat area labels
centroids.df <- as.data.frame(coordinates(stat)) %>% 
  mutate(id=as.character(rownames(stat@data))) %>% 
  rename(long=V1,lat=V2) %>% 
  inner_join(mylabs)
#----------------------------------------------------------------------------------------
#  End Alaska Basemap
#---------------------------------------------------------------------------------------


# Load R bycatch dataset through 11/14/2019
data <- readRDS("rcode/Data/Genetics_bsai_salmon_bycatch_through_11142019.RDS") %>% 
  rename_all(tolower) %>% 
  rename(stat_area=primary_adfg_stat_area_code) %>% 
  filter(season=="B")

#  Define your year of interest! Yo.
this.year <- 2017

#  Identify all stat areas from 2013 to the year of interest in which at least three vessels caught chum
mystatarea <- data %>% 
  filter(year>=2013 & year<=this.year) %>% 
  group_by(stat_area) %>% 
  summarise(vessels=length(unique(catcher_vessel_adfg))) %>% 
  filter(vessels>2)


#  link psc totals to the centroid of their stat area for plotting points
mychum <- data %>% 
  filter(year==this.year) %>% 
  group_by(stat_area) %>% 
  summarise(chum=sum(number_chum)) %>% 
  filter(chum>0) %>% 
  inner_join(centroids.df)

#  stat is a map of the stat areas but the data frame of these stat areas created by tidy(stat)
#  only has a rownumber for each stat area and not the actual stat area names. So we join mylabs
#  which will combine the rownumbers with stat area names so that each polygon now has the stat area id.
#  Now we also join with our data. Since it's a left_join, we have all stat areas but NAs for those stat
#  areas in which we did not have at least three vessels. We use this later for coloring different clusters
mystat <- tidy(stat) %>% 
  inner_join(mylabs) %>% 
  left_join(mystatarea) 

#  Define the cluster boundaries based on some coarse lat/lon criteria. Easy to change.
mycluster <- mystat %>% 
  left_join(mystatarea %>% 
  inner_join(centroids.df) %>% 
  mutate(cluster=ifelse(long>(-167.00000),1,
                        ifelse(long<(-167) & long>(-169),2,
                               ifelse(long<(-169) & lat<58,3,4)))) %>% 
  dplyr::select(-c(long,lat))) %>% 
  filter(!is.na(vessels))
  

#  This creates a map where clusters are colored.
png(paste0("Figures/Map_bycatch_clusters_",this.year,".png"),width=6.5,height=5,units="in",res=1200)
ggplot() + 
  geom_polygon(data=mystat,aes(x=long,y=lat,group=factor(group)),fill=NA,color="grey50",size=0.25) + 
  geom_polygon(data=mycluster,aes(x=long,y=lat,group=factor(group),fill=factor(cluster)),color="black") + 
  geom_polygon(data=nmfs,aes(x=long,y=lat,group=factor(group)),fill=NA,color="black",size=0.7) + 
  geom_polygon(data=ak,aes(x=long,y=lat,group=factor(group)),fill=mapbackground) + 
  geom_point(data=mychum,aes(x=long,y=lat,size=chum),alpha=0.5) + 
  scale_size(name="",range=c(2,15)) +
  #xlim(xmin,xmax) + ylim(ymin,ymax)
  coord_map("albers",lat0=53.5,lat1=62,xlim=c(xmin,xmax),ylim=c(ymin,ymax)) + 
  guides(fill=FALSE,size=FALSE) +
  scale_fill_manual(values=clusterpalette) +
  theme_bw() + 
  theme(axis.title=element_blank())
dev.off()


#  This creates a map where all clusters are grey.
ggplot() + 
  geom_polygon(data=mystat,aes(x=long,y=lat,group=factor(group)),fill=NA,color="black") + 
  geom_polygon(data=mystat %>% filter(!is.na(vessels)),aes(x=long,y=lat,group=factor(group)),fill="grey",color="black") + 
  geom_polygon(data=ak,aes(x=long,y=lat,group=factor(group)),fill="grey10") + 
  geom_point(data=mychum,aes(x=long,y=lat,size=chum),alpha=0.5) + 
  scale_size(name="",range=c(2,15)) +
  coord_map("albers",lat0=53,lat1=62,xlim=c(xmin,xmax),ylim=c(ymin,ymax)) + 
  theme_bw()
