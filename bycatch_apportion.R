library(tidyverse)
library(readxl)

data <- read_csv("rcode/Data/Genetic BSAI Salmon Bycatch.csv") %>% 
  rename_all(tolower) %>% 
  dplyr::select(-c(first_name,last_name,akfin_load_date)) %>% 
                  rename(malechumclip=`sum(male_chum_finclip)`,      
                         femalechumclip=`sum(female_chum_finclip)`,    
                         chumclip=`sum(total_chum_finclip)`,   
                         malechinclip=`sum(male_chinook_finclip)`,  
                         femalechinclip=`sum(female_chinook_finclip)`, 
                         chinclip=`sum(total_chinook_finclip)`,  
                         chin=`sum(number_chinook)`,         
                         chum=`sum(number_chum)`,            
                         coho=`sum(number_coho)`,           
                         pink=`sum(number_pink)`,            
                         sockeye=`sum(number_sockeye)`,     
                         unid=`sum(number_unidentified)`) %>% 
  mutate(salmon=chin+chum+coho+pink+sockeye+unid)


chindat <- data %>% 
  rename(stat=primary_adfg_stat_area_code,
         stat2=adfg_stat_area_codes) %>% 
         mutate(stat3=gsub(", ","",stat2),
                nstat=nchar(stat3)/6) %>% 
  dplyr::select(cruise,permit,season,haul_offload,week_number,processing_sector,catcher_vessel_adfg,
                stat,nstat,stat2,stat3,chin)
  
chindat %>% 
  ggplot(aes(factor(week_number),fill=processing_sector)) + 
  geom_bar(stat="identity") + 
  theme_bw()

unique(chindat$stat)
unique(chindat$stat2)
unique(chindat$processing_sector)

summary(chindat %>% filter(processing_sector=="S"))

In a given week, how many of the hauls in a stat area were from trips where hauls occurred in only that stat area?
chindat %>% 
  filter(processing_sector=="S") %>% 
  ggplot(aes(week_number,fill=factor(nstat))) + 
  geom_bar(stat="count")

multi <- chindat %>% 
  filter(processing_sector=="S" & nstat>1) %>% 
  mutate(index=1:n())
  
single <- chindat %>% 
  filter(processing_sector=="S" & nstat==1) %>% 
  dplyr::select(week_number,stats=stat,chin) %>% 
  group_by(week_number,stats) %>% 
  summarise(singlechin=sum(chin),
            singletrips=n(),
            proptripswbycatch=length(chin[chin>0])/length(chin)) %>% 
  mutate(stats=as.character(stats)) %>% 
  data.frame


multi %>% 
  right_join(data.frame(index=sort(rep(multi$index,multi$nstat)),
                        stats=unlist(lapply(multi$index,function(x)strsplit(gsub(" ","",multi$stat2[x]), "[,]"))))) %>% 
  dplyr::select(-c(processing_sector,catcher_vessel_adfg,season,cruise,permit,stat3,nstat)) %>% 
  data.frame %>% 
  mutate(stats=as.character(stats)) %>% 
  group_by(week_number,stats) %>% 
  summarise(multitrips=n(),
            multichin=sum(chin)) %>% 
  left_join(single) %>% 
  data.frame

