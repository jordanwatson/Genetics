#-------------------------------------------------------------------------------------------------------
#  This file accompanies the "Chum_tech_memo_bycatch_from_2017_no_nmfs_areas.R" file
#  One end result is the stock composition table for the aged chum bycatch. But there are additional outputs.
#  1) Output a csv summary of age composition for each of the spatial (clusters) and temporal (early/late) stratum (independent of genetics).
#  2) Output figures of age composition for each of the spatial (clusters) and temporal (early/late) stratum.
#  3) Output a .txt file of the stock compositions by age for each spatial and temporal stratum, including:
#     a) GSI percentages from Bayes output
#     b) P=0 values which are calculated from Bayes output
#     c) Shrink factor from Bayes output
#
#  Author: Jordan Watson (jordan.watson@noaa.gov)
#  Date: Initially produced for 2019 report on 2017 chum bycatch 2/5/2019
#-------------------------------------------------------------------------------------------------------



#-------------------------------------------------------------------------------------------------------
#  Load packages and define variables
library(tidyverse)
library(readxl)
library(viridis)
library(stringr)

#  Define colors
strongblue <- rgb(69,117,180,max=255)

#  During which week does the season start for spatial clusters?
earlycluster <- 23
latecluster <- 33

#  Define stock reporting groups
#  Jordan, change to Upper/Middle Yukon and Eastern GOA/PNW
Region=c("SE Asia","NE Asia","Western AK","Up/Mid Yukon","SW Alaska","E GOA/PNW")


#  Read age data for genotyped samples
data_all <- read_excel("rcode/Data/2017 Chum Bycatch Samples & Scores.xlsx",sheet="n=3491 All Scores & Binary") %>% 
  rename_all(tolower) %>% 
  rename(age=`total age`,
         brood=`brood year`)

#  Only BSAI fish were aged so we filter out BSAI
#  Fish with missing ages are reported in several ways. But only those fish for which there is a value in "Brood Year" have good ages.
#  So, if Brood Year is missing, I change the age value to be "NA" so that I have a consistent filtering value.
data <- data_all %>% 
  filter(fmp_area=="BSAI") %>% 
  dplyr::select(age,
                cluster,
                extra_clusters_added,
                week_number,
                brood
                ) %>% 
  mutate(period=ifelse(week_number>=earlycluster & week_number<latecluster,"early",
                       ifelse(week_number>=latecluster,"late",NA)),
         age=ifelse(is.na(brood),NA,age))

#-------------------------------------------------------------------------------------------------------
#  Create a function that summarizes the counts and proportions of each age class in each stratum.
#  There are two versions of this function. One is formatted for output to a csv. The other is formatted 
#  for data manipulation and plotting.
#-------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------
#  Version 1 - for output to csv
#-------------------------------------------------------------------------------------------------------

agefun <- function(description,mydata){
  data.frame(table(mydata$age,exclude = NULL)) %>% 
    rename_all(tolower) %>% 
    rename(age=var1) %>% 
    mutate(proportion=round(freq/sum(freq),4),
           age=ifelse(is.na(age),"No age",as.character(age))) %>% 
    right_join(data.frame(age=as.character(c(2:7,"No age")))) %>% 
    replace(is.na(.),0) %>% 
    mutate(stratum=c(paste0(description," (N=",prettyNum(sum(freq),big.mark = ","),")"),rep("",6)),
           proportion=ifelse(proportion==0,
                             format(as.character(proportion),digits=1),
                             formatC(proportion,digits=4,format="f"))) %>% 
    dplyr::select(stratum,age,freq,proportion)}

agesummary <- bind_rows(
agefun("All",data),
agefun("Cluster 1",data %>% filter(cluster=="1")),
agefun("Cluster 2",data %>% filter(cluster=="2")),
agefun("Cluster 3",data %>% filter(cluster=="3")),
agefun("Cluster 4",data %>% filter(cluster=="4")),
agefun("Cluster 1 Early",data %>% filter(cluster=="1" & period=="early")),
agefun("Cluster 1 Late",data %>% filter(cluster=="1" & period=="late")),
agefun("Cluster 2 Early",data %>% filter(cluster=="2" & period=="early")),
agefun("Clusters 1-2 Early",data %>% filter(cluster%in%unique(data$cluster[grep(paste(c("1","2"),collapse="|"),data$cluster)]) & period=="early")),
agefun("Cluster 1-2 Late",data %>% filter(cluster%in%unique(data$cluster[grep(paste(c("1","2"),collapse="|"),data$cluster)]) & period=="late")),
agefun("Cluster 3-4 Early",data %>% filter(cluster%in%unique(data$cluster[grep(paste(c("3","4"),collapse="|"),data$cluster)]) & period=="early")),
agefun("Cluster 3-4 Late",data %>% filter(cluster%in%unique(data$cluster[grep(paste(c("3","4"),collapse="|"),data$cluster)]) & period=="late"))
)

write.csv(agesummary,"Age_Summaries.csv",row.names=FALSE)
#-------------------------------------------------------------------------------------------------------
#  End version 1
#-------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------
# Version 2 for manipulation and plotting
#-------------------------------------------------------------------------------------------------------

agefunplot <- function(description,mydata)
{data.frame(table(mydata$age,exclude = NULL)) %>% 
    rename_all(tolower) %>% 
    rename(age=var1) %>% 
    mutate(proportion=round(freq/sum(freq),4),
           age=ifelse(is.na(age),"No age",as.character(age))) %>% 
    right_join(data.frame(age=as.character(c(2:7,"No age")))) %>% 
    replace(is.na(.),0) %>% 
    mutate(stratum=rep(paste0(description," (N=",prettyNum(sum(freq),big.mark = ","),")"),7),
           labels=rep(paste0(description,"\n(N=",prettyNum(sum(freq),big.mark = ","),")"),7),
           labels2=rep(description,7),
           proportion=ifelse(proportion==0,
                             format(as.character(proportion),digits=1),
                             formatC(proportion,digits=4,format="f"))) %>% 
    dplyr::select(stratum,age,freq,proportion,labels,labels2)}

agesummaryplot <- bind_rows(
  agefunplot("All",data),
  agefunplot("Cluster 1",data %>% filter(cluster=="1")),
  agefunplot("Cluster 2",data %>% filter(cluster=="2")),
  agefunplot("Cluster 3",data %>% filter(cluster=="3")),
  agefunplot("Cluster 4",data %>% filter(cluster=="4")),
  agefunplot("Cluster 1 Early",data %>% filter(cluster=="1" & period=="early")),
  agefunplot("Cluster 1 Late",data %>% filter(cluster=="1" & period=="late")),
  agefunplot("Cluster 2 Early",data %>% filter(cluster=="2" & period=="early")),
  agefunplot("Clusters 1-2 Early",data %>% filter(cluster%in%unique(data$cluster[grep(paste(c("1","2"),collapse="|"),data$cluster)]) & period=="early")),
  agefunplot("Cluster 1-2 Late",data %>% filter(cluster%in%unique(data$cluster[grep(paste(c("1","2"),collapse="|"),data$cluster)]) & period=="late")),
  agefunplot("Cluster 3-4 Early",data %>% filter(cluster%in%unique(data$cluster[grep(paste(c("3","4"),collapse="|"),data$cluster)]) & period=="early")),
  agefunplot("Cluster 3-4 Late",data %>% filter(cluster%in%unique(data$cluster[grep(paste(c("3","4"),collapse="|"),data$cluster)]) & period=="late"))
)


pdf("Age_Summary_dodged_bars.pdf",width=10,height=5.75)
agesummaryplot %>% 
  ggplot(aes(age,as.numeric(proportion))) + 
  geom_bar(aes(fill=stratum),stat="identity",position="dodge") + 
  scale_fill_viridis_d(name="Stratum") + 
  theme_bw() + 
  ylab("Proportion of genotyped samples") + 
  xlab("Scale age") + 
  theme(legend.position="top")
dev.off()

pdf("Age_Summary_stacked_bars.pdf",width=10,height=5.75)
agesummaryplot %>% 
  ggplot(aes(labels,y=as.numeric(proportion),fill=(age))) + 
  geom_bar(stat="identity") + 
  scale_fill_viridis_d(name="Age") + 
  theme_bw() + 
  ylab("Proportion of genotyped samples") + 
  xlab("Stratum") + 
  theme(axis.text=element_text(size=8),
        legend.position="top") + 
  scale_x_discrete(labels=function(x) str_wrap(as.character(x),width=10)) + 
  guides(fill=guide_legend(ncol=7))
dev.off()

#-------------------------------------------------------------------------------------------------------
#  End Version 2 (note that objects from version 2 are used below so do not clear workspace)
#-------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------
#  Begin generation of the age-space-time stock compositions 
#-------------------------------------------------------------------------------------------------------

# To calculate the total PSC catch in each stratum we have to assume that we know the age structure for that stratum.
# We can use the above calculations, which tell us the age structure of the genotyped samples. 
# But in the above, we included the non-aged samples. Create a proportion for each age / stratum that includes only the aged samples
agesforpcalc <- agesummaryplot %>% 
  group_by(stratum) %>%
  mutate(propaged=ifelse(age!="No age",freq/sum(freq[age!="No age"]),0))

#  For the subsequent calculation of the P=0 values we will need the total PSC catch from each of the time and space strata.
#  Read in these data and begin summarizing across strata.
pscdata <- read_excel("rcode/Data/AKFIN Genetic BSAI Salmon Bycatch 2017.xlsx",sheet="Genetic BSAI Salmon Bycatch") %>% 
  rename_all(tolower) %>% 
  rename(chum=`sum(number_chum)`,
         cluster=`all clusters`) %>% 
  mutate(period=ifelse(week_number>=earlycluster & week_number<latecluster,"early",
                              ifelse(week_number>=latecluster,"late",NA)))

#  Create a function to do this for each of the specifed strata
pscsummaryfun <- function(mystratum,mydata){
  mydata %>%
    summarize(chum=sum(chum),
              labels2=mystratum)
}

pscsummary <- bind_rows(
  pscsummaryfun("All",pscdata),
  pscsummaryfun("Cluster 1",pscdata %>% filter(cluster=="1")),
  pscsummaryfun("Cluster 2",pscdata %>% filter(cluster=="2")),
  pscsummaryfun("Cluster 3",pscdata %>% filter(cluster=="3")),
  pscsummaryfun("Cluster 4",pscdata %>% filter(cluster=="4")),
  pscsummaryfun("Cluster 1 Early",pscdata %>% filter(cluster=="1" & period=="early")),
  pscsummaryfun("Cluster 1 Late",pscdata %>% filter(cluster=="1" & period=="late")),
  pscsummaryfun("Cluster 2 Early",pscdata %>% filter(cluster=="2" & period=="early")),
  pscsummaryfun("Clusters 1-2 Early",pscdata %>% filter(cluster%in%unique(pscdata$cluster[grep(paste(c("1","2"),collapse="|"),pscdata$cluster)]) & period=="early")),
  pscsummaryfun("Cluster 1-2 Late",pscdata %>% filter(cluster%in%unique(pscdata$cluster[grep(paste(c("1","2"),collapse="|"),pscdata$cluster)]) & period=="late")),
  pscsummaryfun("Cluster 3-4 Early",pscdata %>% filter(cluster%in%unique(pscdata$cluster[grep(paste(c("3","4"),collapse="|"),pscdata$cluster)]) & period=="early")),
  pscsummaryfun("Cluster 3-4 Late",pscdata %>% filter(cluster%in%unique(pscdata$cluster[grep(paste(c("3","4"),collapse="|"),pscdata$cluster)]) & period=="late"))
)

#  Need a way to join the strata names and the bayes file/folder names for the subsequent stock comp section. 
#  The "group" column is the name of all the folders from the Bayes runs which allows us to pull the data from the Bayes *.rgn files.
key <- data.frame(labels2=rep(c("All","Cluster 1","Cluster 2","Cluster 3","Cluster 4","Cluster 1 Early","Cluster 1 Late",
                                "Cluster 2 Early","Clusters 1-2 Early","Cluster 1-2 Late","Cluster 3-4 Early","Cluster 3-4 Late"),3),
           group=c(paste0("Age",3,c("tot","Area1","Area2","Area3","Area4","Area1E","Area1L","Area2E","Area1-2E","Area1-2L","Area3-4E","Area3-4L")),
                  paste0("Age",4,c("tot","Area1","Area2","Area3","Area4","Area1E","Area1L","Area2E","Area1-2E","Area1-2L","Area3-4E","Area3-4L")),
                  paste0("Age",5,c("tot","Area1","Area2","Area3","Area4","Area1E","Area1L","Area2E","Area1-2E","Area1-2L","Area3-4E","Area3-4L"))),
           age=as.character(c(sort(rep(3:5,12)))))

#  Define sample sizes for each age-time-area stratum (i.e., number of genotyped and aged fish for each).
ageclusters <- data.frame(group=c(
  "Age3Area1","Age3Area1-2E","Age3Area1-2L",
  "Age3Area2","Age3Area2E",
  "Age3Area3","Age3Area3-4E","Age3Area3-4L",
  "Age3Area4",
  "Age3tot",
  "Age4Area1","Age4Area1-2E","Age4Area1-2L","Age4Area1E","Age4Area1L",
  "Age4Area2","Age4Area2E",
  "Age4Area3","Age4Area3-4E","Age4Area3-4L",
  "Age4Area4",
  "Age4tot",
  "Age5Area1","Age5Area1-2E",
  "Age5Area2",
  "Age5tot"
),
samples=c(147,224,134,211,136,87,105,124,142,613,432,749,195,324,108,512,425,119,193,103,177,1323,100,170,92,245))


#  Finally, keep only the ages and clusters for which there are data. We need these for the P=0 calculation coming up.
psctotals <- pscsummary %>% 
  right_join(agesforpcalc) %>% 
  mutate(chum=round(chum*propaged,0)) %>% 
  dplyr::select(labels2,chum,age) %>% 
  left_join(key) %>% 
  inner_join(ageclusters) %>% 
  filter(!is.na(group)) %>% 
  data.frame

#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
#  Pull P=0 values from Bayes data ----
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---

folder.names <- list.files(path="rcode/Data/bayes/ages",pattern=".")
myout <- data.frame()

for(i in 1:length(folder.names)){
  print(folder.names[i])
  myrgn <- list.files(path=paste0("rcode/Data/bayes/ages/",folder.names[i]),pattern="RGN")
  
  test <- lapply(myrgn,function(x)data.frame(chain=x,read.table(paste0("rcode/Data/bayes/ages/",folder.names[i],"/",x))))
  test2 <- bind_rows(test[[1]][(nrow(test[[1]])-4999):nrow(test[[1]]),],
                     test[[2]][(nrow(test[[2]])-4999):nrow(test[[2]]),],
                     test[[3]][(nrow(test[[3]])-4999):nrow(test[[3]]),],
                     test[[4]][(nrow(test[[4]])-4999):nrow(test[[4]]),],
                     test[[5]][(nrow(test[[5]])-4999):nrow(test[[5]]),],
                     test[[6]][(nrow(test[[6]])-4999):nrow(test[[6]]),])
  names(test2) <- c("chain","iteration","SE Asia","NE Asia","Western AK","Up/Mid Yukon","SW Alaska","E GOA/PNW")
  
  psctotal <- psctotals$chum[psctotals$group==folder.names[i]]
  #  placeholder psctotal
  #psctotal <- 10000
  pthreshold <- test2 %>% 
    dplyr::select(-c(chain,iteration)) %>% 
    summarise_all(funs(0.5/(psctotal*mean(.)))) %>% 
    gather(stock,threshold)
  
  myout <- bind_rows(myout,test2 %>% 
                       dplyr::select(-c(chain,iteration)) %>% 
                       gather(stock,value) %>% 
                       inner_join(pthreshold) %>% 
                       group_by(stock) %>% 
                       summarise(`P = 0`=sum(ifelse(value<threshold,1,0))/30000,
                                 group=folder.names[i]) %>% 
                       spread(stock,`P = 0`))
}

pvalues <- myout %>% 
  gather(Region,`P=0`,-group)

#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
#  Pull stock proportions from Bayes data ----
#  It is important here that the Bayes output file with the Gelman and Rubin stats are consistent.
#  In some cases, I had to go into the .SUM file and make sure that the file path names were only one line long.
#  I also had to delete a chunk of info that displayed Raftery and Lewis output that had not appeared in the original files.
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---

folder.names <- list.files(path="rcode/Data/bayes/ages",pattern=".")

stockcomp<-data.frame()

for(i in 1:length(folder.names)){
  print(i)
  myrgn <- list.files(path=paste0("rcode/Data/bayes/ages/",folder.names[i]),pattern="estimate")
  
  shrink <- read.table(paste0("rcode/Data/bayes/ages/",folder.names[i],"/",myrgn),skip=450,nrows = 6) %>% 
    bind_cols(data.frame(Region)) %>% 
    dplyr::select(Region,`Shrink Factor`=V4)
  
  comp <- read.table(paste0("rcode/Data/bayes/ages/",folder.names[i],"/",myrgn),skip=539,nrows = 6) %>% 
    bind_cols(data.frame(Region)) %>%   
    mutate(group=folder.names[i]) %>% 
    dplyr::select(group,Region,Mean=V4,SD=V5,`2.5%`=V6,Median=V7,`97.5%`=V8)
  
  stockcomp <- bind_rows(stockcomp,
                         comp %>% inner_join(shrink)) %>% 
    mutate(group=as.character(group),
           Region=as.character(Region))
}



#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
#  Create stock composition file ----
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---

#  Define the names of each of the Bayes runs (these correspond to the Bayes folder names)
mygroupings <- c("Age3Area1","Age3Area1-2E","Age3Area1-2L","Age3Area2","Age3Area2E","Age3Area3","Age3Area3-4E","Age3Area3-4L","Age3Area4","Age3tot",
                 "Age4Area1","Age4Area1-2E","Age4Area1-2L","Age4Area1E","Age4Area1L","Age4Area2","Age4Area2E",
                 "Age4Area3","Age4Area3-4E","Age4Area3-4L","Age4Area4","Age4tot",
                 "Age5Area1","Age5Area1-2E","Age5Area2","Age5tot")

mygroupings <- list.files(path="rcode/Data/bayes/ages",pattern=".")


stockdata <- stockcomp %>% 
  inner_join(pvalues) %>% 
  inner_join(psctotals) %>% 
  mutate(group=fct_relevel(group,mygroupings),
         `Est. num.`=round(Mean*chum)) %>% 
  dplyr::select(group,Region,`Est. num.`,Mean,SD,`2.5%`,Median,`97.5%`,`P=0`,`Shrink Factor`) %>% 
  arrange(group)


#  The following outputs a text file for putting into the report. However, the formatting is lost if you just open it in Excel haphazardly:
#  1) Go through the delimited file options - 
#     a) deselect "tab delimited and select "comma delimited".
#     b) highlight all columns and change type to "text"
#  2) Because it's a comma delimited field, I could not use a comma to make big numbers pretty. I arbitrarily used an "!" instead.
#     Find and replace all "!" with a ","

sink("rcode/Data/mystockcomps_ageclusters.txt")
groups <- unique(stockdata$group)

for(i in 1:length(groups)){
  temp <- psctotals %>% filter(group %in% as.character(groups[i]))
  cat(paste0("Age ",temp$age," ",temp$labels2," sample set (PSC = ",prettyNum(round(temp$chum),big.mark="!"),"; n=",prettyNum(temp$samples,big.mark="!"),")"))
  cat('\n')
  write.csv(
    stockdata %>% 
      filter(group==groups[i]) %>% 
      dplyr::select(-group) %>% 
      mutate(`Shrink Factor`=formatC(as.numeric(`Shrink Factor`),digits=2,format="f",flag='0'),
             `Est. num.`=ifelse(nchar(`Est. num.`)>3,paste0("'",prettyNum(`Est. num.`,big.mark="!")),`Est. num.`)) %>% 
      mutate_at(3:8,funs(ifelse(.>0,formatC(.,digits=3,format="f"),formatC(.,digits=0,format="fg")))),row.names=FALSE) 

  cat('\n')
}
sink()








stockcomp <- stockcomp %>% 
  mutate(age=as.numeric(substr(group,4,4)),
         Region=fct_relevel(Region,
                     "SE Asia",
                     "NE Asia",
                     "Western AK",
                     "Up/Mid Yuk",
                     "SW Alaska",
                     "E GOA/PNW"))

pdf("Age3_Comps.pdf",width=11,height=8.5)
stockcomp %>% 
  filter(age==3) %>% 
  ggplot(aes(Region,Mean)) + 
  geom_bar(stat="identity",fill=strongblue) + 
  facet_wrap(~group) +
  theme_bw() + 
  theme(axis.text.x = element_text(size=8,angle=45,vjust=0.75))
dev.off()

pdf("Age4_Comps.pdf",width=11,height=8.5)
stockcomp %>% 
  filter(age==4) %>% 
  ggplot(aes(Region,Mean)) + 
  geom_bar(stat="identity",fill=strongblue) + 
  facet_wrap(~group) + 
  theme_bw() + 
  theme(axis.text.x = element_text(size=8,angle=45,vjust=0.75))
dev.off()

pdf("Age5_Comps.pdf",width=11,height=8.5)
stockcomp %>% 
  filter(age==5) %>% 
  ggplot(aes(Region,Mean)) + 
  geom_bar(stat="identity",fill=strongblue) + 
  facet_wrap(~group) +
  theme_bw() + 
  theme(axis.text.x = element_text(size=8,angle=45,vjust=0.75))
dev.off()

stockcomp %>% 
  ggplot(aes(Region,Mean)) + 
  geom_bar(stat="identity",fill=strongblue) + 
  facet_grid(group~.) +
  theme_bw() + 
  theme(axis.text.x = element_text(size=8,angle=45,vjust=0.75))
