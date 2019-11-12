#  Create figures for chum salmon bycatch tech memo 
#  Author: Jordan Watson, jordan.watson@noaa.gov
#  Creation date: 10/16/2018

#  If you just want to make figures, you can scroll down to the figures section and read in 
#  a file that already contains the data.

#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
# Load packages ----
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---

library(tidyverse)
library(readxl)
library(forcats)
library(viridis)
library(gridExtra)
library(grid)
library(stringi)
library(scales)


#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
#  Pull P=0 values from Bayes data ----
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---

psctotals <- read_excel("rcode/Data/psc_totals_by_sector_and_year.xlsx")


folder.names <- list.files(path="rcode/Data/bayes/sector",pattern=".")
myout <- data.frame()

for(i in 1:length(folder.names)){
  print(folder.names[i])
  myrgn <- list.files(path=paste0("rcode/Data/bayes/sector/",folder.names[i]),pattern="RGN")
  
  test <- lapply(myrgn,function(x)data.frame(chain=x,read.table(paste0("rcode/Data/bayes/sector/",folder.names[i],"/",x))))
  test2 <- bind_rows(test[[1]][(nrow(test[[1]])-4999):nrow(test[[1]]),],
                     test[[2]][(nrow(test[[2]])-4999):nrow(test[[2]]),],
                     test[[3]][(nrow(test[[3]])-4999):nrow(test[[3]]),],
                     test[[4]][(nrow(test[[4]])-4999):nrow(test[[4]]),],
                     test[[5]][(nrow(test[[5]])-4999):nrow(test[[5]]),],
                     test[[6]][(nrow(test[[6]])-4999):nrow(test[[6]]),])
  names(test2) <- c("chain","iteration","SE Asia","NE Asia","Western AK","Up/Mid Yuk","SW Alaska","E GOA/PNW")
  
  psctotal <- psctotals$chum[psctotals$group==folder.names[i]]
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

folder.names <- list.files(path="rcode/Data/bayes/sector",pattern=".")

stockcomp<-data.frame()

Region=c("SE Asia","NE Asia","Western AK","Up/Mid Yuk","SW Alaska","E GOA/PNW")

for(i in 1:length(folder.names)){
  print(i)
  myrgn <- list.files(path=paste0("rcode/Data/bayes/sector/",folder.names[i]),pattern="estimate")
  
  shrink <- read.table(paste0("rcode/Data/bayes/sector/",folder.names[i],"/",myrgn),skip=450,nrows = 6) %>% 
    bind_cols(data.frame(Region)) %>% 
    dplyr::select(Region,`Shrink Factor`=V4)
  
  comp <- read.table(paste0("rcode/Data/bayes/sector/",folder.names[i],"/",myrgn),skip=539,nrows = 6) %>% 
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

mygroupings <- list.files(path="rcode/Data/bayes/sector",pattern=".")

stockdata <- stockcomp %>% 
  inner_join(pvalues) %>% 
  inner_join(psctotals) %>% 
  mutate(group=fct_relevel(group,mygroupings),
         `Est. num.`=round(Mean*chum)) %>% 
  dplyr::select(group,Region,`Est. num.`,Mean,SD,`2.5%`,Median,`97.5%`,`P=0`,`Shrink Factor`) %>% 
  arrange(group)

#saveRDS(stockdata,file="rcode/Data/stockcomp_sector.RDS")

#  The following outputs a text file for putting into the report. However, the formatting is lost if you just open it in Excel haphazardly:
#  1) Go through the delimited file options - 
#     a) deselect "tab delimited and select "comma delimited".
#     b) highlight all columns and change type to "text"
#  2) Because it's a comma delimited field, I could not use a comma to make big numbers pretty. I arbitrarily used an "!" instead.
#     Find and replace all "!" with a ","

sink("rcode/Data/mysectorstockcomps.csv")
groups <- unique(stockdata$group)

for(i in 1:length(groups)){
  temp <- psctotals %>% filter(group %in% as.character(groups[i]))
  cat(paste0(temp$group," sample set (PSC = ",prettyNum(round(temp$chum),big.mark="!"),"; n=",prettyNum(temp$samples,big.mark="!"),")"))
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



mygroups <- data.frame(group=c('2013-14_M',	'2013_CP',	'2013_S',	'2014_CP',	'2014_S',	'2015_CP',	'2015_M',	'2015_S',	'2016_CP',	'2016_M',	'2016_S',	'2017_CP',	'2017_S'),
                       year=c('2013-14',	'2013',	'2013',	'2014',	'2014',	'2015',	'2015',	'2015',	'2016',	'2016',	'2016',	'2017',	'2017'),
                       sector=c('Mothership',	'Catcher Processor',	'Shoreside',	'Catcher Processor',	'Shoreside',	'Catcher Processor',	'Mothership',	'Shoreside',	'Catcher Processor',	'Mothership',	'Shoreside',	'Catcher Processor',	'Shoreside'))

strongred <- rgb(215,48,39,max=255)
salmon <- rgb(252,141,89,max=255)
yellow <- rgb(254,224,144,max=255)
#steelblue <- rgb(224,243,248,max=255)
periwinkle <- rgb(181,63,250,max=255)
bluegrey <- rgb(145,191,219,max=255)
strongblue <- rgb(69,117,180,max=255)

mypalette <- c(strongred,salmon,yellow,bluegrey,strongblue,periwinkle)

library(cowplot)


myplot <- stockdata %>% 
  inner_join(mygroups) %>% 
  filter(sector!="Mothership") %>% 
  ggplot(aes(year,Mean,fill=Region)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~sector) + 
  theme_bw() + 
  scale_fill_manual(values=mypalette) + 
  guides(fill=guide_legend(ncol=3)) + 
  ylab("Stock proportion") + 
  xlab("Year") +
  theme(legend.position='none',
        axis.text.x=element_text(size=7))

p2 <- stockdata %>% 
  inner_join(mygroups) %>% 
  filter(sector=="Mothership") %>% 
  ggplot(aes(year,Mean,fill=Region)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~sector) + 
  theme_bw() + 
  theme(axis.title.y=element_blank(),
        axis.text.x=element_text(size=7)) +
  scale_fill_manual(values=mypalette) + 
  xlab("Year")

png("Figures/sector_stock_comp.png",width=6.5,height=3,units="in",res=300)
grid.arrange(myplot,p2,nrow=1)
dev.off()

png("Figures/sector_stock_comp2.png",width=6.5,height=6.5,units="in",res=300)
stockdata %>% 
  inner_join(mygroups) %>% 
  filter(sector!="Mothership") %>% 
  ggplot(aes(Region,Mean,fill=sector,ymin=`2.5%`,ymax=`97.5%`)) + 
  geom_bar(stat="identity",position="dodge") + 
  geom_errorbar(width=0.2,position=position_dodge(0.9),size=0.25) + 
  facet_wrap(~year,ncol=2) + 
  theme_bw() + 
  scale_fill_manual(values=mypalette) + 
  ylab("Stock proportion") + 
  xlab("Year") + 
  theme(legend.position=c(0.75,0.15),
        axis.text.x=element_text(size=6))
dev.off()

png("Figures/sector_stock_comp2_mother.png",width=6.5,height=6.5,units="in",res=300)
stockdata %>% 
  inner_join(mygroups) %>% 
  filter(sector=="Mothership") %>% 
  ggplot(aes(Region,Mean,fill=sector,ymin=`2.5%`,ymax=`97.5%`)) + 
  geom_bar(stat="identity",position="dodge") + 
  geom_errorbar(width=0.2,position=position_dodge(0.9),size=0.25) + 
  facet_wrap(~year,ncol=2) + 
  theme_bw() + 
  scale_fill_manual(values=mypalette) + 
  ylab("Stock proportion") + 
  xlab("Year") + 
  theme(legend.position=c(0.75,0.15),
        axis.text.x=element_text(size=6))
dev.off()

#  2017 only data for NPFMC presentation (note there is no mothership data for this year)

stockdata <- readRDS("rcode/Data/stockcomp_sector.RDS")

png("Figures/sector_stock_comp_2017_for_NPFMC.png",width=6.5,height=6.5,units="in",res=300)
stockdata %>% 
  inner_join(mygroups) %>% 
  mutate(Region=fct_relevel(Region,
                            "SE Asia",
                            "NE Asia",
                            "Western AK",
                            "Up/Mid Yuk",
                            "SW Alaska",
                            "E GOA/PNW")) %>% 
  filter(sector!="Mothership" & year==2017) %>% 
  ggplot(aes(Region,Mean,fill=sector,ymin=`2.5%`,ymax=`97.5%`)) + 
  geom_bar(stat="identity",position="dodge") + 
  geom_errorbar(width=0.2,position=position_dodge(0.9),size=0.25) + 
  theme_bw() + 
  scale_fill_manual(values=mypalette,guides(name="Sector")) + 
  ylab("Stock proportion") + 
  xlab("") + 
  theme(legend.position=c(0.75,0.75),
        axis.text.x=element_text(size=9),
        legend.text = element_text(size=13),
        panel.grid=element_blank())
dev.off()  


