#  Create figures for chum salmon bycatch tech memo 
#  Author: Jordan Watson, jordan.watson@noaa.gov
#  Creation date: 10/16/2018


#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
# Load packages ----
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---

library(tidyverse)
library(readxl)
library(forcats)
library(viridis)
library(gridExtra)
library(grid)

#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
# Color palette ----
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---

strongred <- rgb(215,48,39,max=255)
salmon <- rgb(252,141,89,max=255)
yellow <- rgb(254,224,144,max=255)
#steelblue <- rgb(224,243,248,max=255)
periwinkle <- rgb(181,63,250,max=255)
bluegrey <- rgb(145,191,219,max=255)
strongblue <- rgb(69,117,180,max=255)

mypalette <- c(strongred,salmon,yellow,bluegrey,strongblue,periwinkle)
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
# Axis labels ----
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---

#  Weekly labels (create labels in 5 week increments starting on week 5)
mylabel <- rep("",52)
mylabel[seq(5,52,by=5)] <- seq(5,52,by=5)


#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
# Define relevant groupings for chum tech memo -----
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---

Region=c("SE Asia","NE Asia","Western AK","Up/Mid Yuk","SW Alaska","E GOA/PNW")

mygroupings <- c("GOA","B-season","BS Early","BS Middle","BS Late","517","517 Early","517 Middle","517 Late","521","521 Early","521 Middle","521 Late","EBS","WBS")

this.year <- 2017

goa.a.start <- as.POSIXct(strptime("01/20/2017","%m/%d/%Y"))
goa.a.end <- as.POSIXct(strptime("03/09/2017","%m/%d/%Y"))
goa.b.start <- as.POSIXct(strptime("03/10/2017","%m/%d/%Y"))
goa.b.end <- as.POSIXct(strptime("05/31/2017","%m/%d/%Y"))
goa.c.start <- as.POSIXct(strptime("08/25/2017","%m/%d/%Y"))
goa.c.end <- as.POSIXct(strptime("09/30/2017","%m/%d/%Y"))
goa.d.start <- as.POSIXct(strptime("10/01/2017","%m/%d/%Y"))
goa.d.end <- as.POSIXct(strptime("10/31/2017","%m/%d/%Y"))

#  For 2017, b season ends during the same stat week as a season ends. For weekly illustrations 
awkstart <- as.numeric(strftime(goa.a.start,format="%W")) 
awkend <- as.numeric(strftime(goa.a.end,format="%W"))  
bwkstart <- as.numeric(strftime(goa.b.start,format="%W"))
bwkend <- as.numeric(strftime(goa.b.end,format="%W"))  
cwkstart <- as.numeric(strftime(goa.c.start,format="%W")) 
cwkend <- as.numeric(strftime(goa.c.end,format="%W"))  
dwkstart <- as.numeric(strftime(goa.d.start,format="%W")) 
dwkend <- as.numeric(strftime(goa.d.end,format="%W")) 

#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
# Tally PSC abundances by groupings ----
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---

#  Read in Bering Sea data
data <- read_excel("rcode/Data/psc_all_catch_and_genetics.xlsx",sheet="Genetic BSAI Salmon Bycatch") %>% 
  rename_all(tolower) %>% 
  mutate(period=ifelse(week_number<23,NA,
                       ifelse(week_number>22 & week_number<30,"Early",
                              ifelse(week_number>29 & week_number<35,"Middle","Late"))),
         period=fct_relevel(period,"Early","Middle"))

wbs <- c(521,523,524)

#  Read in GOA data
goa <- read_excel("rcode/Data/psc_all_catch_and_genetics.xlsx",sheet="Genetic GOA Salmon Bycatch") %>% 
  rename_all(tolower)


psctotals <- bind_rows(data %>% 
            filter(season=="B") %>% 
            summarise(group="B-season",
                      chum=sum(`sum(number_chum)`),
                      samples=sum(`sum(total_chum_finclip)`)),
          data %>% 
            filter(!is.na(period)) %>% 
            group_by(period) %>% 
            summarise(chum=sum(`sum(number_chum)`),
                      samples=sum(`sum(total_chum_finclip)`)) %>% 
            ungroup %>% 
            rename(group=period) %>% 
            mutate(group=paste("BS",group,sep=" ")),
          data %>% 
            filter(nmfs_area %in%c(517,521) & season=="B") %>% 
            group_by(nmfs_area) %>% 
            summarise(chum=sum(`sum(number_chum)`),
                      samples=sum(`sum(total_chum_finclip)`)) %>% 
            rename(group=nmfs_area) %>% 
            mutate(group=as.character(group)),
          data %>% 
            filter(nmfs_area %in%c(517,521) & !is.na(period)) %>% 
            group_by(nmfs_area,period) %>% 
            summarise(chum=sum(`sum(number_chum)`),
                      samples=sum(`sum(total_chum_finclip)`)) %>% 
            mutate(group=paste(nmfs_area,period,sep=" ")) %>% 
            ungroup %>% 
            dplyr::select(-c(nmfs_area,period)),
          data %>% 
            filter((nmfs_area %in% wbs) & season=="B") %>% 
            summarise(group="WBS",
                      chum=sum(`sum(number_chum)`),
                      samples=sum(`sum(total_chum_finclip)`)),
          data %>% 
            filter((!nmfs_area %in% wbs) & season=="B") %>% 
            summarise(group="EBS",
                      chum=sum(`sum(number_chum)`),
                      samples=sum(`sum(total_chum_finclip)`)),
          goa %>% 
            summarise(group="GOA",
                      chum=sum(`sum(number_nonchinook)`),
                      samples=sum(`sum(total_chum_finclip)`)))


#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
#  Pull P=0 values from Bayes data ----
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---

folder.names <- list.files(path="rcode/Data/bayes",pattern=".")
myout <- data.frame()

for(i in 1:length(folder.names)){
  print(folder.names[i])
  myrgn <- list.files(path=paste0("rcode/Data/bayes/",folder.names[i]),pattern="RGN")
  
  test <- lapply(myrgn,function(x)data.frame(chain=x,read.table(paste0("rcode/Data/bayes/",folder.names[i],"/",x))))
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
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---


folder.names <- list.files(path="rcode/Data/bayes",pattern=".")

stockcomp<-data.frame()

for(i in 1:length(folder.names)){
myrgn <- list.files(path=paste0("rcode/Data/bayes/",folder.names[i]),pattern="estimate")

shrink <- read.table(paste0("rcode/Data/bayes/",folder.names[i],"/",myrgn),skip=450,nrows = 6) %>% 
  bind_cols(data.frame(Region)) %>% 
  dplyr::select(Region,`Shrink Factor`=V4)

comp <- read.table(paste0("rcode/Data/bayes/",folder.names[i],"/",myrgn),skip=539,nrows = 6) %>% 
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

#mygroupings <- c("GOA","B-season","BS Early","BS Middle","BS Late","517 Early","517 Middle","517 Late","521","521 Early","521 Middle","521 Late","EBS","WBS")


stockdata <- stockcomp %>% 
  inner_join(pvalues) %>% 
  inner_join(psctotals) %>% 
  mutate(group=fct_relevel(group,mygroupings),
         `Est. num.`=round(Mean*chum)) %>% 
  dplyr::select(group,Region,`Est. num.`,Mean,SD,`2.5%`,Median,`97.5%`,`P=0`,`Shrink Factor`) %>% 
  arrange(group)


sink("rcode/Data/mystockcomps.csv")
groups <- unique(stockdata$group)

for(i in 1:length(groups)){
  temp <- psctotals %>% filter(group %in% as.character(groups[i]))
  cat(paste0(temp$group," sample set (PSC = ",round(temp$chum),"; n=",temp$samples,")"))
cat('\n')
  write.csv(
    stockdata %>% 
      filter(group==groups[i]) %>% 
      dplyr::select(-group) %>% 
      mutate(Mean=round(Mean,3),
             SD=round(SD,3),
             `2.5%`=round(`2.5%`,3),
             Median=round(Median,3),
             `97.5%`=round(`97.5%`,3),
             `P=0`=round(`P=0`,3),
             `Shrink Factor`=round(`Shrink Factor`,2)),row.names=FALSE)
cat('\n')
}
sink()



#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
#  Figure 2 non Chinook PSC by year----
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---

#  Update this.year to the year of current report
#this.year <- 2017
start.year <- 1991
myn <- (this.year-start.year) + 1

#  Read in Bering Sea data
data <- read_excel("rcode/Data/psc_all_catch_and_genetics.xlsx",sheet="fig2",skip=53,n_max=27) %>% 
  rename_all(tolower) %>% 
  dplyr::select(year,psc=`with cdq`) %>% 
  mutate(psc=as.numeric(psc)) %>% 
  filter(!is.na(psc))


mymean <- mean(data$psc[data$year<this.year])/1000
mymedian <- median(data$psc[data$year<this.year])/1000

# Jordan, format later.
png("Figures/figure2_psc_totals.png",width=7.5,height=6,units="in",res=300)
data %>% 
  ggplot(aes(year,psc/1000)) + 
  geom_line(color=strongblue,size=1.25) + 
  geom_point(size=2,color=strongred) + 
  geom_hline(yintercept=c(mymean,mymedian),linetype=c(1,2)) + 
  theme_bw() + 
  theme(panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black")) + 
  ylab("Number of fish x 1,000") + 
  xlab("Year") + 
  scale_x_continuous(breaks=seq(1994,this.year,by=2))
dev.off()


#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
#  Figure 3 PSC vs. genetic by week ----
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---

#  Read in Bering Sea data
data <- read_excel("rcode/Data/psc_all_catch_and_genetics.xlsx",sheet="Genetic BSAI Salmon Bycatch") %>% 
  rename_all(tolower) %>% 
  group_by(week_number) %>% 
  summarise(chum=sum(`sum(number_chum)`),
            chum2=chum/1000,
            samples=sum(`sum(total_chum_finclip)`),
            ratio=chum2/samples)

month.labels <- data.frame(month=as.numeric(strftime(seq(as.Date(paste0(this.year,"/1/1")), as.Date(paste0(this.year,"/12/31")), "weeks"),format="%m")),
                           week=as.numeric(strftime(seq(as.Date(paste0(this.year,"/1/1")), as.Date(paste0(this.year,"/12/31")), "weeks"),format="%W"))) %>% 
  filter(week<max(data$week_number)) %>% 
  group_by(month) %>% 
  summarise(center=mean(week)) %>% 
  data.frame %>% 
  mutate(mymonth=month.abb[month]) 

png("Figures/figure3.png",width=6,height=3,units="in",res=300)
par(mar = c(5,5,2,5))
#with(data, plot(week_number, chum2,type="n",ylim=c(0,mymax*1.2),ylab="Chum catch x 1,000",las=1))
with(data, plot(week_number, chum2, type="l", col="black",las=1,axes=F,xlab=NA,ylab=NA))
box()
axis(side = 2,las=1, labels=NA,tck=-0.02)
axis(side = 2,las=1, lwd = 0,line=-0.75,cex.axis=0.75)
axis(side = 1,las=1,at=seq(5,40,by=5), labels=NA,tck=-0.02)
axis(side = 1,las=1,at=seq(5,40,by=5),lwd = 0,line=-1,cex.axis=0.75)
par(new = T)
with(data, plot(week_number, samples, type="l",lwd=2, lty=2,col=strongred, axes=F, xlab=NA, ylab=NA))
axis(side = 4,las=1,labels=NA,tck=-0.02)
axis(side = 4,las=1,lwd=0,line=-0.75,cex.axis=0.75)
mtext(side = 4, line = 1.5, 'Genetic samples',cex=0.75)
mtext(side = 1, line = 0.65, 'Statistical week',cex=0.75)
mtext(side = 2, line = 1.25, 'Chum catch x 1,000',cex=0.75)
legend("topleft",
       legend=c(paste0("Chum samples (",formatC(sum(data$chum), format="d", big.mark=","),")"),
                paste0("Genetic samples (",formatC(sum(data$samples), format="d", big.mark=","),")")),
       lty=c(1,2), lwd=c(1,1),col=c("black", strongred),cex=0.5)
axis(side=3,at=month.labels$center,labels=NA,tck=-0.02)
axis(side=3,at=month.labels$center,labels=month.labels$mymonth,lwd=0,cex.axis=0.75,line=-0.95)
dev.off()



#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
#  Figure 4 PSC vs genetic by wk area----
#  Note, this figure is pretty simple but the month polygons at the top are complicated and account for most of the code.
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---

#  Read the data and filter for B season and any spatial groupings you want. In this case, 517, 521.
data <- read_excel("rcode/Data/psc_all_catch_and_genetics.xlsx",sheet="Genetic BSAI Salmon Bycatch") %>% 
  rename_all(tolower) %>% 
  filter(nmfs_area %in%c(509,513,516,517,519,521,523,524) & season=="B") %>%
  group_by(week_number,nmfs_area) %>%
  summarise(vessels=length(unique(catcher_vessel_adfg)),
            chum=sum(`sum(number_chum)`),
            samples=sum(`sum(total_chum_finclip)`))

data2 <- data %>% 
  mutate(nmfs=ifelse(nmfs_area%in%c(509,513,516),"509/513/516",
                     ifelse(nmfs_area%in%c(521,523,524),"521/523/524",as.character(nmfs_area)))) %>% 
  group_by(nmfs,week_number) %>% 
  summarise(vessels=sum(vessels),
            chum=sum(chum),
            samples=sum(samples))
  
#  For 2017, we group weeks 23-25 and 37-40. I put this as a separate chunk so it's easier to customize for different years.
#  This is also the chunk where I divide the number of chum by 1000 to simplify the y-axis scale.
data3 <- data2 %>% 
  mutate(week=ifelse(week_number %in% c(23:25),25,
                     ifelse(week_number%in%c(37:41),37,week_number))) %>% 
  group_by(nmfs,week_number) %>% 
  summarise(vessels=sum(vessels),
            chum=sum(chum)/1000,
            samples=sum(samples),
            week=week[1],
            week2=ifelse(week==25,"23-25",
                         ifelse(week==37,"37-41",
                                as.character(week)))) %>% 
  arrange(vessels) %>% 
  filter(vessels>2) #  Filter out combos with fewer than 3 vessels.

#  This is for the month polygons. Because the month polygons span different numbers of weeks, we have to come up with decimals values corresponding
#  to the point at which a particular month begins and ends. For example, if Monday the first day of week 5, it's decimal week value is 5.0. Each additional
#  day adds about 0.143 weeks, so Tuesday would be week 5.143. This allows us to create the minimum and maximum decimal week values for each polygon. 

month.labels <- data.frame(month=as.numeric(strftime(seq(as.Date(paste0(this.year,"/1/1")), 
                                                         as.Date(paste0(this.year,"/12/31")), "days"),format="%m")),
                           week=as.numeric(strftime(seq(as.Date(paste0(this.year,"/1/1")), 
                                                        as.Date(paste0(this.year,"/12/31")), "days"),format="%W")),
                           julian=as.numeric(strftime(seq(as.Date(paste0(this.year,"/1/1")), 
                                                          as.Date(paste0(this.year,"/12/31")), "days"),format="%j")),
                           weekday=weekdays(seq(as.Date(paste0(this.year,"/1/1")), 
                                                as.Date(paste0(this.year,"/12/31")), "days"))) %>% 
  group_by(month) %>% 
  mutate(weekdayno=(as.numeric(fct_relevel(weekday,"Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")))-1,
         monthendfraction=week+(weekdayno/7),
         mymax=ifelse(julian==max(julian),1,0),
         mymin=ifelse(julian==min(julian),1,0)) %>% 
  data.frame

#  Still working on the polygons. If we were showing the entire year, this would be less complicated. But we are showing barplots beginning with a bar that represents
#  week 23-25. We know that week 23 is in June (so I filter for month>6). However, our figure is functionally only going to go as low as week 24.5. So I say that if 
#  a month starts at a week less than 25, round it to 24.5, which will correspond to the minimum range of the barplot. Same thing happens on the other end of the plot, 
#  where our barplots will go up to week 37 (we already combined the data for 37:40). 
molab2 <- month.labels %>%
  filter(mymax==1 | mymin==1) %>% 
  mutate(terminus=ifelse(mymax==1,"max","min")) %>% 
  dplyr::select(-c(week,julian,weekday,weekdayno,mymax,mymin)) %>% 
  spread(terminus,monthendfraction) %>% 
  filter(month>=6 & month<10) %>% 
  mutate(mymin=ifelse(min<25,24.5,min),
         mymax=ifelse(max>37,37.5,max))

#  Now set the height of the month polygons to scale automatically based on the data. This may require a little futzing from year to year. You can adjust the height 
#  of the polygon by tweaking the minpercent and maxpercent values. 
chummax=max((data3 %>% group_by(week) %>% summarise(mysum=sum(chum)))$mysum)
samplesmax=max((data3 %>% group_by(week) %>% summarise(mysum=sum(samples)))$mysum)
minpercent=1.01
maxpercent=1.08

#  Create the actual month polygons and find the center value (we call it "center") at which the month label will be centered. 
datapoly <- data.frame(x=c(molab2$mymin[molab2$month==6],molab2$mymin[molab2$month==6],molab2$mymax[molab2$month==6],molab2$mymax[molab2$month==6],
                           molab2$mymin[molab2$month==7],molab2$mymin[molab2$month==7],molab2$mymax[molab2$month==7],molab2$mymax[molab2$month==7],
                           molab2$mymin[molab2$month==8],molab2$mymin[molab2$month==8],molab2$mymax[molab2$month==8],molab2$mymax[molab2$month==8],
                           molab2$mymin[molab2$month==9],molab2$mymin[molab2$month==9],molab2$mymax[molab2$month==9],molab2$mymax[molab2$month==9]),
                       ychum=rep(c(chummax*minpercent,chummax*maxpercent,chummax*maxpercent,chummax*minpercent),4),
                       ysamples=rep(c(samplesmax*minpercent,samplesmax*maxpercent,samplesmax*maxpercent,samplesmax*minpercent),4)) %>% 
  mutate(molabel=sort(rep(6:9,4))) %>% 
  group_by(molabel) %>% 
  mutate(center=mean(x),
         moname=month.abb[molabel]) %>% 
  data.frame

nmfs_n <- length(unique(data3$nmfs))

#  We probably could have faceted these two figures but this allows more customization. 
p1 <-  ggplot() + 
  geom_polygon(data=datapoly,aes(x,ychum),fill="grey",color="white") + 
  geom_bar(data=data3,aes(week,chum,fill=factor(nmfs)),stat="identity") + 
  geom_text(data=datapoly,aes(center,chummax*1.05,label=moname)) +
  theme_bw() + 
  theme(panel.grid=element_blank())  + 
  theme(legend.position="top",
        axis.title.x = element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black")) +
  scale_fill_manual(values=mypalette[1:nmfs_n],name="") + 
  scale_x_continuous(breaks=min(data3$week):max(data3$week),labels=c("23-25",paste(26:36),"37-41")) +
  ylab("Number of Fish (1,000)") + 
  annotate("text",x=34,y=0.8*chummax,label="Chum salmon PSC",size=4)
            
p2 <-  ggplot() + 
  geom_polygon(data=datapoly,aes(x,ysamples),fill="grey",color="white") + 
  geom_bar(data=data3,aes(week,samples,fill=factor(nmfs)),stat="identity") + 
  geom_text(data=datapoly,aes(center,samplesmax*1.05,label=moname)) +
  theme_bw() + 
  theme(panel.grid=element_blank(),
        legend.position="none",
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black")) +
  scale_fill_manual(values=mypalette[1:nmfs_n],name="") + 
  scale_x_continuous(breaks=min(data3$week):max(data3$week),labels=c("23-25",paste(26:36),"37-41")) +
  xlab("Statistical Week") + 
  ylab("Number of Fish") + 
  annotate("text",x=34,y=0.8*samplesmax,label="Chum salmon genetic samples",size=4)

#  Because the two plots have different numbers of digits in their y-axis scale, the figures don't line up right if you just use grid.arrange. 
#  Instead the key is to use grid.draw with size="last" so that plot two will be scale to match plot 1.
png("Figures/figure4.png",width=6,height=6.5,units="in",res=300)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))
dev.off()




#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
#  Figure 5 sample vs PSC by vessel ----
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---


data <- read_excel("rcode/Data/psc_all_catch_and_genetics.xlsx",sheet="Genetic BSAI Salmon Bycatch") %>% 
  rename_all(tolower) %>% 
  group_by(catcher_vessel_adfg) %>%
  summarise(chum=sum(`sum(number_chum)`),
            samples=sum(`sum(total_chum_finclip)`))

cor(data$chum,data$samples) # 0.94
cor(data$chum[data$chum<20000],data$samples[data$chum<20000]) # 0.94
summary(lm(samples~chum,data=data)) #0.89

#  Determine x-axis labels
labelsequence <- seq(0,max(data$chum),by=5000)

png("Figures/figure5.png",width=6,height=3,units="in",res=300)
data %>% 
  ggplot(aes(chum,samples)) + 
  geom_smooth(method="lm",se=FALSE,size=0.65) + 
  geom_point(size=0.95) + 
  theme_bw() + 
  theme(panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black"))  + 
  xlab("Number of chum salmon PSC per vessel") + 
  ylab("Number of genetic samples") + 
  annotate("text",x=labelsequence[3],y=0.95*max(data$samples),label=paste("Correlation coefficient\n r = ",round(cor(data$chum,data$samples),2))) + 
  scale_x_continuous(breaks=labelsequence) +
  coord_cartesian(expand=FALSE,xlim=c(0,max(data$chum*1.01))) 

dev.off()

data %>% 
  filter(chum<20000) %>% 
  ggplot(aes(chum,samples)) + 
  geom_smooth(method="lm",se=FALSE,size=0.65) + 
  geom_point(size=0.95) + 
  theme_bw() + 
  theme(panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black"))  + 
  xlab("Number of chum salmon PSC per vessel") + 
  ylab("Number of genetic samples") + 
  annotate("text",x=labelsequence[3],y=0.95*max(data$samples),label=paste("Correlation coefficient\n r = ",round(cor(data$chum[data$chum<20000],data$samples[data$chum<20000]),2))) + 
  scale_x_continuous(breaks=labelsequence) +
  coord_cartesian(expand=FALSE,xlim=c(0,max(data$chum*1.01))) 





dat2 <- data %>% 
  rename(adfg=catcher_vessel_adfg) %>% 
  filter(chum>30 & samples==30) %>% 
  summarise(sum(chum))

data <- read_excel("rcode/Data/psc_all_catch_and_genetics.xlsx",sheet="Genetic BSAI Salmon Bycatch") %>% 
  rename_all(tolower) %>% 
  rename(chum=`sum(number_chum)`,
            samples=`sum(total_chum_finclip)`) %>% 
  filter(samples==0 & chum>30) %>% 
  summarise(sum(chum))




read_excel("rcode/Data/psc_all_catch_and_genetics.xlsx",sheet="Genetic BSAI Salmon Bycatch") %>% 
  rename_all(tolower) %>%
  rename(chum=`sum(number_chum)`,samples=`sum(total_chum_finclip)`) %>% 
  filter(chum>30) %>% 
  mutate(ratio=samples/chum) %>% 
  ggplot(aes(factor(catcher_vessel_adfg),ratio)) + 
  geom_boxplot() + 
  ylim(0,0.1) + 
  geom_hline(yintercept=0.033) + 
  theme(axis.text=element_text(angle=90))




newdata <- read_excel("rcode/Data/psc_all_catch_and_genetics.xlsx",sheet="Genetic BSAI Salmon Bycatch") %>% 
  rename_all(tolower) %>%
  filter(catcher_vessel_adfg%in%dat2$adfg) %>% 
  rename(chum=`sum(number_chum)`,samples=`sum(total_chum_finclip)`) %>% 
  mutate(ratio=samples/chum)

x11()
newdata %>% 
  ggplot(aes(chum,samples)) + 
  geom_point() + 
  facet_wrap(~catcher_vessel_adfg)

newdata %>% 
  ggplot(aes(ratio)) + 
  geom_histogram(binwidth=0.05) + 
  facet_wrap(~catcher_vessel_adfg)

newdata %>% 
  ggplot(aes(factor(week_number),ratio)) + 
  geom_boxplot() + 
  facet_wrap(~catcher_vessel_adfg) + 
  ylim(0,0.1)

newdata %>% 
  filter(ratio<0.03) %>% 
  ggplot(aes(factor(week_number),ratio)) + 
  geom_boxplot() + 
  facet_wrap(~catcher_vessel_adfg,ncol=1)

newdata %>% 
  filter(ratio<0.03) %>% 
  ggplot(aes(chum,samples)) + 
  geom_point() + 
  facet_wrap(~catcher_vessel_adfg)
  
newdata %>% 
  filter(catcher_vessel_adfg==54886) %>% 
  arrange(-chum) %>% 
  dplyr::select(chum,samples)

newdata %>% filter(catcher_vessel_adfg==54886) %>% arrange(-chum) %>% 
  dplyr::select(chum,samples) %>% 
  data.frame

newdata %>% filter(catcher_vessel_adfg==54886) %>% arrange(-chum) %>% 
  dplyr::select(chum,samples,first_name,last_name) %>% 
  mutate(flag=ifelse(samples==0,0,1)) %>% 
  data.frame %>% 
  count(paste(first_name,last_name),flag)


#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
#  Figure 6 GOA PSC by target species ----
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---

#  Create a list of species target codes and labels
targets <- data.frame(trip_target_code=c("A","B","P","C","H","I","K","L","O","S","W","X"),
                      species=c("Atka mackerel","Bottom pollock","Midwater pollock","Pacific cod","GOA shallow-water flatfish","Halibut","Rockfish","Flathead sole","Other species","Sablefish","Arrowtooth flounder","Rex sole"))
gears <- data.frame(gear=1:10,type=c("Non-pelagic trawl","Pelagic Trawl","Mixed Trawl","Pair Trawl","Shrimp Trawl","Pot","Jig","Hook-and-line","Gillnet","Scottish Seine"))

#  Set a threshold such that catches below a certain level become part of the "Other" category.
threshold=0

## 
#  For 2017 bycatch, we opted for a threshold of 100 to create an other category that consolidated target species.
#  The first version here does not include the other category.
## 

#  Read in GOA data
goa <- read_excel("rcode/Data/psc_all_catch_and_genetics.xlsx",sheet="Genetic GOA Salmon Bycatch") %>% 
  rename_all(tolower) %>% 
  group_by(trip_target_code,gear) %>% 
  summarise(chum=sum(`sum(number_nonchinook)`),
            vessels=length(unique(catcher_vessel_adfg))) %>% 
  inner_join(targets) %>% 
  inner_join(gears) %>% 
  filter(chum>0) %>% 
  mutate(species2=ifelse(chum<threshold,"Other",as.character(species))) %>% 
  group_by(type,species2) %>% 
  summarise(Chum=sum(chum),
            vessels=sum(vessels)) %>% 
  filter(vessels>2)

png("Figures/figure6.png",width=6,height=3,units="in",res=300)
goa %>% 
  ggplot(aes(type,Chum,fill=species2)) + 
  geom_bar(stat="identity") +
  theme_bw() + 
  theme(panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black")) +
  scale_fill_manual(values=c(mypalette,"green","grey"),name="") + 
  annotate("text",x=1.5,y=max(goa$Chum),label=paste(this.year,"GOA groundfish fisheries")) + 
  ylab("Number of chum salmon") + 
  theme(axis.title.x=element_blank())
dev.off()


#  The following is a second version that includes an "other category (i.e., threshold > 0). Delete either the above code or this code after
#  choosing whether or not to have an other category.

#  Set a threshold such that catches below a certain level become part of the "Other" category.
threshold=100

#  Read in GOA data
goa <- read_excel("rcode/Data/psc_all_catch_and_genetics.xlsx",sheet="Genetic GOA Salmon Bycatch") %>% 
  rename_all(tolower) %>% 
  group_by(trip_target_code,gear) %>% 
  summarise(chum=sum(`sum(number_nonchinook)`),
            vessels=length(unique(catcher_vessel_adfg))) %>% 
  inner_join(targets) %>% 
  inner_join(gears) %>% 
  filter(chum>0) %>% 
  mutate(species2=ifelse(chum<threshold,"Other",as.character(species))) %>% 
  group_by(type,species2) %>% 
  summarise(Chum=sum(chum),
            vessels=sum(vessels)) %>% 
  filter(vessels>2) %>% 
  data.frame


png("Figures/figure6_threshold100.png",width=6,height=3,units="in",res=300)
goa %>% 
  mutate(species2=fct_relevel(species2,"Other",after=Inf)) %>% 
  ggplot(aes(type,Chum,fill=species2)) + 
  geom_bar(stat="identity") +
  theme_bw() + 
  theme(panel.grid=element_blank(),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black")) +
  scale_fill_manual(values=c(mypalette),name="") + 
  annotate("text",x=1.5,y=max(goa$Chum),label=paste(this.year,"GOA groundfish fisheries")) + 
  ylab("Number of chum salmon") + 
  theme(axis.title.x=element_blank())
dev.off()


#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
#  Figure 7 GOA by season ----
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---

#  Read in GOA data
goa <- read_excel("rcode/Data/psc_all_catch_and_genetics.xlsx",sheet="Genetic GOA Salmon Bycatch") %>% 
  rename_all(tolower) %>% 
  mutate(season=ifelse(between(week_ending_date,goa.a.start,goa.a.end),"A",
                       ifelse(between(week_ending_date,goa.b.start,goa.b.end),"B",
                              ifelse(between(week_ending_date,goa.c.start,goa.c.end),"C",
                                     ifelse(between(week_ending_date,goa.d.start,goa.d.end),"D","Between"))))) %>% 
  group_by(week_number) %>% 
  summarise(chum=sum(`sum(number_nonchinook)`),
            samples=sum(`sum(total_chum_finclip)`),
            vessels=length(unique(catcher_vessel_adfg)),
            season=season[1]) %>% 
  filter(vessels>2) %>% 
  gather(category,counts,-vessels,-week_number,-season)

goa2 <- goa %>% 
  group_by(week_number) %>% 
  summarise(chum=sum(counts),
            samples=sum(counts[category=="samples"])) %>% 
  filter(chum>1) %>% 
  gather(category,counts,-week_number) %>% 
  mutate(category=fct_relevel(category,"samples"))


#  Need to figure out what the tallest bar in the bar plot is going to be so that we can scale the seasonal polygon in the y direction.
mymax <- ceiling(max((goa2 %>% group_by(week_number) %>% summarise(mysum=sum(counts)))$mysum))

#  Create a dataframe that will draw the seasonal polygons
#  There is a tricky step here. First, because we are going to make a barplot (which spans from say week 0.5 to week 1.5 for week 1),
#  we have to adjust the polygons so that the are lagged half a week in either direction. So when you start a season, you have to subtract
#  half a week to make sure that the week polygon begins at the beginning of the bar plot for that week. Otherwise it starts in the middle. 
#  It gets tricky though when one season ends and the next season begins. In such a case, you don't want to lag the start of the second season forward.
#  If you do you get a funky overlap. Instead, we will add half a week to the second season. So the overlap, newx, and terminus bits are all just 
#  figuring out if the next season starts the same week as the previous season, and if it doesn't don't subract to lag the second season, rather add.
datapoly <- data.frame(x=as.numeric(c(awkstart,awkstart,awkend,awkend,
                                      bwkstart,bwkstart,bwkend,bwkend,
                                      cwkstart,cwkstart,cwkend,cwkend,
                                      dwkstart,dwkstart,dwkend,dwkend)),
                       y=rep(c(0,mymax*1.025,mymax*1.025,0),4),
                       terminus=rep(c(rep("start",2),rep("end",2)),4)) %>% 
  mutate(term=sort(rep(letters[1:4],4)),
           week=paste(terminus,term),
         newx=ifelse(week=="start a",x-0.5,
                     ifelse(week=="end a",x+0.5,
                            ifelse(week=="start b" & x[week=="end a"]==x[week=="start b"],x+0.5,
                                   ifelse(week=="start b" & x[week=="end a"]!=x[week=="start b"],x-0.5,
                                          ifelse(week=="end b",x+0.5,
                                                 ifelse(week=="start c" & x[week=="end b"]==x[week=="start c"],x+0.5,
                                                        ifelse(week=="start c" & x[week=="end b"]!=x[week=="start c"],x-0.5,
                                                               ifelse(week=="end c",x+0.5,
                                                                      ifelse(week=="start d" & x[week=="end c"]==x[week=="start d"],x+0.5,
                                                                             ifelse(week=="start d" & x[week=="end c"]!=x[week=="start d"],x-0.5,
                                                                                    x+0.5)))))))))))
 
seasonlab <- datapoly %>% 
  group_by(term) %>% 
  summarise(center=mean(x),
            yval=mymax*0.8) %>% 
  mutate(mylab=LETTERS[1:4])
         

month.labels <- data.frame(month=as.numeric(strftime(seq(as.Date(paste0(this.year,"/1/1")), as.Date(paste0(this.year,"/12/31")), "weeks"),format="%m")),
                           week=as.numeric(strftime(seq(as.Date(paste0(this.year,"/1/1")), as.Date(paste0(this.year,"/12/31")), "weeks"),format="%W"))) %>% 
  filter(week<max(goa$week_number[goa$counts>1])) %>% 
  group_by(month) %>% 
  summarise(center=mean(week)) %>% 
  data.frame %>% 
  mutate(mymonth=month.abb[month]) 

month.poly <- data.frame(x=c(0,0,max(goa$week_number[goa$counts>1])+1,max(goa$week_number[goa$counts>1])+1),
                         y=c(mymax*1.025,mymax*1.075,mymax*1.075,mymax*1.025))

png("Figures/figure7.png",width=6,height=3,units="in",res=300)
ggplot() + 
  geom_polygon(data=month.poly,aes(x=x,y=y),fill="grey60",color="black",size=0.5) + 
  geom_text(data=month.labels,aes(x=center,y=mymax*1.05,label=mymonth),size=3) + 
  geom_polygon(data=datapoly,aes(newx,y),fill="grey90",color="black",size=0.5) +
  geom_bar(data=goa2,aes(week_number,counts,fill=category),stat="identity") +
  geom_text(data=seasonlab,aes(x=center,y=yval,label=mylab),size=4) + 
  theme_bw() + 
  theme(panel.grid=element_blank(),
        legend.position="none",
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black")) +
  coord_cartesian(xlim=c(1,(max(goa$week_number[goa$counts>1])+1)),ylim=c(0,mymax*1.075),expand=FALSE) + 
  scale_fill_manual(values=c(strongred,strongblue)) + 
  scale_x_continuous(breaks=1:(max(goa$week_number[goa$counts>1])+1),labels=mylabel[1:(max(goa$week_number[goa$counts>1])+1)]) + 
  scale_y_continuous(breaks=seq(0,mymax*1.075,by=500),labels=seq(0,mymax*1.075,by=500)) + 
  xlab("Statistical Week") + 
  ylab("Number of chum salmon")
dev.off()

#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
#  Figure 8 GOA by season, area ----
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---

#  Read in GOA data
goa <- read_excel("rcode/Data/psc_all_catch_and_genetics.xlsx",sheet="Genetic GOA Salmon Bycatch") %>% 
  rename_all(tolower) %>% 
  mutate(season=ifelse(between(week_ending_date,goa.a.start,goa.a.end),"A",
                       ifelse(between(week_ending_date,goa.b.start,goa.b.end),"B",
                              ifelse(between(week_ending_date,goa.c.start,goa.c.end),"C",
                                     ifelse(between(week_ending_date,goa.d.start,goa.d.end),"D","Between")))),
         season2=ifelse(week_ending_date<goa.c.start,"Prior to C",
                        ifelse(season%in%c("C","D"),season,NA))) %>% 
  group_by(season2,nmfs_area) %>% 
  summarise(chum=sum(`sum(number_nonchinook)`),
            samples=sum(`sum(total_chum_finclip)`),
            vessels=length(unique(catcher_vessel_adfg))) %>% 
  filter(samples>0 & vessels>2) %>% 
  data.frame


#  Need to figure out if we should put a threshold of minimum number of samples such that 
#  in "Prior to C" period, it would remove 640 and probably 620. Or maybe Prior to C altogether.
#  To see the breakdown in the data run the following:
goa %>% 
  filter(!is.na(season2)) 


png("Figures/figure8.png",width=6,height=3,units="in",res=300)
goa %>% 
  filter(!is.na(season2)) %>% 
  mutate(
    season2=fct_relevel(season2,"Prior to C","C","D")) %>% 
  ggplot(aes(season2,samples,fill=factor(nmfs_area))) + 
  geom_bar(stat="identity") +
  theme_bw() + 
  theme(panel.grid=element_blank(),
        legend.position=c(0.225,0.5),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black")) +
  scale_fill_manual(values=mypalette[1:length(unique(goa$nmfs_area))],name="NMFS Areas") + 
  ylab("Number of genetic samples") + 
  xlab("Gulf of Alaska pollock season")
dev.off()


#  Below we create a second version that includes the previous year for comparison. 

#GOA pollock season 2016 (dates taken from 2016 Figures & Tables for Tech Memo.xlsx, worksheet = pivot goa vessels by area, seaso)
goa.a.start16 <- as.POSIXct(strptime("01/20/2016","%m/%d/%Y"))
goa.a.end16 <- as.POSIXct(strptime("03/09/2016","%m/%d/%Y"))
goa.b.start16 <- as.POSIXct(strptime("03/10/2016","%m/%d/%Y"))
goa.b.end16 <- as.POSIXct(strptime("05/31/2016","%m/%d/%Y"))
goa.c.start16 <- as.POSIXct(strptime("08/25/2016","%m/%d/%Y"))
goa.c.end16 <- as.POSIXct(strptime("09/30/2016","%m/%d/%Y"))
goa.d.start16 <- as.POSIXct(strptime("10/01/2016","%m/%d/%Y"))
goa.d.end16 <- as.POSIXct(strptime("10/31/2016","%m/%d/%Y"))

goa2016 <- read_excel("rcode/Data/2016 Figures & Tables for Tech Memo.xlsx",sheet="GOA AKFIN Salmon Bycatch") %>% 
  rename_all(tolower) %>% 
  mutate(season=ifelse(between(week_ending_date,goa.a.start16,goa.a.end16),"A",
                       ifelse(between(week_ending_date,goa.b.start16,goa.b.end16),"B",
                              ifelse(between(week_ending_date,goa.c.start16,goa.c.end16),"C",
                                     ifelse(between(week_ending_date,goa.d.start16,goa.d.end16),"D","Between")))),
         season2=ifelse(week_ending_date<goa.c.start16,"Prior to C",
                        ifelse(season%in%c("C","D"),season,NA))) %>% 
  group_by(season2,nmfs_area) %>% 
  summarise(chum=sum(`number_nonchinook`),
            samples=sum(`total_chum_finclip`),
            vessels=length(unique(catcher_vessel_adfg))) %>% 
  filter(samples>0 & vessels>2) %>% 
  data.frame

goa.combined <- bind_rows(goa %>% mutate(year=this.year),
                          goa2016 %>% mutate(year=this.year-1))

png("Figures/figure8_2years.png",width=6,height=6,units="in",res=300)
goa.combined %>% 
  filter(!is.na(season2)) %>% 
  mutate(
    season2=fct_relevel(season2,"Prior to C","C","D")) %>% 
  ggplot(aes(season2,samples,fill=factor(nmfs_area))) + 
  geom_bar(stat="identity") +
  theme_bw() + 
  theme(panel.grid=element_blank(),
        legend.position=c(0.225,0.85),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black")) +
  scale_fill_manual(values=mypalette[1:length(unique(goa$nmfs_area))],name="NMFS Areas") + 
  ylab("Number of genetic samples") + 
  facet_wrap(~year,ncol=1) + 
  xlab("Gulf of Alaska pollock season")
dev.off()





#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
#  Figure 10 A-season stock proportion comparison by year----
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---

# Note - In 2017 there were no Aseason results and thus this figure did not happen.


#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
#  Figure 11 Stock proportion across years ----
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---

# Requires "stockdata" from the "Create stock composition file" code chunk above.

props <- read_excel("rcode/Data/psc_all_catch_and_genetics.xlsx",sheet="historic_stock_proportions") %>% 
  gather(stock,proportion,-c(year,totalbycatch))

#  Pull in the 2017 Bayes data for B-season from "stockdata" and "psctotals" objects created previous in this file.
newdat <- stockdata %>% 
  filter(group=="B-season") %>% 
  inner_join(psctotals) %>% 
  mutate(ymin=`2.5%`,
         ymax=`97.5%`) %>% 
  dplyr::select(stock=Region,mymean=Mean,totalbycatch=chum,ymin,ymax) %>% 
  mutate(year=as.character(this.year))

data.early <- props %>% 
  filter(year<2011) %>% 
  group_by(stock) %>% 
  summarise(mymean=mean(proportion),
         se.early=sd(proportion)/sqrt(n()),
         ymin=mymean-se.early,
         ymax=mymean+se.early,
         year="mean 1994,1995,2005-2010") %>% 
  dplyr::select(-se.early)

data.recent <- props %>% 
  filter(between(year,2011,this.year-1)) %>% 
  group_by(stock) %>% 
  summarise(mymean=mean(proportion),
         se.recent=sd(proportion)/sqrt(n()),
         ymin=mymean-se.recent,
         ymax=mymean+se.recent,
         year=paste0("mean 2011-",this.year-1)) %>% 
  dplyr::select(-se.recent)


p1 <- newdat %>% 
  dplyr::select(-totalbycatch) %>% 
  bind_rows(data.early) %>% 
  bind_rows(data.recent) %>% 
  mutate(year=fct_relevel(year,
                          "mean 1994,1995,2005-2010",
                          "mean 2011-2016"),
         stock=fct_recode(stock,
                          "Upper/Middle Yukon"="Up/Mid Yuk",
                          "Eastern GOA/PNW"="E GOA/PNW"),
         stock=fct_relevel(stock,
                          "SE Asia",
                          "NE Asia",
                          "Western AK",
                          "Upper/Middle Yukon",
                          "SW Alaska",
                          "Eastern GOA/PNW")) %>% 
  ggplot(aes(stock,mymean,fill=year,ymin=ymin,ymax=ymax)) + 
  geom_bar(stat="identity",position="dodge",color="black") + 
  scale_fill_manual(values=c(strongred,bluegrey,strongblue),name="") + 
  theme_bw() + 
  theme(axis.text.y=element_text(color="black"),
        axis.title.y=element_text(color="black"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position=c(0.65,0.8),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_blank(),
        panel.grid=element_blank(),
        axis.ticks.x = element_blank()) + 
  geom_errorbar(width=0.2,position=position_dodge(0.9)) + 
  ylab("Stock proportion") 


numbers <- read_excel("rcode/Data/psc_all_catch_and_genetics.xlsx",sheet="historic_psc_numbers") %>% 
  gather(stock,psc,-c(year,totalbycatch))

#  Pull in the 2017 Bayes data for B-season from "stockdata" and "psctotals" objects created previous in this file.
newnum <- stockdata %>% 
  filter(group=="B-season") %>% 
  inner_join(psctotals) %>% 
  mutate(ymin=`2.5%`,
         ymax=`97.5%`) %>% 
  dplyr::select(stock=Region,mymean=Mean,totalbycatch=chum,ymin,ymax) %>% 
  mutate(totalbycatch=totalbycatch/1000,
         year=as.character(this.year),
         mymean=mymean*totalbycatch,
         ymin=ymin*totalbycatch,
         ymax=ymax*totalbycatch)


num.early <- numbers %>% 
  filter(year<2011) %>% 
  group_by(stock) %>% 
  summarise(mymean=mean(psc)/1000,
            se.early=sd(psc/1000)/sqrt(n()),
            ymin=mymean-se.early,
            ymax=mymean+se.early,
            year="mean 1994,1995,2005-2010") %>% 
  dplyr::select(-se.early)

num.recent <- numbers %>% 
  filter(between(year,2011,this.year-1)) %>% 
  group_by(stock) %>% 
  summarise(mymean=mean(psc)/1000,
            se.recent=sd(psc/1000)/sqrt(n()),
            ymin=mymean-se.recent,
            ymax=mymean+se.recent,
            year=paste0("mean 2011-",this.year-1)) %>% 
  dplyr::select(-se.recent)


p2 <- newnum %>% 
  dplyr::select(-totalbycatch) %>% 
  bind_rows(num.early) %>% 
  bind_rows(num.recent) %>% 
  mutate(year=fct_relevel(year,"mean 1994,1995,2005-2010","mean 2011-2016"),
         stock=fct_recode(stock,
                          "Upper/Middle Yukon"="Up/Mid Yuk",
                          "Upper/Middle Yukon"="Up/Mid Yukon",
                          "Eastern GOA/PNW"="E GOA/PNW"),
         stock=fct_relevel(stock,
                           "SE Asia",
                           "NE Asia",
                           "Western AK",
                           "Upper/Middle Yukon",
                           "SW Alaska",
                           "Eastern GOA/PNW")) %>% 
  ggplot(aes(stock,mymean,fill=year,ymin=ymin,ymax=ymax)) + 
  geom_bar(stat="identity",position="dodge",color="black") + 
  scale_fill_manual(values=c(strongred,bluegrey,strongblue),
                    name="",
                    labels=c(paste0("mean 1994,1995,2005-2010 (",formatC(round(1000*sum(num.early$mymean)), format="d", big.mark=","),")"),
                             paste0("mean 2011-2016 (",formatC(round(1000*sum(num.recent$mymean)), format="d", big.mark=","),")"),
                             paste0(this.year," (",formatC(round(1000*newnum$totalbycatch[1]), format="d", big.mark=","),")"))) + 
  scale_x_discrete(labels=function(x) str_wrap(as.character(x),width=12)) + 
  theme_bw() + 
  theme(axis.text=element_text(color="black"),
        axis.title=element_text(color="black"),
        legend.position=c(0.65,0.8),
        legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        panel.grid=element_blank(),
        axis.title.x = element_blank()) + 
  geom_errorbar(width=0.2,position=position_dodge(0.9)) + 
  ylab("Numbers of fish x 1,000")

#  Because the two plots have different numbers of digits in their y-axis scale, the figures don't line up right if you just use grid.arrange. 
#  Instead the key is to use grid.draw with size="last" so that plot two will be scale to match plot 1.
png("Figures/figure11.png",width=6,height=6.5,units="in",res=300)
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))
dev.off()



                