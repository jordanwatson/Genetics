library(lubridate)
library(tidyverse)
library(readxl)
library(broom)



#tempdat <- readRDS("rcode/Data/mur_SST_stat6_all_columns.rds") %>% 
#  filter(NMFSAREA%in%c(517,519,521) & month%in%c(6,7,8))

#tempdat <- readRDS("rcode/Data/mur_SST_stat6_all_columns.rds") %>% 
#  filter(NMFSAREA%in%c(517,519,521) & month%in%c(6,7,8))

tempdat <- readRDS("rcode/Data/temperature_data.RDS") %>% 
  inner_join(readRDS("rcode/Data/spatial_data.RDS") %>% dplyr::select(STAT_AREA,NMFSAREA)) %>% 
  mutate(month=month(as.POSIXlt(date, format="%d/%m/%Y")),
         year=year(as.POSIXlt(date, format="%d/%m/%Y"))) %>% 
  filter(NMFSAREA%in%c(517,519,521) & month%in%c(6,7,8))

#tempdat <- readRDS("rcode/Data/temperature_data.RDS") %>% 
##  inner_join(readRDS("rcode/Data/spatial_data.RDS") %>% dplyr::select(STAT_AREA,NMFSAREA)) %>% 
#  mutate(month=month(as.POSIXlt(date, format="%d/%m/%Y")),
#         year=year(as.POSIXlt(date, format="%d/%m/%Y"))) %>% 
#  filter(NMFSAREA%in%c(610) & month%in%c(6,7,8))

data <- read_excel("rcode/Data/psc_summary.xlsx",sheet="chum") %>% 
  rename_all(tolower) %>% 
  mutate(psc=as.numeric(psc)) %>% 
  filter(!is.na(psc))

datsum <- tempdat %>% 
  group_by(year) %>% 
  summarise(sst=mean(sst.mean)) %>% 
  inner_join(data) %>% 
  mutate(psc10=ifelse(year>2010,psc*1.2,psc))

pdf("Chum_vs_temp.pdf")
datsum %>% 
  ggplot(aes(sst,psc)) + 
  geom_text(aes(label=year)) +
  geom_smooth(method="lm") + 
  theme_bw() + 
  ylab("Chum salmon psc")
dev.off()

png("Chum_vs_temp.png")
datsum %>% 
  ggplot(aes(sst,psc)) + 
  geom_text(aes(label=year)) +
  geom_smooth(method="lm") + 
  theme_bw() + 
  ylab("Chum salmon psc")
dev.off()

cor(datsum$psc,datsum$sst)
cor(datsum$psc[datsum$year<2011],datsum$sst[datsum$year<2011])
cor(datsum$psc[datsum$year>=2011],datsum$sst[datsum$year>=2011])
cor(datsum$psc10,datsum$sst)

summary(lm(psc~sst,data=datsum))
summary(lm(psc~sst,data=datsum[datsum$year<2011,]))
summary(lm(psc~sst,data=datsum[datsum$year>=2011,]))


datsum %>% 
  ggplot(aes(sst,psc)) + 
  geom_text(aes(label=year)) +
  geom_abline(intercept=-1808500,slope=260906) + 
  geom_abline(intercept=-438432,slope=80469,linetype=2) + 
  theme_bw() + 
  ylab("Chum salmon psc")


#-----------------------------------------------------------------
data <- read_excel("rcode/Data/psc_summary.xlsx",sheet="chinook") %>% 
  rename_all(tolower) %>% 
  mutate(psc=as.numeric(psc)) %>% 
  filter(!is.na(psc))

datsum <- tempdat %>% 
  group_by(year) %>% 
  summarise(sst=mean(sst.mean)) %>% 
  inner_join(data) %>% 
  mutate(psc10=ifelse(year>2010,psc*1.2,psc))

pdf("Chinook_vs_temp.pdf")
datsum %>% 
  ggplot(aes(sst,psc)) + 
  geom_text(aes(label=year)) +
  geom_smooth(method="lm") + 
  theme_bw() + 
  ylab("Chinook salmon psc")
  dev.off()

png("Chinook_vs_temp.png")
datsum %>% 
  ggplot(aes(sst,psc)) + 
  geom_text(aes(label=year)) +
  geom_smooth(method="lm") + 
  theme_bw() + 
  ylab("Chinook salmon psc")
dev.off()

cor(datsum$psc,datsum$sst)
cor(datsum$psc[datsum$year<2011],datsum$sst[datsum$year<2011])
cor(datsum$psc[datsum$year>=2011],datsum$sst[datsum$year>=2011])

summary(lm(psc~sst,data=datsum))
summary(lm(psc~sst,data=datsum[datsum$year<2011,]))
summary(lm(psc~sst,data=datsum[datsum$year>=2011,]))



#------------------------------------------------------------------------

#-----------------------------------------------------------------
data <- read_excel("rcode/Data/psc_summary.xlsx",sheet="sector") %>% 
  rename_all(tolower) %>% 
  mutate(psc=as.numeric(psc)) %>% 
  filter(!is.na(psc))

datsum <- tempdat %>% 
  group_by(year) %>% 
  summarise(sst=mean(sst.mean)) %>% 
  inner_join(data)

pdf("Chinook_vs_temp_sector.pdf")
datsum %>% 
  ggplot(aes(sst,psc)) + 
  geom_text(aes(label=year)) +
  geom_smooth(method="lm") + 
  theme_bw() + 
  ylab("Salmon psc") + 
  facet_wrap(species~sector,scales="free_y")
dev.off()

png("Salmon_vs_temp_sector.png",width=11,height=8,units="in",res=300)
datsum %>% 
  ggplot(aes(sst,psc)) + 
  geom_text(aes(label=year)) +
  geom_smooth(method="lm") + 
  theme_bw() + 
  ylab("Salmon psc") + 
  facet_wrap(species~sector,scales="free_y")
dev.off()


pdf("Salmon_vs_temp_sector.pdf")
datsum %>% 
  filter(species=="chinook") %>% 
  ggplot(aes(sst,psc)) + 
  geom_text(aes(label=year)) +
  geom_smooth(method="lm") + 
  theme_bw() + 
  ylab("Chinook salmon psc") + 
  facet_wrap(~sector)
dev.off()


datsum %>% 
  group_by(species,sector) %>% 
  do(tidy(lm(psc~sst,data=.))) %>% 
  filter(term!="(Intercept)") %>% 
  inner_join(datsum %>% 
               group_by(species,sector) %>% 
               do(glance(lm(psc~sst,data=.))) %>% 
               dplyr::select(species,sector,adj.r.squared))



#------------------------------------------------------------------------------------------
#  Iterate through different months
#------------------------------------------------------------------------------------------

tempdat <- readRDS("rcode/Data/temperature_data.RDS") %>% 
  inner_join(readRDS("rcode/Data/spatial_data.RDS") %>% dplyr::select(STAT_AREA,NMFSAREA)) %>% 
  mutate(month=month(as.POSIXlt(date, format="%d/%m/%Y")),
         year=year(as.POSIXlt(date, format="%d/%m/%Y"))) %>% 
  filter(NMFSAREA%in%c(517,519,521))

data <- read_excel("rcode/Data/psc_summary.xlsx",sheet="chum") %>% 
  rename_all(tolower) %>% 
  mutate(psc=as.numeric(psc)) %>% 
  filter(!is.na(psc))

#  Need to add the monthly filter here
datsum <- tempdat %>% 
  group_by(year,month) %>% 
  summarise(sst=mean(sst.mean)) %>% 
  inner_join(data)

datsum <- tempdat %>% 
  filter(month%in%c(6,7,8)) %>% 
  group_by(year) %>% 
  summarise(sst=mean(sst.mean)) %>% 
  inner_join(data)

datsum <- tempdat %>% 
  filter(month%in%c(6,7)) %>% 
  group_by(year) %>% 
  summarise(sst=mean(sst.mean)) %>% 
  inner_join(data)

datsum %>% 
  do(glance(lm(psc~sst,data=.))) %>% 
  dplyr::select(adj.r.squared)

datsum %>% 
  group_by(month) %>% 
  do(glance(lm(psc~sst,data=.))) %>% 
  dplyr::select(adj.r.squared)

datsum %>% 
  group_by(month) %>% 
  do(glance(lm(psc~sst,data=.[.$year<2011,]))) %>% 
  dplyr::select(adj.r.squared)

datsum %>% 
  do(glance(lm(psc~sst,data=.[.$year<2011,]))) %>% 
  dplyr::select(adj.r.squared)
