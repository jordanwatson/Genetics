#------------------------------------------------------------
# Explore the number of genetic samples collected for a particular vessel versus the number expected
#
# Author: Jordan Watson
# Creation date: 1/29/2019
#------------------------------------------------------------

library(tidyverse)
library(readxl)
library(viridis)

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
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---



data <- read_excel("Data/2017 Figs & Tables-CK1.xlsx",sheet="Genetic BSAI Salmon Bycatch") %>% 
  dplyr::select(vessel=CATCHER_VESSEL_ADFG, samples=`SUM(TOTAL_CHUM_FINCLIP)`, psc=`SUM(NUMBER_CHUM)`)

data <- data %>% 
  filter(!is.na(vessel)) %>% 
  group_by(vessel) %>% 
  summarise(samples=sum(samples),
            psc=sum(psc),
            expected=round(psc/30),
            missing=expected-samples,
            missing=ifelse(missing<1,0,round(missing)),
            proportion=round(missing/expected,3))

data %>% 
  filter(!is.na(vessel)) %>% 
  ggplot(aes(factor(vessel),missing)) + 
  geom_bar(stat="identity") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90,vjust=0.5)) + 
  xlab("Vessel") + 
  ylab("Missing samples")

data %>% arrange(-missing) %>% data.frame

data %>% 
  filter(!is.na(vessel)) %>% 
  summarise(sum(missing)/sum(expected))

data %>% 
  filter(missing>5) %>% 
  summarise(sum(missing)/sum(expected))

data %>% 
  filter(missing>100) %>% 
  summarise(sum(missing)/sum(expected))

data %>% 
  filter(vessel==57450) %>% 
  summarise(sum(missing)/sum(expected))


p1 <- data %>% 
  rename(Collected=samples,`Not collected`=missing) %>% 
  dplyr::select(vessel,Collected,`Not collected`) %>% 
  gather(Samples,number,-vessel) %>% 
  mutate(Samples=fct_relevel(Samples,"Not collected","Collected")) %>% 
  ggplot(aes(factor(vessel),number,fill=Samples)) + 
  geom_bar(stat="identity",col="white",size=0.25) + 
  theme_bw() + 
  xlab("Vessel") + 
  ylab("Number of expected samples") + 
  scale_fill_manual(values=c(strongred,strongblue))

pdf("Missing_Samples_no_x_labels.pdf",width=10,height=6)
p1 + 
  theme(axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position="top")
dev.off()

pdf("Missing_Samples_with_x_labels.pdf",width=10,height=6)
p1 +
theme(axis.text.x = element_text(angle=90,vjust=0.5,size=7.5),
      panel.grid.major.x = element_blank(),
      legend.position="top")
dev.off()

