library(tidyverse)

strongred <- rgb(215,48,39,max=255)
salmon <- rgb(252,141,89,max=255)
yellow <- rgb(254,224,144,max=255)
#steelblue <- rgb(224,243,248,max=255)
periwinkle <- rgb(181,63,250,max=255)
bluegrey <- rgb(145,191,219,max=255)
strongblue <- rgb(69,117,180,max=255)

mypalette <- c(strongred,salmon,yellow,bluegrey,strongblue,periwinkle)

stockcomp <- read_csv("rcode/Data/stock_age_cluster_time.csv") %>% 
  mutate(Region=fct_relevel(Region,
                            "SE Asia",
                            "NE Asia",
                            "Western AK",
                            "Up/Mid Yukon",
                            "SW Alaska",
                            "E GOA/PNW")) %>% 
  rename(ymin=`X2.5.`,ymax=`X97.5.`)


stockcomp %>% 
  filter(cluster%in%c(1,2,3,4) & time=="All year") %>% 
  ggplot(aes(Region,Mean)) + 
  geom_bar(stat="identity",fill=strongblue) + 
  theme_bw() + 
  facet_grid(paste("Cluster",cluster)~paste("Age",age)) + 
  theme(axis.text.x = element_text(size=8,angle=45,vjust=0.75))

png("Figures/figure_age_by_cluster.png",width=6.5,height=8,units="in",res=300)
stockcomp %>% 
  filter(cluster%in%c(1,2,3,4,"All") & time=="All year") %>% 
  right_join(expand.grid(cluster=as.character(c(1:4,"All")),age=3:5,Region=unique(stockcomp$Region),group=unique(stockcomp$group))) %>% 
  mutate(cluster=ifelse(cluster%in%1:4,paste("Cluster",cluster),paste(cluster,"clusters"))) %>% 
  ggplot(aes(Region,Mean,ymin=ymin,ymax=ymax,fill=factor(age))) + 
  geom_bar(stat="identity",position="dodge") + 
  geom_errorbar(width=0.2,position=position_dodge(0.9),size=0.25) + 
  theme_bw() + 
  facet_wrap(~cluster,ncol=1) + 
  theme(axis.title.x = element_blank(),
        axis.text = element_text(face = "bold"),
        legend.justification = c(0,1), 
        legend.position = c(0.325,1.1),
        plot.margin=unit(c(1.25,0.25,0.25,0.25),"cm")) + 
  scale_fill_manual(values=mypalette,
                    name="Age")  +
  guides(fill=guide_legend(ncol=4,title.position = "left")) + 
  ylab("Stock proportion")
dev.off()

#  Plot the Are 1-2 Early and Late
png("Figures/figure_cluster1_2_age_period.png",width=6.5,height=4,units="in",res=300)
stockcomp %>% 
  filter(cluster%in%c("1-2") & time%in%c("Early","Late")) %>% 
  right_join(expand.grid(age=3:5,Region=unique(stockcomp$Region),group=unique(stockcomp$group[stockcomp$cluster=="1-2"]))) %>% 
  mutate(cluster="1-2",
         time=ifelse(substr(group,12,12)=="E","Early","Late")) %>% 
  ggplot(aes(Region,Mean,fill=factor(age),ymin=ymin,ymax=ymax)) + 
  geom_bar(stat="identity",position="dodge") + 
  geom_errorbar(width=0.2,position=position_dodge(0.9),size=0.25) + 
  theme_bw() + 
  facet_wrap(~paste0(time,", Clusters 1-2"),ncol=1) + 
  theme(axis.title.x = element_blank(),
        axis.text = element_text(face = "bold"),
        legend.justification = c(0,1), 
        legend.position = c(0.325,1.225),
        plot.margin=unit(c(1.25,0.25,0.25,0.25),"cm")) + 
  scale_fill_manual(values=mypalette,
                    name="Age")  +
  guides(fill=guide_legend(ncol=4,title.position = "left")) + 
  ylab("Stock proportion")
dev.off()


png("Figures/figure_cluster1_2_age_period_transpose.png",width=6.5,height=6,units="in",res=300)
stockcomp %>% 
  filter(cluster%in%c("1-2") & time%in%c("Early","Late")) %>% 
  right_join(expand.grid(age=3:5,Region=unique(stockcomp$Region),group=unique(stockcomp$group[stockcomp$cluster=="1-2"]))) %>% 
  mutate(cluster="1-2",
         time=ifelse(substr(group,12,12)=="E","Early","Late")) %>% 
  ggplot(aes(Region,Mean,fill=factor(time),ymin=ymin,ymax=ymax)) + 
  geom_bar(stat="identity",position="dodge") + 
  geom_errorbar(width=0.2,position=position_dodge(0.9),size=0.25) + 
  theme_bw() + 
  facet_wrap(~paste("Age",age),ncol=1) + 
  theme(axis.title.x = element_blank(),
        axis.text = element_text(face = "bold"),
        legend.justification = c(0,1), 
        legend.position = c(0.25,1.15),
        plot.margin=unit(c(1.2,0.25,0.25,0.25),"cm")) + 
  scale_fill_manual(values=mypalette,
                    name="Time period")  +
  guides(fill=guide_legend(ncol=4,title.position = "left")) + 
  ylab("Stock proportion")
dev.off()