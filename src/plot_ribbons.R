library(tidyverse)
library(ggthemes)
library(hrbrthemes)
library(patchwork)

load(file="data/HEAT350_Climate_scenario_data.Rda")

Basins = c("Kattegat",
          "Danish Straits",
          "Arkona Basin",
          "Bornholm Basin",
          "Baltic Proper",
          "Gulf of Riga",
          "Gulf of Finland",
          "Bothnian Sea",
          "Bothnian Bay")

Models <- c("bias0","bias1","Hindcast","modA","modB","modD")
Loads <- c("PEAK","REF","BSAP","HIST")
LoadNames <- c("Peak (1980-1990)","Reference (2012-2014)","BSAP","Historic (1970-2012)")

df <- dfScenarios %>%
  mutate(ER=Value/Target) %>%
  mutate(Load = LoadNames[match(Load,Loads)])

df <- df %>%
  mutate(Drop=ifelse(Scenario=="hindcast",0,ifelse(Year>2018,0,1))) %>%
  filter(Drop==0) %>%
  select(-Drop)


df <- df %>%
  group_by(Scenario,Climate,Model,Load,Year,Basin,CriteriaID,Criteria) %>%
  summarise(ER=mean(ER,na.rm=T)) %>%
  group_by(Scenario,Climate,Model,Load,Year,Basin) %>%
  summarise(ER=max(ER,na.rm=T)) %>%
  ungroup()


df2 <-  df %>%
#  filter(Year>1975) %>%
  filter(Load %in% LoadNames) %>%
  group_by(Load,Year,Basin) %>%
  summarise(ER_avg=mean(ER,na.rm=T),ER_min=min(ER,na.rm=T),ER_max=max(ER,na.rm=T)) %>%
  ungroup() 

df2$Basin <- factor(df2$Basin,levels=Basins)
df2$Load <- factor(df2$Load,levels=LoadNames)

# c("#d7191c","#fdae61","#ffffbf","#abdda4","#2b83ba")
mypal <- c("#d7191c","#abdda4","#2b83ba","#999999")


p1 <- ggplot(data=df2) +
  geom_hline(colour="#FFFFFF",yintercept=0.6,linetype=0,size=0) +
  geom_line(aes(x=Year,y=ER_avg,color=Load),linetype=2) +
  geom_ribbon(aes(x=Year,ymin=ER_min,ymax=ER_max,color=Load,fill=Load),alpha=0.1)  +
  #facet_grid(Basin~Load) 
  facet_wrap(Basin~.,scales="free_y") +
  geom_hline(colour="#333333",yintercept=1,linetype=2,size=1) +
  scale_fill_manual(values=mypal,name="Load Scenario") +
  scale_color_manual(values=mypal,name="Load Scenario") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),
        panel.spacing = unit(1, "lines"))+
        #legend.position="bottom") +
  ylab("Eutrophication Ratio (ER)")
p1


dfBaltic <- df %>%
  group_by(Scenario,Climate,Model,Load,Year) %>%
  summarise(ER=mean(ER,na.rm=T)) %>%
  ungroup() %>%
#  filter(Year>1975 | Scenario!="hindcast") %>%
  filter(Load %in% LoadNames) %>%
  group_by(Load,Year) %>%
  summarise(ER_avg=mean(ER,na.rm=T),ER_min=min(ER,na.rm=T),ER_max=max(ER,na.rm=T)) %>%
  ungroup() 
dfBaltic$Load <- factor(dfBaltic$Load,levels=LoadNames)


p2 <- ggplot(data=dfBaltic) +
  #geom_hline(colour="#FFFFFF",yintercept=0.6,linetype=0,size=0) +
  geom_line(aes(x=Year,y=ER_avg,color=Load),linetype=2) +
  geom_ribbon(aes(x=Year,ymin=ER_min,ymax=ER_max,color=Load,fill=Load),alpha=0.1)  +
  geom_hline(colour="#333333",yintercept=1,linetype=2,size=1) +
  scale_fill_manual(values=mypal,name="Load Scenario") +
  scale_color_manual(values=mypal,name="Load Scenario") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),
        panel.spacing = unit(1, "lines"))+
  ylab("Eutrophication Ratio (ER)")
p2

p <- p1 / p2 + plot_layout(heights=c(2,1))
p

ggsave("figures/ER_Baltic.png",p2,dpi=300,units="cm",width=10,height=6,scale=1.5)
ggsave("figures/ER_Basin.png",p1,dpi=300,units="cm",width=21,height=14,scale=1.5)
ggsave("figures/ER_Baltic+Basins.png",p,dpi=300,units="cm",width=21,height=21,scale=1.5)



