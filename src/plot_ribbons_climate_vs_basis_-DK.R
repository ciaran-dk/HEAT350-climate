library(tidyverse)
library(ggthemes)
library(hrbrthemes)
library(patchwork)
library(scales)

load(file="data/HEAT350_Climate_scenario_data.Rda")

Basins <- c("Kattegat",
          "Danish Straits",
          "Arkona Basin",
          "Bornholm Basin",
          "Baltic Proper",
          "Gulf of Riga",
          "Gulf of Finland",
          "Bothnian Sea",
          "Bothnian Bay")

Basins <- Basins[!Basins %in% c("Kattegat","Danish Straits")]

Models <- c("bias0","bias1","Hindcast","modA","modB","modD")
Loads <- c("PEAK","REF","BSAP","HIST")
LoadNames <- c("Peak (1980-1990)","Reference (2012-2014)","BSAP","Historic (1970-2012)")


df <- dfScenarios %>%
  mutate(ER=Value/Target) %>%
  mutate(Load = LoadNames[match(Load,Loads)])

df <- df %>%
  filter(Basin %in% Basins)

x_axis_text_angle<-0
 # df <- df %>%
 #   mutate(Drop=ifelse(Scenario=="hindcast",0,ifelse(Year>1999,0,1))) %>%
 #   filter(Drop==0) %>%
 #   select(-Drop)


df <- df %>%
  group_by(Scenario,Climate,Model,Load,Year,Basin,CriteriaID,Criteria) %>%
  summarise(ER=mean(ER,na.rm=T)) %>%
  group_by(Scenario,Climate,Model,Load,Year,Basin) %>%
  summarise(ER=max(ER,na.rm=T)) %>%
  ungroup()

df <- df %>%
  mutate(ClimateModel=ifelse(Model=="mod0","Present","Future"))

mypal <- c("#d7191c","#abdda4","#2b83ba","#999999")

df$Load <- factor(df$Load,levels=LoadNames)
df$ClimateModel <- factor(df$ClimateModel,levels=c("Present","Future"))
df$Basin <- factor(df$Basin,levels=Basins)


dfBaltic <- df %>%
  group_by(Scenario,ClimateModel,Load,Year) %>%
  summarise(ER=mean(ER,na.rm=T)) %>%
  ungroup() %>%
  group_by(ClimateModel,Load,Year) %>%
  summarise(ER_avg=mean(ER,na.rm=T),ER_min=min(ER,na.rm=T),ER_max=max(ER,na.rm=T)) %>%
  ungroup() 


climate_scenario_name <- c(
  Present="Present climate",
  Future="Future climate"
)


p <- ggplot(data=dfBaltic) +
  geom_ribbon(aes(x=Year,ymin=ER_min,ymax=ER_max,color=Load,fill=Load),alpha=0.2, linetype="blank")  +
  geom_line(aes(x=Year,y=ER_avg,color=Load),linetype=1) +
  geom_hline(colour="#333333",yintercept=1,linetype=2,size=1) +
  scale_fill_manual(values=mypal,name="Load Scenario") +
  scale_color_manual(values=mypal,name="Load Scenario") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=x_axis_text_angle,vjust=0.5,hjust=1),
        panel.spacing = unit(1, "lines"),
        legend.position="bottom",
        plot.margin = unit(c(0,0,0,0),units="cm")) +
  coord_cartesian(expand=FALSE) +
  ylab("Eutrophication Ratio (ER)") +
  facet_wrap(.~ClimateModel,ncol=1, labeller=labeller(ClimateModel=climate_scenario_name))
p
ggsave("figures/ER_Baltic_Climate+Present.png",p,dpi=300,units="cm",width=12,height=12,scale=1.5)
ggsave("figures/ER_Baltic_Climate+Present_100dpi.png",p,dpi=100,units="cm",width=12,height=12,scale=1)


dfBasins <- df %>%
  group_by(Basin,Scenario,ClimateModel,Load,Year) %>%
  summarise(ER=mean(ER,na.rm=T)) %>%
  ungroup() %>%
  group_by(Basin,ClimateModel,Load,Year) %>%
  summarise(ER_avg=mean(ER,na.rm=T),ER_min=min(ER,na.rm=T),ER_max=max(ER,na.rm=T)) %>%
  ungroup() 

dfplot <- dfBaltic %>%
  mutate(Basin="Baltic Sea") %>%
  bind_rows(dfBasins)

dfplot <- dfplot %>%
  filter(Load %in% c("BSAP"))
dfplot$ClimateModel <- factor(dfplot$ClimateModel,levels=c("Future","Present"))
dfplot$Basin <- factor(dfplot$Basin,levels=c(Basins,"Baltic Sea"))




mypal2 <- c("#999999","#2b83ba")


p1a <- ggplot(data=filter(dfplot,Basin!="Baltic Sea")) +
  geom_ribbon(aes(x=Year,ymin=ER_min,ymax=ER_max,color=ClimateModel,fill=ClimateModel),alpha=0.2, linetype="blank")  +
  geom_line(aes(x=Year,y=ER_avg,color=ClimateModel),linetype=1) +
  geom_hline(colour="#333333",yintercept=1,linetype=2,size=1) +
  scale_fill_manual(values=mypal2,name="Climate scenario") +
  scale_color_manual(values=mypal2,name="Climate scenario") +
  guides(color=guide_legend(reverse=TRUE),fill=guide_legend(reverse=TRUE)) +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=x_axis_text_angle,vjust=0.5,hjust=1),
        #strip.text.x = element_text(size=7),
        panel.spacing = unit(1, "lines"), 
        legend.position = "bottom",
        plot.margin = unit(c(0,0,0,0),units="cm")) +
  ylab("Eutrophication Ratio (ER)") +
  coord_cartesian(expand=FALSE) +
  facet_wrap(.~Basin,ncol=2,scales="free_y")
p1a

p1b <- ggplot(data=filter(dfplot,Basin=="Baltic Sea")) +
  labs(subtitle="Baltic Sea") + 
  geom_ribbon(aes(x=Year,ymin=ER_min,ymax=ER_max,color=ClimateModel,fill=ClimateModel),alpha=0.2, linetype="blank")  +
  geom_line(aes(x=Year,y=ER_avg,color=ClimateModel),linetype=1) +
  geom_hline(colour="#333333",yintercept=1,linetype=2,size=1) +
  scale_fill_manual(values=mypal2,name="Climate scenario") +
  scale_color_manual(values=mypal2,name="Climate scenario") +
  guides(color=guide_legend(reverse=TRUE),fill=guide_legend(reverse=TRUE)) +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=x_axis_text_angle,vjust=0.5,hjust=1),
        #strip.text.x = element_text(size=7),
        panel.spacing = unit(1, "lines"), legend.position = "bottom",
        plot.margin = unit(c(0,0,0,0),units="cm")) +
  coord_cartesian(expand=FALSE) +
  ylab("Eutrophication Ratio (ER)")
p1b
p1 <-  p1a +p1b + plot_layout(ncol=1,nrow=2, heights=c(4,2))
#p1 + theme(legend.position = "bottom")
ggsave("figures/ER_BSAP_Climate+Present.png",p1,dpi=300,units="cm",width=12,height=17,scale=1.5)
ggsave("figures/ER_BSAP_Climate+Present_100dpi.png",p1,dpi=100,units="cm",width=20,height=28,scale=1)



p1x <- ggplot(data=filter(dfplot,Basin=="Baltic Sea")) +
  geom_ribbon(aes(x=Year,ymin=ER_min,ymax=ER_max,color=ClimateModel,fill=ClimateModel),alpha=0.2, linetype="blank")  +
  geom_line(aes(x=Year,y=ER_avg,color=ClimateModel),linetype=1) +
  geom_hline(colour="#333333",yintercept=1,linetype=2,size=1) +
  scale_fill_manual(values=mypal2,name="Climate scenario") +
  scale_color_manual(values=mypal2,name="Climate scenario") +
  guides(color=guide_legend(reverse=TRUE),fill=guide_legend(reverse=TRUE)) +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=x_axis_text_angle,vjust=0.5,hjust=1),
        legend.margin=margin(0.1,0.1,0.1,0.1,unit='cm'),
        legend.direction = "horizontal",
        legend.justification = "left",
        legend.position = c(0.15, 0.9),
        panel.spacing = unit(1, "lines"),
        plot.margin = unit(c(0,0,0,0),units="cm"),
        legend.background = element_rect(fill = "#FFFFFF", color="#999999")) +
  coord_cartesian(expand=FALSE,xlim=c(2010,2100)) +
  scale_x_continuous(breaks=pretty_breaks(n=5)) +
  scale_y_continuous(breaks=pretty_breaks()) +
  ylab("Eutrophication Ratio (ER)") 
p1x

ggsave("figures/ER_BSAP_Climate+Present_Baltic.png",p1x,dpi=300,units="cm",width=15.95,height=7,scale=1)


# dfBasins<- df %>%
#   group_by(Basin,Scenario,ClimateModel,Load,Year) %>%
#   summarise(ER=mean(ER,na.rm=T)) %>%
#   ungroup() %>%
#   group_by(Basin,ClimateModel,Load,Year) %>%
#   summarise(ER_avg=mean(ER,na.rm=T),ER_min=min(ER,na.rm=T),ER_max=max(ER,na.rm=T)) %>%
#   ungroup() 


p2 <- ggplot(data=dfBasins) +
  geom_ribbon(aes(x=Year,ymin=ER_min,ymax=ER_max,color=Load,fill=Load),alpha=0.2,linetype="blank")  +
  geom_line(aes(x=Year,y=ER_avg,color=Load),linetype=1) +
  geom_hline(colour="#333333",yintercept=1,linetype=2,size=1) +
  scale_fill_manual(values=mypal,name="Load scenario") +
  scale_color_manual(values=mypal,name="Load scenario") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=x_axis_text_angle,vjust=0.5,hjust=1),
        panel.spacing = unit(1, "lines"),
        legend.position="bottom",
        plot.margin = unit(c(0,0,0,0),units="cm")) +
  ylab("Eutrophication Ratio (ER)") +
  facet_grid(Basin~ClimateModel, scales="free_y")
p2

ggsave("figures/ER_Basins_Climate+Present.png",p2,dpi=300,units="cm",width=13,height=30,scale=1.5)
ggsave("figures/ER_Basins_Climate+Present_100dpi.png",p2,dpi=100,units="cm",width=13,height=30,scale=1)












