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

#Models <- c("bias0","bias1","Hindcast","modA","modB","modD")
Models <- c("mod0","modA","modB","modD")
Loads <- c("PEAK","REF","BSAP","HIST")
LoadNames <- c("Peak (1980-1990)","Reference (2012-2014)","BSAP","Historic (1970-2012)")
climatemodels<-c("Hindcast", "present0", "present1", "rcp45_modB", "rcp45_modA", "rcp45_modD", "rcp85_modB", "rcp85_modA", "rcp85_modD")

climatemodels<-c("random00_mod0","random22_mod0","random44_mod0","rcp452_modA","rcp852_modA","rcp452_modB",
                 "rcp852_modB","rcp452_modD","rcp852_modD")

dfPresentBasins <- dfScenarios %>%
  filter(Model %in% c("mod0")) %>%
  select(Climate,ParameterRes,Basin,Year,Load,Value,Target, Unit,CriteriaID,Criteria) %>%
  mutate(ER=ifelse(ParameterRes %in% c("Secchi_sum"),Target/Value,Value/Target)) %>%
  group_by(ParameterRes,Basin,Year,Load) %>%
  summarise(ER_min=min(ER,na.rm=T),ER_max=max(ER,na.rm=T),ER_avg=mean(ER,na.rm=T)) %>%
  mutate(ER_min=ifelse(is.finite(ER_min),ER_min,NA),
         ER_max=ifelse(is.finite(ER_max),ER_max,NA),
         ER_min=ifelse(is.nan(ER_min),NA,ER_min),
         ER_max=ifelse(is.nan(ER_max),NA,ER_max))

dfPresentBaltic <- dfScenarios %>%
  filter(Model %in% c("mod0")) %>%
  select(Climate,ParameterRes,Basin,Year,Load,Value,Target, Unit,CriteriaID,Criteria) %>%
  mutate(ER=ifelse(ParameterRes %in% c("Secchi_sum"),Target/Value,Value/Target)) %>%
  group_by(Climate,ParameterRes,Year,Load) %>%
  summarise(ER=mean(ER,na.rm=T)) %>%
  ungroup() %>%
  group_by(ParameterRes,Year,Load) %>%
  summarise(ER_min=min(ER,na.rm=T),ER_max=max(ER,na.rm=T),ER_avg=mean(ER,na.rm=T)) %>%
  mutate(Basin="Baltic Sea") %>%
  mutate(ER_min=ifelse(is.finite(ER_min),ER_min,NA),
         ER_max=ifelse(is.finite(ER_max),ER_max,NA),
         ER_min=ifelse(is.nan(ER_min),NA,ER_min),
         ER_max=ifelse(is.nan(ER_max),NA,ER_max))

dfPresent <- dfPresentBasins %>%
  bind_rows(dfPresentBaltic)

dfCCBasins <- dfScenarios %>%
  filter(!Model %in% c("mod0")) %>%
  mutate(ClimateModel=paste0(Climate,"_",Model)) %>%
  mutate(ER=ifelse(ParameterRes %in% c("Secchi_sum"),Target/Value,Value/Target)) %>%
  select(ClimateModel,ParameterRes,Load,Basin,Year,ER) %>%
  arrange(ParameterRes,Basin,Year,ClimateModel,Load)

dfCCBasinsAvg <- dfScenarios %>%
  filter(!Model %in% c("mod0")) %>%
  group_by(ParameterRes,Load,Basin,Year,Target) %>%
  summarise(Value=mean(Value,na.rm=T)) %>%
  ungroup() %>%
  mutate(ER=ifelse(ParameterRes %in% c("Secchi_sum"),Target/Value,Value/Target)) %>%
  mutate(ClimateModel="Climate avg") %>%
  select(ClimateModel,ParameterRes,Load,Basin,Year,ER)
  
dfCCBasins <-  dfCCBasins %>% 
  bind_rows(dfCCBasinsAvg)

dfCCBaltic <- dfCCBasins %>%
  group_by(ClimateModel,ParameterRes,Load,Year) %>%
  summarise(ER=mean(ER,na.rm=T)) %>%
  ungroup() %>%
  mutate(Basin="Baltic Sea")

dfCC <-  dfCCBasins %>% 
  bind_rows(dfCCBaltic)


dfCC <-  dfCC %>%
  left_join(dfPresent,by=c("ParameterRes","Load","Basin","Year")) %>%
  filter(Load=="BSAP")


dfCC$ClimateModel <- factor(dfCC$ClimateModel,
                                  levels=c("rcp452_modA","rcp852_modA","rcp452_modB",
                                           "rcp852_modB","rcp452_modD","rcp852_modD",
                                           "Climate avg"))

dfCC$Basin <- factor(dfCC$Basin,levels=c(Basins,"Baltic Sea"))

params<-c("DIN_win_surf","DIP_win_surf","Chla_sum_surf","Secchi_sum","Oxdebt")


mypal <- c("#d7191c","#abdda4","#2b83ba")

i<-5
for(i in 1:length(params)){
  dfplot <- dfCC %>% filter(ParameterRes==params[i])
  
  if(i==5){
    subt <- "(Only assessed in Bornholm Basin, Baltic Proper, Gulf of Finland)"
  }else{
    subt <- ""
  }
  
  p <- ggplot(data=dfplot) +
    geom_hline(colour="#FFFFFF",yintercept=0.6,linetype=0,size=0) +
    geom_ribbon(aes(x=Year,ymin=ER_min,ymax=ER_max),color=NA,fill="#abdda4",alpha=1)  +
    geom_line(aes(x=Year,y=ER),colour="#d7191c",linetype=1,alpha=0.7) +
    geom_hline(colour="#333333",yintercept=1,linetype=2,size=0.1) +
    facet_grid(Basin~ClimateModel,scales="free_y") +
    theme_ipsum(base_size=9) + 
    ggtitle(label=paste0(params[i]," (ER)"),subtitle=subt) +
    ylab(label=element_text(paste0("Eutrophication Ratio"),size=4)) +
    labs(caption ="Six future climate models and their average (red) compared with range of 3 present climate models (green)") +
    theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),
          panel.spacing = unit(1, "lines"),legend.position = "bottom",
          plot.title=element_text(size=12),
          plot.subtitle=element_text(size=9),
          strip.text=element_text(size=9),
          legend.text=element_text(size=9),
          plot.caption = element_text(size=9)) 

  # p
  ggsave(paste0("figures/compare_climate_basis_ER_",i,".pdf"),device=cairo_pdf,p,dpi=600,units="cm",width=21,height=29.7,scale=1)

  if(i==1){
    pp <- p
  }else{
    pp <- pp + p
  }
}
