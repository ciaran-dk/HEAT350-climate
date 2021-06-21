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
climatemodels<-c("Hindcast", "present0", "present1", "rcp45_modB", "rcp45_modA", "rcp45_modD", "rcp85_modB", "rcp85_modA", "rcp85_modD")



dfCC1 <- dfScenarios %>%
  filter(Model %in% c("Hindcast","bias0","bias1")) %>%
  select(Model,ParameterRes,Basin,Year,Load,Value,Unit) %>%
  arrange(ParameterRes,Basin,Year,Load,Model)

dfCC2 <- dfScenarios %>%
  filter(!Model %in% c("Hindcast","bias0","bias1")) %>%
  mutate(ClimateModel=paste0(Climate,"_",Model)) %>%
  select(ClimateModel,ParameterRes,Load,Basin,Year,Value,Unit) %>%
  arrange(ParameterRes,Basin,Year,ClimateModel,Load)

dfCC3 <- dfCC2 %>% 
  select(ClimateModel,ParameterRes,Load,Basin,Year) %>%
  merge(dfCC1,by=c("ParameterRes","Load","Basin","Year"))

dfCC <- dfCC2 %>%
  mutate(Model="Climate") %>%
  bind_rows(dfCC3)




df <- dfScenarios %>%
  filter(Basin=="Baltic Proper") %>%
  filter(Parameter %in% c("Chla","O2debt")) %>%
  filter(Load=="BSAP") %>%
  mutate(Load = LoadNames[match(Load,Loads)]) 


df <-  df %>%
  mutate(ClimateModel=ifelse(Model %in% c("Hindcast","bias0","bias1"),
                             Model,
                             paste0(Climate,"_",Model)))






df$ParameterRes <- factor(df$ParameterRes,levels=unique(df$ParameterRes))


df0 <- df %>%
  filter(ClimateModel %in% c("Hindcast","bias0","bias1")) %>%
  select(Model,ParameterRes,Year,Value)
  
df1 <- df %>%
  filter(!ClimateModel %in% c("Hindcast","bias0","bias1"))  %>%
  select(ClimateModel,ParameterRes,Year,Value)

df0$Model <- factor(df0$Model,levels=c("Hindcast","bias0","bias1"))
df1$ClimateModel <- factor(df1$ClimateModel,levels=c("rcp45_modA","rcp45_modB","rcp45_modD","rcp85_modA","rcp85_modB","rcp85_modD"))

#mypal <- c("#d7191c","#abdda4","#2b83ba","#999999")
mypal <- c("#999999","#AAAAAA","#BBBBBB")

p1 <- ggplot(data=df1) +
  geom_line(data=df0,aes(x=Year,y=Value,color=Model),linetype=1) +
  geom_line(aes(x=Year,y=Value),linetype=1,color="#2b83ba",size=1) +
  facet_wrap(ParameterRes~ClimateModel,scales="free_y",ncol=3) +
  scale_color_manual(values=mypal,name="Load Scenario") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),
        panel.spacing = unit(1, "lines"), legend.position="none")
p1
