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

dfCC1 <- dfScenarios %>%
  #filter(Model %in% c("Hindcast","bias0","bias1")) %>%
  filter(Model %in% c("mod0")) %>%
  select(Model,ParameterRes,Basin,Year,Load,Value,Target,Unit,CriteriaID,Criteria) %>%
  arrange(ParameterRes,Basin,Year,Load,Model)

dfCC2 <- dfScenarios %>%
  #filter(!Model %in% c("Hindcast","bias0","bias1")) %>%
  filter(!Model %in% c("mod0")) %>%
  mutate(ClimateModel=paste0(Climate,"_",Model)) %>%
  select(ClimateModel,ParameterRes,Load,Basin,Year,Value,Target,Unit,CriteriaID,Criteria) %>%
  arrange(ParameterRes,Basin,Year,ClimateModel,Load)

dfCC3 <- dfCC2 %>% 
  select(ClimateModel,ParameterRes,Load,Basin,Year) %>%
  merge(dfCC1,by=c("ParameterRes","Load","Basin","Year"))

dfCC <- dfCC2 %>%
  mutate(Model="Climate") %>%
  bind_rows(dfCC3)


params<-c("DIN_win_surf","DIP_win_surf","Chla_sum_surf","Secchi_sum","Oxdebt")

df <- dfCC %>%
  filter(Load=="BSAP") %>%
  #filter(Basin=="Baltic Proper") %>%
  mutate(Load = LoadNames[match(Load,Loads)]) %>%
  arrange(ParameterRes,Basin,Year,Load,ClimateModel,Model)


df$ParameterRes <- factor(df$ParameterRes,levels=params)
df$Basin <- factor(df$Basin,levels=Basins)
  

# df$Model <- factor(df$Model,levels=c("Climate","bias0","bias1"))
# df$ClimateModel <- factor(df$ClimateModel,levels=c("rcp45_modA","rcp45_modB","rcp45_modD","rcp85_modA","rcp85_modB","rcp85_modD"))

df$Model <- factor(df$Model,levels=c("Climate","mod0"))
df$ClimateModel <- factor(df$ClimateModel,levels=c("rcp452_modA","rcp852_modA","rcp452_modB","rcp852_modB","rcp452_modD","rcp852_modD"))



#mypal <- c("#d7191c","#abdda4","#2b83ba","#999999")
mypal <- c("#999999","#BBBBBB","#2b83ba")
mysize <- c(0.5,0.5,1)

mypal <- c("#d7191c","#abdda4","#2b83ba")
mysize <- c(0.4,0.2,0.2)
mylabels <- c("Climate model","present climate [bias0]","present climate [bias1]")


dfBounds <- dfScenarios %>%
  filter(Load=="BSAP") %>%
  group_by(ParameterRes,Basin,Target) %>%
  summarise(n=n())
dfBounds$ParameterRes <- factor(dfBounds$ParameterRes,levels=params)
dfBounds$Basin <- factor(dfBounds$Basin,levels=Basins)

# ----------------------- plot obs by parameter ------------------------------
i<-4
for(i in 1:length(params)){
  dfplot <- df %>% filter(ParameterRes==params[i])
  dfBounds1 <- dfBounds %>% filter(ParameterRes==params[i])
  label_unit <- dfplot$Unit[1]
  
  if(i==5){
    subt <- "(Only assessed in Bornholm Basin, Baltic Proper, Gulf of Finland)"
  }else{
    subt <- ""
  }
  
p <- ggplot(data=dfplot) +
  geom_line(aes(x=Year,y=Value,color=Model,size=Model),linetype=1,alpha=0.7) +
  geom_hline(data=dfBounds1,colour="#333333",aes(yintercept=Target),linetype=2,size=0.1) +
  #geom_line(data=dfBounds,aes(x=Year,y=Target),color="#000000",linetype=2,alpha=0.5,size=0.1) +
  facet_grid(Basin~ClimateModel,scales="free_y") +
  scale_color_manual(values=mypal,name="Model", labels=mylabels) +
  scale_size_manual(values=mysize,name="Model", labels=mylabels) +
  theme_ipsum(base_size=9) + 
  ggtitle(label=params[i],subtitle=subt) +
  ylab(label=element_text(paste0("Value [",label_unit,"]"),size=4)) +
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),
        panel.spacing = unit(1, "lines"),legend.position = "bottom",
        plot.title=element_text(size=12),
        plot.subtitle=element_text(size=9),
        strip.text=element_text(size=9),
        legend.text=element_text(size=9))
#p
  #ggsave(paste0("figures/compare_climate_basis_",i,".pdf"),device=cairo_pdf,p,dpi=600,units="cm",width=21,height=29.7,scale=1)
  #ggsave(paste0("figures/compare_climate_basis_",i,".png"),p,dpi=300,units="cm",width=18.4,height=27,scale=3)
  if(i==1){
    pp <- p
  }else{
    pp <- pp + p
  }
}

pp1 <- pp + plot_layout(ncol=5,guides="keep")
#pp1 <- pp + plot_layout(ncol=5,guides="collect") + theme(legend.position='bottom')
#pp1
ggsave("figures/compare_climate_basis_0.png",pp1,dpi=300,units="cm",width=105,height=29,scale=1)


ggsave(paste0("figures/compare_climate_basis_",i,"x.png"),p,dpi=300,units="cm",width=18.4,height=27,scale=2)

# ----------- now calculate ER values -----------------------
dfER <- df %>%
  mutate(ER=ifelse(ParameterRes %in% c("Secchi_sum"),Target/Value,Value/Target))

i<-4
for(i in 1:length(params)){
  dfplot <- dfER %>% filter(ParameterRes==params[i])

  if(i==5){
    subt <- "(Only assessed in Bornholm Basin, Baltic Proper, Gulf of Finland)"
  }else{
    subt <- ""
  }
  
  p <- ggplot(data=dfplot) +
    geom_hline(colour="#FFFFFF",yintercept=0.6,linetype=0,size=0) +
    geom_line(aes(x=Year,y=ER,color=Model,size=Model),linetype=1,alpha=0.7) +
    geom_hline(colour="#333333",yintercept=1,linetype=2,size=0.1) +
    facet_grid(Basin~ClimateModel,scales="free_y") +
    scale_color_manual(values=mypal,name="Model", labels=mylabels) +
    scale_size_manual(values=mysize,name="Model", labels=mylabels) +
    theme_ipsum(base_size=9) + 
    ggtitle(label=paste0(params[i]," (ER)"),subtitle=subt) +
    ylab(label=element_text(paste0("Eutrophication Ratio"),size=4)) +
    theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),
          panel.spacing = unit(1, "lines"),legend.position = "bottom",
          plot.title=element_text(size=12),
          plot.subtitle=element_text(size=9),
          strip.text=element_text(size=9),
          legend.text=element_text(size=9))
  #p
  ggsave(paste0("figures/compare_climate_basis_",i,"_ER.pdf"),device=cairo_pdf,p,dpi=600,units="cm",width=21,height=29.7,scale=1)
  #ggsave(paste0("figures/compare_climate_basis_",i,".png"),p,dpi=300,units="cm",width=18.4,height=27,scale=3)
  if(i==1){
    pp <- p
  }else{
    pp <- pp + p
  }
}


# ----------- now calculate Criteria C1, C2, C3 -----------------------
dfER_CR <- dfER %>%
  ungroup() %>%
  group_by(Basin,Year,Load,ClimateModel,Model,CriteriaID,Criteria) %>%
  summarise(n=n(),ER=mean(ER,na.rm=T), Params = paste0(ParameterRes,collapse=",")) %>%
  mutate(ER=ifelse(is.nan(ER),NA,ER))

criteria <- c("Causative factors","Direct effects","Indirect effects")
dfER_CR$Criteria <- factor(dfER_Criteria$Criteria,levels=criteria)

i<-3
for(i in 1:length(criteria)){
  dfplot <- dfER_CR %>% filter(Criteria==criteria[i])
  subt <- dfER_CR$Params[i]
  
  p <- ggplot(data=dfplot) +
    geom_hline(colour="#FFFFFF",yintercept=0.6,linetype=0,size=0) +
    geom_line(aes(x=Year,y=ER,color=Model,size=Model),linetype=1,alpha=0.7) +
    geom_hline(colour="#333333",yintercept=1,linetype=2,size=0.1) +
    facet_grid(Basin~ClimateModel,scales="free_y") +
    scale_color_manual(values=mypal,name="Model", labels=mylabels) +
    scale_size_manual(values=mysize,name="Model", labels=mylabels) +
    theme_ipsum(base_size=9) + 
    ggtitle(label=paste0("C",i," - ",criteria[i]),subtitle=subt) +
    ylab(label=element_text(paste0("Eutrophication Ratio"),size=4)) +
    theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),
          panel.spacing = unit(1, "lines"),legend.position = "bottom",
          plot.title=element_text(size=12),
          plot.subtitle=element_text(size=9),
          strip.text=element_text(size=9),
          legend.text=element_text(size=9))
  #p
  #p
  ggsave(paste0("figures/compare_climate_basis_Crit_",i,".pdf"),device=cairo_pdf,p,dpi=600,units="cm",width=21,height=29.7,scale=1)
  #ggsave(paste0("figures/compare_climate_basis_",i,".png"),p,dpi=300,units="cm",width=18.4,height=27,scale=3)
  if(i==1){
    pp <- p
  }else{
    pp <- pp + p
  }
}



# ----------- now calculate HEAT overall -----------------------
dfHEAT <- dfER_CR %>%
  ungroup() %>%
  group_by(Basin,Year,Load,ClimateModel,Model) %>%
  summarise(ER=max(ER,na.rm=T)) %>%
  mutate(ER=ifelse(is.nan(ER),NA,ER))

  p <- ggplot(data=dfHEAT) +
    geom_hline(colour="#FFFFFF",yintercept=0.6,linetype=0,size=0) +
    geom_line(aes(x=Year,y=ER,color=Model,size=Model),linetype=1,alpha=0.7) +
    geom_hline(colour="#333333",yintercept=1,linetype=2,size=0.1) +
    facet_grid(Basin~ClimateModel,scales="free_y") +
    scale_color_manual(values=mypal,name="Model", labels=mylabels) +
    scale_size_manual(values=mysize,name="Model", labels=mylabels) +
    theme_ipsum(base_size=9) + 
    ggtitle(label="HEAT",subtitle="OOAO from C1, C2, C3") +
    ylab(label=element_text(paste0("Eutrophication Ratio"),size=4)) +
    theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),
          panel.spacing = unit(1, "lines"),legend.position = "bottom",
          plot.title=element_text(size=12),
          plot.subtitle=element_text(size=9),
          strip.text=element_text(size=9),
          legend.text=element_text(size=9))
  #p
  ggsave(paste0("figures/compare_climate_basis_HEAT.pdf"),device=cairo_pdf,p,dpi=600,units="cm",width=21,height=29.7,scale=1)
  #ggsave(paste0("figures/compare_climate_basis_",i,".png"),p,dpi=300,units="cm",width=18.4,height=27,scale=3)
  if(i==1){
    pp <- p
  }else{
    pp <- pp + p
  }




