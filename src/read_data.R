library(tidyverse)

# model<-c("bias","B","C")
# rcp <- c("rcp45","rcp85")
# loads <- c("HIST_1971_2012","REF_2013_2100","PEAK_2013_2100","BSAP_2013_2100")

# ScenarioDataBaltsem94\modA_rcp45_REF
# hindcast
# bias0_rcp45_BSAP
# bias1_rcp45_BSAP
# 
# scendata_BS.csv

# gather results
dirs <- list.dirs(path="data/ScenarioDataBaltsem94")
dirs <- list.dirs(path="data/BALTSEMrecalibrated")
dfScenarios <- NULL
for(dir in dirs){
  cat(paste0(dir,"\n"))
  files <- list.files(path=dir,pattern="*.csv")
  for(file in files){
    temp <- read.table(file=paste0(dir,"/",file),sep=",",header=T)
    Scenario <- substr(dir,nchar(dirs)+2,nchar(dir))
    temp$Scenario <- Scenario
    temp$Basin <- substr(file,10,nchar(file)-4)
    #temp$From = as.numeric(substr(dir,nchar(dir)-8,nchar(dir)-5))
    #temp$To = as.numeric(substr(dir,nchar(dir)-3,nchar(dir)))
    if(Scenario=="hindcast"){
      temp$Model <- "Hindcast"
      temp$Climate <- ""
      temp$Load <- "HIST"
    }else{
      n1 <- str_locate(Scenario,"_")[1]
      n2 <- str_locate(substr(Scenario,n1+1,99),"_")[1]
      temp$Model <- substr(Scenario,1,n1-1)
      temp$Climate <- substr(Scenario,n1+1,n1+n2-1)
      temp$Load <- substr(Scenario,n1+n2+1,nchar(Scenario))
    }
    if(is.null(dfScenarios)){
      dfScenarios <- temp
    } else{
      dfScenarios <- bind_rows(dfScenarios,temp)
    } 
  }
}

df <- dfScenarios %>%
  group_by(Model,Climate,Load) %>%
  summarise(From=min(Year),To=max(Year))



# pivot from wide to long format
dfScenarios <- dfScenarios %>%
  pivot_longer(cols=2:30,names_to = "Parameter", values_to = "Value")

# read threshold information
dfTargets <-  read.table("data/targets.txt",header=T,sep="\t",fileEncoding="UTF-8")

# table matching parameter names in Scenario results to parameter names in target (threshold) data
dfParameterMatch <- data.frame(
  ParameterRes=c("DIN_win_surf",
                 "DIP_win_surf",
                 "Chla_sum_surf",
                 "Secchi_sum",
                 "Oxdebt"),
  Parameter=c("DIN",
              "PO4",
              "Chla",
              "Secchi",
              "O2debt"))

# table matching basin names in Scenario results to basin names in target (threshold) data
dfBasinMatch <- data.frame(
  BasinRes = c("Kattegat",
               "Straits",
               "AR",
               "BN",
               "GS",
               "GR",
               "GF",
               "BS",
               "BB"),
  Basin = c("Kattegat",
            "Danish Straits",
            "Arkona Basin",
            "Bornholm Basin",
            "Baltic Proper",
            "Gulf of Riga",
            "Gulf of Finland",
            "Bothnian Sea",
            "Bothnian Bay"))

# match scenario results to threshold values
dfScenarios <- dfScenarios %>%
  rename(ParameterRes=Parameter,BasinRes=Basin) %>%
  left_join(dfParameterMatch,by="ParameterRes") %>%
  filter(!is.na(Parameter)) %>%
  left_join(dfBasinMatch,by="BasinRes") %>%
  filter(!is.na(Basin)) %>%
  left_join(dfTargets,by=c("Basin","Parameter"))

# save results
save(dfScenarios,file="data/HEAT350_Climate_scenario_data.Rda")

