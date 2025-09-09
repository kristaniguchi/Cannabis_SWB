###Tidying FFM data for bioassessment sites in the North Coast (unimpaired and actual from Upstream Tech Models)

{
  #load libraries and install if necessary
  library(patchwork)
  library("ggplot2")
  library("dplyr")
  library("tidyverse")
  # if (!require(devtools)) install.packages("devtools")
  # library(devtools)
  # install_github("hzambran/hydroGOF")
  library("hydroGOF")
  #install.packages("ztable")
  library("ztable")
  library("glue")
  library("scales")
  library("fmsb")
  library("readxl")
  library(dataRetrieval)
}

#data directories (location where csv files are saved - change to your local directory for each folder)
#FFM directory where all gage FFMs and upstream tech ffms are saved
FFM_dir <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/FFM/"

#read in bioassessment data NC
bio.dat <- read.csv("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/Bio/Bio_Stream_Type_Land_Use_summary.csv")

#read in impaired and unimpaired FFM from upstream tech models, filter to biosite dataset only
unimpaired.ffm <- read.csv("Unimpaired_FFM_upstream_tech_comid_gages.csv") %>% 
  filter(dataset == "Biosite") %>% 
  pivot_longer()
actual.ffm <- read.csv("Impaired_FFM_upstream_tech_comid_gages.csv") %>% 
  filter(dataset == "Biosite")

#read in model performance summaries



####Tidying ffm datasets

#find FFM columns to pivot longer on
ind.start.col <- grep("FA_Mag", names(unimpaired.ffm))
ind.end.col <- grep("DS_No_Flow_Dur", names(unimpaired.ffm))
cols.ffm <- names(unimpaired.ffm)[ind.start.col:ind.end.col]

#pivot longer the FFM columns 
unimpaired.ffm.lng <- unimpaired.ffm %>% 
  #pivot longer to get an FFM column
  pivot_longer(cols = cols.ffm, 
               names_to = "FFM",
               values_to = "Value_Unimpaired") %>% 
  #exclude certain columns
  select(-c(X, Avg, Std, CV, siteID, Int_Class, Overall_Int_Class, used_calculator))

actual.ffm.lng <- actual.ffm %>% 
  #pivot longer to get an FFM column
  pivot_longer(cols = cols.ffm, 
               names_to = "FFM",
               values_to = "Value_Actual") %>% 
  #exclude certain columns
  select(-c(X, Avg, Std, CV, siteID, Int_Class, Overall_Int_Class, used_calculator))

#joing unimpaired to actual df
ffm.all.biosites <- left_join(unimpaired.ffm.lng, actual.ffm.lng, by = c("Year", "COMID", "FFM", "WYT", "dataset"))

write.csv(ffm.all.biosites, file="NC_Biosites_Impaired_Unimpaired_FFM_Upstream_Tech_Models.csv", row.names=FALSE)

unique.comids <- length(unique(ffm.all.biosites$COMID))
bio.unique.comids <- length(unique(bio.dat$COMID))

#############################
#summarize model performance by FFM for upstream tech unimpaired and impaired models
#we will exclude metrics with lower performance

actual.perf <- read.csv("../FFM_eval/Model_performance_FFM_summary_Upstream_Actual_flows.csv") %>% 
  #filter to Validation only
  filter(Upstream_Type =="Validation") %>% 
  #must have at least 5 annual values per metric
  filter(n_annual_gage_model >4)

unimpaired.perf <- read.csv("../FFM_eval/Model_performance_FFM_summary_Upstream_Unimpaired_flows.csv") %>% 
  #filter to Validation only
  filter(Upstream_Type =="Validation") %>% 
  #must have at least 5 annual values per metric
  filter(n_annual_gage_model >4)

#SWB tidy performance table for heatmap - composite using all criteria
perf.criteria.FFM.table.actual <- actual.perf %>% 
  select(Gage.ID, FFM, composite_index, Upstream_Type) %>% 
  pivot_wider(names_from = FFM, values_from = composite_index) 

#SWB tidy performance table for heatmap - composite using dispersion only
perf.criteria.FFM.table.disp.unimpaired <- unimpaired.perf %>% 
  select(Gage.ID, FFM, composite_index_disp, Upstream_Type) %>% 
  pivot_wider(names_from = FFM, values_from = composite_index_disp) 


#summarize and use the mean score for every metric
perf.criteria.FFM.table.actual.mean <- perf.criteria.FFM.table.actual[,3:21] %>% 
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

perf.criteria.FFM.table.disp.unimpaired.mean <- perf.criteria.FFM.table.disp.unimpaired[,3:21] %>% 
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

#find which values are <0.5 overall performance mean
ind.actual <- which(perf.criteria.FFM.table.actual.mean < 0.5)
ind.unimpaired <- which(perf.criteria.FFM.table.disp.unimpaired.mean < 0.5)

#excluded FFM with poor performance
perf.criteria.FFM.table.actual.mean[,ind.actual]
perf.criteria.FFM.table.disp.unimpaired.mean[,ind.unimpaired]

