#'  Cannabis SWB Phase III - Exploring Peak duration recalculation at Biosites
#'  
#'  This script takes the impaired FFMs from upstream tech's models at all biosites and recalculates peak 10 duration using the unimpaired peak 10 magnitude
#'  Goal is to see if the peak 10 duration actually increased under actual flow scenarios or if it was an artifact of the peak 10 magnitude reducing and more days over this lower peak flow threshold 
#'  
#'  
#'      
#'      
#'@author Kris Taniguchi-Quan, SCCWRP

############################################################################################################################

#install package to combine two geom_tile figures together (since only one gage)
#install.packages("patchwork")
#install.packages("fmsb")

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

# set wd
setwd("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/FFM/")

###Load data

#lookup tables for sites COMID, etc.
biosites_lu <- read.csv("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/Bio/Bio_Stream_Type_Land_Use_summary.csv") %>% 
  #select masterid and COMID only
  select("masterid", "COMID") %>% 
  rename("siteID" = "masterid") %>% 
  mutate(dataset = "biosites", Dataset = "biosites")

#read in biosite FFMs (unimpaired and actual)
biosite_FFMs <- read.csv(file="C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/FFM/NC_Biosites_Impaired_Unimpaired_FFM_Upstream_Tech_Models.csv")

###########################
####Loop to recalculate peak 10 duration based on unimpaired peak 10 mag

#create empty output dataframe for updated peak10dur
out.updated.peak10dur <- data.frame()
unique.comid <- unique(biosite_FFMs$COMID)

for(i in 1:length(unique.comid)) {
  
  #find master ID associated with unique.comid i
  biosites_lu.i <- biosites_lu[biosites_lu$COMID == unique.comid[i],]
  
  #list files of actual flow data
  list.files.actual <- list.files(path="C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/", full.names=TRUE)
  #find COMID i file
  ind.file <- grep(unique.comid[i], list.files.actual)
  
  #read in impaired flows for unique.comids i
  flows.i <- read.csv(list.files.actual[ind.file]) %>% 
    select("datetime", "actual_discharge_mean") %>% 
    rename("Date" = "datetime",
           "flow" = "actual_discharge_mean") %>% 
    mutate(Date = as.Date(Date)) 
  #add water year to flows.i
  flows.i.wy <- addWaterYear(flows.i)
  
  #subset FFM (natural) for comid i
  biosite_FFMs.i <- biosite_FFMs %>% 
    filter(COMID == unique.comid[i],
           FFM == "Peak_10")
  
  # Join threshold to flow data by matching year
  flows_with_thresh <- flows.i.wy %>%
    left_join(biosite_FFMs.i, by = c("waterYear" = "Year"))
  
  # Count days where flow > threshold for each year
  days_above_thresh <- flows_with_thresh %>%
    group_by(waterYear) %>%
    summarise(
      days_above = sum(flow >= Value_Unimpaired, na.rm = TRUE),
      total_days = n(),
      percent_above = 100 * mean(flow >=  Value_Unimpaired, na.rm = TRUE)
    )
  
  #create output df for new recalc ffm:
  #subset FFM (natural) for comid i
  biosite_FFMs.i.out <- biosite_FFMs %>% 
    filter(COMID == unique.comid[i],
           FFM == "Peak_Dur_10") %>% 
    select(Year, COMID, FFM) %>% 
    left_join(days_above_thresh, by = c("Year"="waterYear")) %>% 
    select(Year, COMID, FFM, days_above) %>% 
    rename(Value_Actual_Recalc = days_above)
  
  #append to output df
  out.updated.peak10dur <- bind_rows(out.updated.peak10dur, biosite_FFMs.i.out)

  
}

#subset FFM all to peak dur 10
biosite_FFMs.peakdur10 <- biosite_FFMs %>% 
  filter(FFM == "Peak_Dur_10") %>% 
  left_join(out.updated.peak10dur, by=c("Year", "COMID", "FFM")) %>% 
  #mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) %>% 
  mutate(Delta_Recalc = Value_Actual_Recalc - Value_Unimpaired,
  Delta_Orig = Value_Actual - Value_Unimpaired)
  

#plot actual (original) versus actual recalc (all annual data)
ggplot(biosite_FFMs.peakdur10, aes(x = Value_Actual, y = Value_Actual_Recalc)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "Original Actual Peak 10 Duration (days)",
    y = "Recalc Actual Peak 10 Duration (days)"
  ) +
  theme_minimal()

#calculate median delta orig and recalc
median_delta <- biosite_FFMs.peakdur10 %>%
  group_by(COMID) %>%
  summarise(
    Median_Delta_Recalc = median(Delta_Recalc, na.rm = TRUE),
    Median_Delta_Orig   = median(Delta_Orig, na.rm = TRUE),
    .groups = "drop"
  )

#plot actual (original) versus actual recalc (all annual data)
ggplot(median_delta, aes(x = Median_Delta_Orig, y = Median_Delta_Recalc )) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(
    x = "Original Delta Actual Peak 10 Duration (days)",
    y = "Recalc Delta Actual Peak 10 Duration (days)"
  ) +
  theme_minimal()

#write.csv
write.csv(median_delta, file="")