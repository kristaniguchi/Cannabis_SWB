#### Calculate annual Q99 (99th percentile of daily flow) from Upstream Tech Unimpaired and Actual flows
#same structure as Upstream_tech_actual_flows_FFC.R / Upstream_tech_unimpaired_flows_FFC.R:
#loops through the csv files for each gage and each COMID associated with biosites
#Q99 is calculated by water year (Oct 1 - Sep 30), WY 2002-2022, from discharge_mean of each model
#outputs are formatted to match the FFC outputs and the biosite FFM table
#(NC_Biosites_Impaired_Unimpaired_FFM_Upstream_Tech_Models.csv) so Q99 can be joined as an additional metric

{
  library(tidyverse)
  library(lubridate)
}

# set wd
#PC desktop
#upstream.dir <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/"
#MacBook
upstream.dir <- "/Users/kristq/Library/CloudStorage/OneDrive-SharedLibraries-OneDrive-SharedLibraries-SCCWRP2 2/Cannabis E-Flows - General/Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/"
setwd(upstream.dir)

#water years matching Upstream Tech model period of record
POR <- c(2002, 2022)

###Load data
#find csv file paths for predictions at gages and biosite COMIDs
list.files <- list.files(upstream.dir, full.names=TRUE, pattern = "^Unimpaired_Actual_.*\\.csv$")

#existing biosite FFM table (to get water year type and output in the same long format)
biosite_FFMs <- read.csv("./FFM/NC_Biosites_Impaired_Unimpaired_FFM_Upstream_Tech_Models.csv")
WYT_lu <- biosite_FFMs %>%
  distinct(Year, COMID, WYT)

######################################
###Loop to calculate annual Q99 for all locations of interest (LOIs)

#create empty df for outputs
outputs_q99 <- data.frame()
#empty file
empty_file <- NA

for(i in 1:length(list.files)){

  #subset to site i
  flows.upstream.sub <- read.csv(list.files[i])

  #if data is empty, skip
  if(length(flows.upstream.sub$site_id)<1){
    empty_file <- c(empty_file, list.files[i])
  }else{
    #determine if gage or comid (biosite)
    gage <- grep("gage", list.files[i])
    #if gage
    if(length(gage)>=1){
      gage.i <- unique(flows.upstream.sub$site_id)
      dataset.i <- "gage"
    }else {
      #then gage.i is NA
      gage.i <- NA
      dataset.i <- "Biosite"
    }

    #find comid
    comid.i <- unique(flows.upstream.sub$actual_comId)

    #calculate Q99 by water year for unimpaired and actual discharge_mean
    q99.i <- flows.upstream.sub %>%
      select(datetime, unimpaired_discharge_mean, actual_discharge_mean) %>%
      mutate(date = as.Date(datetime),
             #water year: Oct-Dec belong to the following year
             Year = year(date) + ifelse(month(date) >= 10, 1, 0)) %>%
      filter(Year >= POR[1], Year <= POR[2]) %>%
      group_by(Year) %>%
      summarise(n_days = sum(!is.na(actual_discharge_mean)),
                Q99_Unimpaired = quantile(unimpaired_discharge_mean, probs = 0.99, na.rm = TRUE),
                Q99_Actual = quantile(actual_discharge_mean, probs = 0.99, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(siteID = gage.i,
             dataset = dataset.i,
             COMID = comid.i)

    # paste into output
    outputs_q99 <- bind_rows(outputs_q99, q99.i)
  }
}

#check: water years with incomplete daily records (should be none for model output)
outputs_q99 %>% filter(n_days < 365)

#write csv (gages and biosites, wide by model)
write.csv(outputs_q99, file="./FFM/Q99_upstream_tech_comid_gages.csv", row.names = FALSE)

######################################
###Format biosite Q99 to match NC_Biosites_Impaired_Unimpaired_FFM_Upstream_Tech_Models.csv

q99.biosites <- outputs_q99 %>%
  filter(dataset == "Biosite") %>%
  left_join(WYT_lu, by = c("Year", "COMID")) %>%
  transmute(Year, WYT, dataset, COMID, FFM = "Q99",
            Value_Unimpaired = Q99_Unimpaired,
            Value_Actual = Q99_Actual)

write.csv(q99.biosites, file="./FFM/NC_Biosites_Q99_Upstream_Tech_Models.csv", row.names = FALSE)

#combined table: all FFMs plus Q99 (original file is left unchanged)
biosite_FFMs_Q99 <- bind_rows(biosite_FFMs, q99.biosites)
write.csv(biosite_FFMs_Q99, file="./FFM/NC_Biosites_Impaired_Unimpaired_FFM_Q99_Upstream_Tech_Models.csv", row.names = FALSE)

######################################
###Median delta Q99 per COMID (relative change, consistent with magnitude FFMs)

median_delta_q99 <- q99.biosites %>%
  mutate(Delta = (Value_Actual - Value_Unimpaired)/Value_Unimpaired) %>%
  group_by(COMID) %>%
  summarise(Median_Delta = median(Delta, na.rm = TRUE),
            n_years = sum(!is.na(Delta)),
            .groups = "drop")

summary(median_delta_q99$Median_Delta)
length(unique(median_delta_q99$COMID))
