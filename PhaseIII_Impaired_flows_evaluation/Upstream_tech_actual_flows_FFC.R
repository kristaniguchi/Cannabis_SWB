#### Calculate functional flow metrics from impaired flows by Upstream Tech
#multiple csv files for each gage and also comids associated with biosites
#also calculate FFMs at gages for entire POR (or matching POR where have actual flows)

#### load necessary libraries
# for first time users, use the following line to install ffc API package:
#install.packages("httr", 'xml2')  


{
  library(httr)
  library(devtools)
  library(usethis)
  library(tidyverse)
  library(readxl)
  library(fs)
  library(ffcAPIClient)
  library(lubridate)
  library(magrittr)
  library(ggplot2)
  library(sf)
}

# set wd
upstream.dir <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/"
setwd(upstream.dir)

###Load data
#find csv file paths for predictions at gages
list.files <- list.files(upstream.dir, full.names=TRUE, pattern = ".csv")

#lookup tables for sites COMID, etc.
biosites_lu <- read.csv("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/Bio/Bio_Stream_Type_Land_Use_summary.csv") %>% 
  #select masterid and COMID only
  select("masterid", "COMID") %>% 
  rename("siteID" = "masterid") %>% 
  mutate(dataset = "biosites", Dataset = "biosites")
#all model output nodes PRMS
PRMS_lu <- read_xlsx("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Eel_River/Lookup_Tables/LOOKUP_table_all_v2_edits.xlsx", sheet = "LU_model") %>% 
  filter(model_type == "PRMS") %>% 
  rename("siteID" = "model_ID") %>% 
  select("siteID", "COMID") %>% 
  mutate(Dataset = "PRMS_nodes", 
         #although for PRMS_nodes, will call dataset "gages" to match up with gage impaired flows
         dataset = "gages")
#gage LU, linking gageID to PRMS model_ID,
gage_lu <- read.csv("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Eel_River/Lookup_Tables/Gage_PRMS_Subbasin_Lookup_comid.csv") %>% 
  mutate(dataset = "gages")

#FFM metric names etc
metric.names <- read.csv("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Eel_River/Lookup_Tables/all_metric_def_list_FFMs_v2 1.csv")

######################################
###Loop to calculate FFMs (annual) for all locations of interest (LOIs)

#create empty df for FFC outputs to be saved
outputs_ffc <- data.frame()
#create empty df for warnings for sites that needed to be skipped (couldn't run FFC
warnings_ffc <- data.frame()
#empty file
empty_file <- NA

# Set the API endpoint URL
url <- "https://flowcalculator.codefornature.org/api/calculator"

for(i in 1:length(list.files)){
  #for debugging: start from last failed iteration
  #for(i in 24:length(list.files)){
  
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
    
    #format flow file, only use date and flow_cfs_impaired columns
    flow.file <- flows.upstream.sub %>% 
      select("datetime", "actual_discharge_mean") %>% 
      rename("date" = "datetime",
             "flow" = "actual_discharge_mean")
    #write a dummy csv to be replaced each iteration, since it only takes csv file timeseries
    csv_temp <- "C:/Users/kristinet/OneDrive - SCCWRP/temp_flow_file.csv"
    write.csv(flow.file, file = csv_temp, row.names = FALSE)
    
    # Prepare the CSV file to upload
    csv_file <- upload_file(csv_temp)
    
    # Define form parameters
    params <- list(
      calculator = "recommended",                # Optional: Choose "reference", "flashy" or "recommended"
      #class = "LSR",                           # Flow class (or use a valid class key)
      comid = comid.i,                         # Optional COMID if it is available
      file = csv_file                          # The timeseries CSV file
    )
    
    # Send the POST request with multipart form data
    response <- POST(url, body = params, encode = "multipart")
    
    # Parse the JSON response
    result <- content(response, as = "parsed")
    # You can continue from here by loading the data into a df, write to disk or whatever your analysis requires!
    
    # read the output metrics string from the results and load it into a dataframe
    df <- read.csv(text = result$output_metrics, header = TRUE)
    # read metadata to know which calculator was used
    df_meta <- read.csv(text = result$metadata_file, header = TRUE)
    
    #if df is empty there are negative flow values and not enough values in POR for FFC, then skip outputting df and create error message
    if(length(df$Year) < 1){
      #print warnings
      warnings.i <- data.frame(matrix(nrow=1, ncol=1)) %>% 
        mutate(warnings = result$warnings,
               iteration = i,
               siteID = gage.i,
               dataset = dataset.i,
               COMID = comid.i) %>% 
        select(warnings, iteration, siteID, dataset, COMID)
      
      # paste into warning output
      warnings_ffc <- bind_rows(warnings_ffc, warnings.i)
    }else{
      # add column with siteID and dataset
      df <- df %>% 
        mutate(siteID = gage.i,
               dataset = dataset.i,
               COMID = comid.i,
               #output which calculator was used
               used_calculator = df_meta$Metadata_value[df_meta$Metadata_code == "Used_Calculator"])
      
      # paste into output
      outputs_ffc <- bind_rows(outputs_ffc, df)
      # Check out the data!
      # print(head(df))
    }
  }
}

#write csv
write.csv(outputs_ffc, file="./FFM/Impaired_FFM_upstream_tech_comid_gages.csv")
write.csv(warnings_ffc, file="./FFM/Impaired_FFM_upstream_tech_comid_gages_warnings.csv")
unique.sites.out <- unique(outputs_ffc$COMID)


#################################################################################
#calculate FFMs for all gages where we have predictions. Match POR to impaired flows
# set wd
#setwd("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/FFMs_LOI_NC_benchmark_sites/")
setwd("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/")

###Load data

#### read-in data and lookup tables
#gage LU, linking gageID to PRMS model_ID,
gage_lu <- read.csv("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Eel_River/Lookup_Tables/Gage_PRMS_Subbasin_Lookup_comid.csv") %>% 
  mutate(dataset = "gages") %>% 
  filter(Type2 == "Validation")
unique.gages <- unique(gage_lu$Gage.ID)

#set period of record for FFM calculations to WY 2016-2022 (POR for impaired flows)
POR <- c(2002, 2022)

#FFM metric names etc
metric.names <- read.csv("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Eel_River/Lookup_Tables/all_metric_def_list_FFMs_v2 1.csv")

######################################
###Loop to calculate FFMs (annual) for all gages (observed data)

#create empty df for FFC outputs to be saved
outputs_ffc <- data.frame()
#create empty df for warnings for sites that needed to be skipped (couldn't run FFC
warnings_ffc <- data.frame()

# Set the API endpoint URL
url <- "https://flowcalculator.codefornature.org/api/calculator/gage"

for(i in 1:length(unique.gages)){
  #for debugging: start from last failed iteration
  #for(i in 8:length(unique.gages)){
  
  #subset to gage lu i
  gage_lu.sub <- gage_lu[gage_lu$Gage.ID == unique.gages[i],]
  
  ##if gage ID has a historical POR,  don't set start and end date. 11472800 WY 1968-2005,11472900/11472200/11472150 WY 1959-2005, 11479700 WY 1958-1967, all only have <=4 yr overlap
  if(unique.gages[i] %in% c(11472800, 11472900, 11472200, 11472150, 11479700)){
    # Define query parameters
    params <- list(
      id = unique.gages[i],                         # USGS gage id or CDEC id (USGS in this example)
      calculator = "recommended")                   # Optional: Choose "reference", "flashy" or "recommended"
  } else{
    # Define query parameters
    params <- list(
      id = unique.gages[i],                         # USGS gage id or CDEC id (USGS in this example)
      calculator = "recommended",                   # Optional: Choose "reference", "flashy" or "recommended"
      #set start and end dates as WY 2002 - 2022
      start_date = "2001/10/01",               # Optional: First day to include in the analysis. Date format: YYYY/MM/DD
      end_date = "2022/09/30"                  # Optional: Last day to include in the analysis. Date format: YYYY/MM/DD
    )
  }
  
  
  # Send the GET request with query parameters
  response <- GET(url, query = params)
  
  # Parse the JSON response
  result <- content(response, as = "parsed")
  # You can continue from here by loading the data into a df, write to disk or whatever your analysis requires!
  
  # read the output metrics string from the results and load it into a dataframe
  df <- read.csv(text = result$output_metrics, header = TRUE)
  # read metadata to know which calculator was used
  df_meta <- read.csv(text = result$metadata_file, header = TRUE)
  
  #if df is empty , then skip outputting df and create error message
  if(length(df$Year) < 1){
    #print warnings
    warnings.i <- data.frame(matrix(nrow=1, ncol=1)) %>% 
      mutate(warnings = result$warnings,
             iteration = i,
             siteID = unique(gage_lu.sub$model_ID),
             dataset = "gages_obs",
             COMID = unique(gage_lu.sub$COMID),
             Gage.ID = unique(gage_lu.sub$Gage.ID)) %>% 
      select(warnings, iteration, siteID, dataset, COMID, Gage.ID)
    
    # paste into warning output
    warnings_ffc <- bind_rows(warnings_ffc, warnings.i)
  }else{
    # add column with siteID and dataset
    df <- df %>% 
      mutate(siteID = unique(gage_lu.sub$model_ID),
             dataset = "gages_obs",
             COMID = unique(gage_lu.sub$COMID),
             Gage.ID = unique(gage_lu.sub$Gage.ID),
             #output which calculator was used
             used_calculator = df_meta$Metadata_value[df_meta$Metadata_code == "Used_Calculator"])
    
    # paste into output
    outputs_ffc <- bind_rows(outputs_ffc, df)
    # Check out the data!
    # print(head(df))
  }
}

#write csv
write.csv(outputs_ffc, file=paste0(upstream.dir,"FFM//Obs_FFM_validation_gages_upstreamtech_POR.csv"))
unique.sites.out <- unique(outputs_ffc$siteID)

