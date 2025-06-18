#### Recalculate functional flow metrics for all gages using updated functional flow calcualtors (FFC)
###Impaired flows estimated by taking unimpaired flows from PRMS model and subtracting total water diverted from SWB's demand dataset

### Updated FFC: https://flowcalculator.codefornature.org/

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
POR <- c(2016, 2022)

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
  
  ##if gage ID has a historical POR,  don't set start and end date
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
      #set start and end dates as WY 2010 - 2022
      start_date = "2015/10/01",               # Optional: First day to include in the analysis. Date format: YYYY/MM/DD
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
write.csv(outputs_ffc, file="C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/NC_impaired_flows_20250515/python_edit_SCCWRP/outputs/FFM_newFFC/Obs_FFM_validation_gages.csv")
#write.csv(warnings_ffc, file="C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/NC_impaired_flows_20250515/python_edit_SCCWRP/outputs/FFM_newFFC/Obs_FFM_validation_gages_warnings.csv")
unique.sites.out <- unique(outputs_ffc$siteID)


