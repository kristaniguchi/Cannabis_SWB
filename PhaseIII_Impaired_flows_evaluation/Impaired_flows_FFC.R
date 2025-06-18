#### Calculate functional flow metrics from impaired flows by SWB
    ###Impaired flows estimated by taking unimpaired flows from PRMS model and subtracting total water diverted from SWB's demand dataset
    #Will also calcualte Q99 (for annual peak flow metric because short POR for 2-10yr peaks)

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
setwd("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/FFMs_LOI_NC_benchmark_sites/")

###Load data
#impaired flows csv (containing all sites: gages, biosites, geomorphic, benchmark, mcBain, etc.)
flows.SWB <- read.csv("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/NC_impaired_flows_20250515/python_edit_SCCWRP/outputs/OVERALL/daily_overall.csv")
#unique sites to run FFC on
sites <- unique(flows.SWB$siteID)
unique.datasets <- unique(flows.SWB$dataset)

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
#benchmark lookup table
benchmark_lu <- data.frame(read_xlsx("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Expected vs Modeled Area/Expected vs Modeled_Summary_3.xlsx", sheet = "Summary"))
#find unique dataset names in benchmark_lu
dataset <- unique(benchmark_lu$Dataset)
#remove PRMS model nodes data and BioStreams data
benchmark_lu2 <- benchmark_lu %>% 
  filter(!Dataset %in% c("PRMS model node delineation", "BioStream2", "BioStream020425")) %>% 
  select("Point", "COMID", "Dataset") %>% 
  rename("siteID" = "Point") %>% 
  #make separate col to match unique.datasets
  mutate(dataset = Dataset)
#change dataset names to match unique.datasets
benchmark_lu2$dataset[benchmark_lu2$dataset == dataset[2]] <- "mcbainsites"
benchmark_lu2$dataset[benchmark_lu2$dataset == dataset[3]] <- "SFEhighresolution"
benchmark_lu2$dataset[benchmark_lu2$dataset == dataset[4]] <- "SFEmainstem"
#make all NC sites "nc_sites_final"
benchmark_lu2$dataset[benchmark_lu2$dataset == dataset[5] | benchmark_lu2$dataset == dataset[6]] <- "nc_sites_final"
#check unique names
unique(benchmark_lu2$dataset)
unique.datasets

#before binding rows, check classes to make sure can combine
df_list <- list(biosites_lu = biosites_lu, PRMS_lu = PRMS_lu, benchmark_lu2 = benchmark_lu2)
class_check <- map_dfr(df_list, ~ as.data.frame(t(sapply(.x, class))), .id = "dataframe")
print(class_check)
#make sure COMID is numeric for all datasets
benchmark_lu2$COMID <- as.numeric(benchmark_lu2$COMID)
biosites_lu$COMID <- as.numeric(biosites_lu$COMID)

#make overall LU table with all datasets (excluding gages because that has siteID as model_ID)
lu_all <- benchmark_lu2 %>% 
  bind_rows(biosites_lu, PRMS_lu) %>% 
  #create duplicate column siteID to make sure McBain sites are consistent with flow data
  mutate(siteID_McBain_orig = siteID)

#Reformat McBain siteIDs to match impaired flows siteID: remove (Google) and spaces
ind.google <- grep("(Google)", lu_all$siteID)
#remove all text inside of parentheses (Google) for mcbainsites
lu_all$siteID[lu_all$dataset == "mcbainsites"] <- gsub("\\s*\\(.*?\\)", "", lu_all$siteID[lu_all$dataset == "mcbainsites"])
#remove all spaces in mcbain names
lu_all$siteID[lu_all$dataset == "mcbainsites"] <- gsub(" ", "", lu_all$siteID[lu_all$dataset == "mcbainsites"])
#for biosites, all sites starting with "BA_" should be "BA-" to match flow siteID
lu_all$siteID[lu_all$dataset == "biosites"] <- gsub("^BA-", "BA_", lu_all$siteID[lu_all$dataset == "biosites"])

#find COMIDs that are zero and replace with appropriate COMID
COMID.0 <- lu_all %>% 
  filter(COMID == 0)
#read in LU for NC geomorphic sites to fill in those zeros (only want to do this for 0 COMIDS, since original LU_all has QA'd comids)
transect_lu <- read_xlsx("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Eel_River/Lookup_Tables/LOOKUP_table_all_v2_edits.xlsx", sheet = "LU_transect")
#filter to siteIDs with COMID zeros
transect_lu <- transect_lu[transect_lu$siteID %in% COMID.0$siteID,] %>% 
  select(COMID, siteID)
#join and replace COMIDs with 0 in lu_all
lu_all <- lu_all %>% 
  left_join(transect_lu, by = "siteID", suffix = c("", ".new")) %>%
  mutate(COMID = coalesce(COMID.new, COMID)) %>%  # Replace if new value exists
  select(-COMID.new)  # Drop extra column

#FFM metric names etc
metric.names <- read.csv("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Eel_River/Lookup_Tables/all_metric_def_list_FFMs_v2 1.csv")

######################################
###Loop to calculate FFMs (annual) for all locations of interest (LOIs)

#create empty df for FFC outputs to be saved
outputs_ffc <- data.frame()
#create empty df for warnings for sites that needed to be skipped (couldn't run FFC
warnings_ffc <- data.frame()

# Set the API endpoint URL
url <- "https://flowcalculator.codefornature.org/api/calculator"

for(i in 1:length(sites)){
#for debugging: start from last failed iteration
#for(i in 285:length(sites)){
    
  #subset to site i
  flows.SWB.sub <- flows.SWB[flows.SWB$siteID == sites[i],]
  
  #find COMID for site i
  lu_sitei <- lu_all %>% 
    filter(dataset == unique(flows.SWB.sub$dataset),
           siteID == unique(flows.SWB.sub$siteID))
  #only use first row of lu_sitei (duplicate sites for some NC_final sites)
  if(length(lu_sitei$COMID)>1){
    lu_sitei <- lu_sitei[1,]
  }
  
  #format flow file, only use date and flow_cfs_impaired columns
  flow.file <- flows.SWB.sub %>% 
    select("date", "flow_cfs_impaired") %>% 
    rename("flow" = "flow_cfs_impaired")
  #write a dummy csv to be replaced each iteration, since it only takes csv file timeseries
  csv_temp <- "C:/Users/kristinet/OneDrive - SCCWRP/temp_flow_file.csv"
  write.csv(flow.file, file = csv_temp, row.names = FALSE)
  
  # Prepare the CSV file to upload
  csv_file <- upload_file(csv_temp)
  
  # Define form parameters
  params <- list(
    calculator = "recommended",                # Optional: Choose "reference", "flashy" or "recommended"
    #class = "LSR",                           # Flow class (or use a valid class key)
    comid = lu_sitei$COMID,                         # Optional COMID if it is available
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
             siteID = unique(flows.SWB.sub$siteID),
             dataset = unique(flows.SWB.sub$dataset),
             COMID = unique(lu_sitei$COMID)) %>% 
      select(warnings, iteration, siteID, dataset, COMID)
    
    # paste into warning output
    warnings_ffc <- bind_rows(warnings_ffc, warnings.i)
  }else{
    # add column with siteID and dataset
    df <- df %>% 
      mutate(siteID = unique(flows.SWB.sub$siteID),
             dataset = unique(flows.SWB.sub$dataset),
             COMID = unique(lu_sitei$COMID),
             #output which calculator was used
             used_calculator = df_meta$Metadata_value[df_meta$Metadata_code == "Used_Calculator"])
    
    # paste into output
    outputs_ffc <- bind_rows(outputs_ffc, df)
    # Check out the data!
    # print(head(df))
  }
}

#write csv
write.csv(outputs_ffc, file="C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/NC_impaired_flows_20250515/python_edit_SCCWRP/outputs/FFM_newFFC/Impaired_FFM_NC_LOIs_ALL.csv")
write.csv(warnings_ffc, file="C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/NC_impaired_flows_20250515/python_edit_SCCWRP/outputs/FFM_newFFC/Impaired_FFM_NC_LOIs_ALL_warnings.csv")
unique.sites.out <- unique(outputs_ffc$siteID)


