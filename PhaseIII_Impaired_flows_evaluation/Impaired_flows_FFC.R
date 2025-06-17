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

#UPDATE FROM HERE McBain siteIDs: no spaces, remove (Google)
lu_all$siteID_McBain_orig[lu_all] <- gsub("(Google)", "", lu_all$siteID_McBain_orig)

#FFM metric names etc
metric.names <- read.csv("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Eel_River/Lookup_Tables/all_metric_def_list_FFMs_v2 1.csv")

######################################
###Loop to calculate FFMs (annual) for all locations of interest (LOIs)

#create empty df for FFC outputs to be saved
outputs_ffc <- data.frame()

# Set the API endpoint URL
url <- "https://flowcalculator.codefornature.org/api/calculator"

for(i in 1:length(sites)){
  #subset to site i
  flows.SWB.sub <- flows.SWB[flows.SWB$siteID == sites[i],]
  
  #find COMID for site i
  lu_sitei <- 
  
  #only use date and flow_cfs_impaired columns
  flow.file <- flows.SWB.sub %>% 
    select("date", "flow_cfs_impaired") %>% 
    rename("flow" = "flow_cfs_impaired")
  
  # Define form parameters
  params <- list(
    calculator = "recommended",                # Optional: Choose "reference", "flashy" or "recommended"
    #class = "LSR",                           # Flow class (or use a valid class key)
    comid = 8212965,                         # Optional COMID if it is available
    file = csv_file                          # The timeseries CSV file
  )
  
  
}

#testing new FFC API

# Prepare the CSV file to upload
csv_file <- upload_file("C:/Users/kristinet/OneDrive - SCCWRP/test_impaired_SFE_2017_209.csv")

# Define form parameters
params <- list(
  calculator = "recommended",                # Optional: Choose "reference", "flashy" or "recommended"
  #class = "LSR",                           # Flow class (or use a valid class key)
  comid = 8212965,                         # Optional COMID if it is available
  file = csv_file                          # The timeseries CSV file
)

# Send the POST request with multipart form data
response <- POST(url, body = params, encode = "multipart")

# Parse the JSON response
result <- content(response, as = "parsed")
# You can continue from here by loading the data into a df, write to disk or whatever your analysis requires!

## Below is an example of reading the flow metrics into a dataframe
# read the output metrics string from the results and load it into a dataframe
df <- read.csv(text = result$output_metrics, header = TRUE)
# Check out the data!
print(head(df))




#subset just to test FFC API
flow.sub <- flows %>% 
  filter(siteID==sites[1]) %>% 
  select(date, flow_cfs_impaired) %>% 
  rename(flow = flow_cfs_impaired) %>% 
  #reformat date
  mutate(date = as.POSIXct(date, format = "%m/%d/%Y"))%>% 
  mutate(date = format(date, "%m/%d/%Y"))
