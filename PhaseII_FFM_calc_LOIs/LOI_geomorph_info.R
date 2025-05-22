#####create summary table to populate geomorphic data for LOIs

## r load packages
#### load necessary libraries
# for first time users, use the following line to install ffc tool package:
# devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
{
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

####import data
#read in lookup table for biosites for COMIDs
lu_biosites <- read.csv("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/BioSites/Bio_Stream_Type_Land_Use_summary.csv") %>% 
  mutate(unique_ID = masterid, Dataset = "BioSite", COMID = as.numeric(COMID)) %>% 
  select(unique_ID, Dataset, COMID, AreaSqKm) %>% 
  rename(DA.sqkm = AreaSqKm)

#### read in lookup table for all LOI datasets to get COMIDs
lu_LOIs_all <- read_excel("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Expected vs Modeled Area/Expected vs Modeled_Summary_3.xlsx", sheet=1) %>% 
  #exclude PRMS model node delineations
  filter(Dataset != "PRMS model node delineation" & Dataset != "BioStream2" & Dataset != "BioStream020425") %>% 
  mutate(unique_ID = Point, COMID = as.numeric(COMID)) %>% 
  select(unique_ID, Dataset, COMID, 'SqKM (from tool)') %>% 
  rename(DA.sqkm = 'SqKM (from tool)') %>% 
  bind_rows(lu_biosites) %>% 
  #remove duplicate rows
  distinct()


#some COMIDs were 0, need to replace with closest COMID match
#### read in lookup table for all LOI datasets to get COMIDs
lu_LOIs_0s <- read_excel("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Expected vs Modeled Area/Expected vs Modeled_Summary_3.xlsx", sheet=1) %>% 
  #exclude PRMS model node delineations
  filter(COMID == 0) %>% 
  mutate(unique_ID = Point, COMID = as.numeric(COMID)) %>% 
  select(unique_ID, Dataset, More) %>% 
  rename(COMID = More) %>% 
  mutate(COMID = as.numeric(COMID))

#remove rows with COMID 0 and attach new COMIDs to use
lu_LOIs_all <- lu_LOIs_all %>% 
  filter(COMID != 0) %>% 
  bind_rows(lu_LOIs_0s)
