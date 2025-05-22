





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

#### set-up token for use
# token is unique to each user  
ffctoken <- 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJmaXJzdE5hbWUiOiJBZHJpYW5hIiwibGFzdE5hbWUiOiJMZSBDb21wdGUiLCJlbWFpbCI6ImFkcmlhbmFsc0BzY2N3cnAub3JnIiwicm9sZSI6IlVTRVIiLCJpYXQiOjE2OTMyNTA2Njl9.mjM3WqZJXJdaJHEqN5e5Fh90JFgERzERqxiksSCpGbE'

#ffctoken <- ffcAPIClient::set_token(Sys.getenv("EFLOWS_TOKEN", ""))
ffc <- ffcAPIClient::FFCProcessor$new()

# set wd
setwd("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/FFMs_LOI_NC_benchmark_sites/")

###Load data
#impaired flows csv
flows <- read.csv("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/NC_impaired_flows_20250515/outputs/NC_sites_daily_impaired_flow.csv")

sites <- unique(flows$siteID)
