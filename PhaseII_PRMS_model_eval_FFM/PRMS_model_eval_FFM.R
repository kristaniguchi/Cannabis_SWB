#'  Cannabis SWB Phase II - PRMS watershed model evaluation at the functional flow metric level
#'  
#'  This script takes FFMs calculated from gaged and observed data and evaluates model performance for every gage
#'  Model performance by FFM following methods by Grantham et al. (2022) and Moriasi et al. (2007)
#'  
#'  PRMS watershed model flow timeseries data for:
#'    Eel River
#'    Can add other PRMS watersheds when SWB sends them and add to this script
#'      
#'@author Kris Taniguchi-Quan, SCCWRP
#'
############################################################################################################################

#load libraries
#install.packages("tidyverse")
#install.packages("dplyr")
library(tidyverse)
library(dplyr)


#data directories (location where csv files are saved - change to your local directory for each folder)
FFM_dir <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/FFC_outputs/csv_results/"


############################################################################################################################
## Tidying Eel River modeled and gaged flow timeseries data

#read in lookup table that has gage_ID and model_ID
lookup.ER <- read.csv(file="C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Eel_River/Lookup_Tables/Gage_PRMS_Subbasin_Lookup.csv") %>% 
  #create col with model_ID
  mutate(model_ID = paste0("ER_", PRMS.Subbasin))

#list all files in FFM_dir
list.files.all <- list.files(FFM_dir, full.names=TRUE)
#find file index for only Eel River (ER) gage and model data
ind.ER <- grep("ER_", list.files.all)
#subset to ER only
list.files.ER <- list.files.all[ind.ER]
#can delete line below later, but only using KTQ files since original ER gage data need to be reformatted
ind.KTQ <- grep("_KTQ", list.files.ER)
list.file.ER2 <- list.files.ER

#read in gaged data
ind.gage.ER <- grep("_gage_", list.file.ER2)
gage.ffm.ER <- read.csv(list.file.ER2[ind.gage.ER])

#read in model data
ind.model.ER <- grep("_model_", list.file.ER2)
model.ffm.ER <- read.csv(list.file.ER2[ind.model.ER])

#find unique gageIDs
unique.gages <- unique(gage.ffm.ER$gage_ID)
#find associated model_IDs for the unique.gages (these are model_IDs we want to keep in FFM data)
unique.model_ID <- lookup.ER$model_ID[lookup.ER$Gage.ID %in% unique.gages] 

#subset to only model_IDs that have gages
model.ffm.ER.sub <- model.ffm.ER[as.character(model.ffm.ER$model_ID) %in% unique.model_ID,]
