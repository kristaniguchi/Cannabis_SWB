#'  Cannabis SWB Phase II - Flow data tidy script: gage and PRMS model timeseries
#'  
#'  This script tidy's the original PRMS flow timeseries sent by SWB for Eel River and associated gages used for validation/calibration
#'  Integrates file name info into data tables and combines all LOI data into one csv
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
flow_dir <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/"


############################################################################################################################
## Tidying Eel River modeled and gaged flow timeseries data

##Eel river directory
ER_dir <- paste0(flow_dir, "/Eel_river/")

####
####Gaged flow
#ER gaged files to read in
gage.files <- list.files(paste0(ER_dir,"/GAGES/"), full.names = TRUE)
#list gage file names to extract gage_ID from in loop
gage.fnames <- list.files(paste0(ER_dir,"/GAGES/"))
#extract gageID
fname.split <- str_split(gage.fnames, pattern="_")

#lookup table with gage_ID and model_ID
lookup <- read.csv(file=paste0(ER_dir, "/Lookup_Tables/Gage_PRMS_Subbasin_Lookup.csv")) %>% 
  #create new column with model_ID
  mutate(model_ID = paste0("ER_", PRMS.Subbasin))

#empty output df for gaged timeseries to be appended to
gage.df <- data.frame()

#loop through gaged data files, read csvs, extract 

for(i in 1:length(gage.files)){
  #read in csv i
  gage.i.dat <- read.csv(gage.files[i])
  #col names for gage.i.dat
  col.names.dat.i <- names(gage.i.dat)
  
  #find gage_ID, second to last element of split
  gage_ID.i <- fname.split[[i]] [length(fname.split[[i]])-1]
  
  #find associated model_ID where gage is located
  lookup.row <- lookup[lookup$Gage.ID == gage_ID.i,]
  model_ID.i <- lookup.row$model_ID
  
  #create output df for i
  output.i <- gage.i.dat %>% 
    mutate(model_ID = model_ID.i,
           gage_ID = gage_ID.i) %>% 
    #rename first column date
    rename(date = col.names.dat.i[1],
           #rename second column obs
           flow_cfs = col.names.dat.i[2])
  #append into output df
  gage.df <- gage.df %>% 
    bind_rows(output.i)
}

#write csv with just gaged flow data
write.csv(gage.df, file=paste0(ER_dir, "EelRiver_Gaged_Flow_combined.csv"), row.names=FALSE)

####
####Modeled flow

#read in modeled flow csv (only one, each column 2:length is for different model subbasin)
model.dat.orig <- read.csv(paste0(ER_dir, "MODEL/eel_subbasins.sub_cfs.csv"), check.names = FALSE)
#column names for model.dat
col.names.model.dat.orig <- names(model.dat.orig)


#emtpy output df for modeled flow data
model.dat <- data.frame()

#loop through columns 2:length to extract and tidy flow timeseries

for(k in 2:length(col.names.model.dat.orig)){
  #subset dat for k subbasin
  model.dat.orig.k <- model.dat.orig %>% 
    #select date col 1 and k column flow for subbasin k
    select(col.names.model.dat.orig[1], col.names.model.dat.orig[k]) %>% 
    #rename col names
    rename(date = col.names.model.dat.orig[1],
           flow_cfs = col.names.model.dat.orig[k]) %>% 
    #create model_ID column by pasting ER_ with original col header k (subbasin number)
    mutate(model_ID = paste0("ER_", col.names.model.dat.orig[k]))
  
  #append data into model.dat
  model.dat <- model.dat %>% 
    bind_rows(model.dat.orig.k)
  
}

#write csv modeled flow data reformatted
write.csv(model.dat, file = paste0(ER_dir, "EelRiver_Modeled_Flow_combined.csv"), row.names=FALSE)

####
####Combine model and gaged flow for watershed delineation tool (omit modeled flow where gaged data is available)

#find model_IDs that are co-located at gages
model.ID.remove <- unique(gage.df$model_ID)

#remove flow data associated with gages
model.dat.remove <- model.dat[!(model.dat$model_ID %in% model.ID.remove),] %>% 
  #create empty column for gage_ID for these
  mutate(gage_ID = "")
#quick check to see if removed model.ID.remove
unique(model.dat.remove$model_ID)

#combine gage.df with model.dat.remove
gage.model.dat.for.tool <- gage.df %>% 
  bind_rows(model.dat.remove)

#write csv
write.csv(gage.model.dat.for.tool, file=paste0(ER_dir, "EelRiver_Gage_Model_Flow_combined_for_tool.csv"), row.names=FALSE)

############################################################################################################################
## Tidying XXXX River modeled and gaged flow timeseries data

#note to Abel: depending on the format of the gaged and modeled flow timeseries data SWB sends us, we may or may not need to tidy or may need to alter how we tidy the data
