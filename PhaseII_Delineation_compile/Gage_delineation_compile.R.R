#'  Cannabis SWB Phase II - Compile USGS gage delineations co-located at PRMS model output nodes
#'  
#'  This script takes all of the polygons associated with PRMS model nodes and subsets to those co-located at USGS gages
#'    #Output: polygon shapefiles containing delineations for every USGS gage in modeled watersheds (validation and calibration gages)
#'      #These polygons will be used to validate SWB impaired flows workflow at USGS impaired gages (SWB workflow: subtract demand data from water rights EWRIM from daily unimpaired streamflow)
#'  
#'    
#'    Notes for Eel:
#'      2 SFE gages had reference time periods 1985-2000, only look at reference years
#'      11473900 MF reference gage split calibration WY 1985-2009, validation WY 2010-2021. only look at validation period for performance
#'      11476500 SFE miranda gage changed to reference validation gage
#'      11478500 Lower Eel Van Duzen now reference validation gage, Reference validation 2010-2021. Calibration from 1985-2009.
#'      
#'@author Kris Taniguchi-Quan, SCCWRP
#'
############################################################################################################################

{
  #load libraries and install if necessary
  library("ggplot2")
  library("sf")
  library("dplyr")
  library("tidyverse")
  #install.packages("ztable")
  library("ztable")
  library("glue")
  library("scales")
}

#data directories (location where delineations are stored)
delin_dir <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/"

#set working directory to delin_dir
setwd(delin_dir)


############################################################################################################################
## Tidying Eel River modeled and gaged functional flow metric values, only keep gage and associated model node FFM

#read in lookup table that has gage_ID and model_ID
lookup.gages <- read.csv(file="C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Eel_River/Lookup_Tables/Gage_PRMS_Subbasin_Lookup.csv") %>% 
  #create col with model_ID. This will be used to subset delineation polygons
  mutate(model_ID = paste0(Model_abbrev, "_", PRMS.Subbasin))

#list all files in entire watersheds for model nodes
list.files.all <- list.files(paste0(delin_dir, "Model_Nodes_Watersheds/watersheds_entiredrainage_for_tool/"), full.names=TRUE)
# Filter for files that end with .shp and contain "catchment" (case-insensitive)
shp_catchment_files <- grep("catchment.*\\.shp$", list.files.all, ignore.case = TRUE, value = TRUE)
#exclude "EelRiver_ManualSnap_WCSTool_catchments_v2.shp", older version, use v3 version
shp_catchment_files_filtered <- shp_catchment_files[!grepl("EelRiver_ManualSnap_WCSTool_catchments_v2.shp", shp_catchment_files)]

######################################
###loop to read in all shapefiles with model node catchments, subset

for(i in 1:length(shp_catchment_files_filtered)){
  
  #read in shapefile i
  shapefile.i <- st_read(shp_catchment_files_filtered[i])
  
  #if missing Model_ID column, make one
  if(length(shapefile.i$Model_ID) == 0) {
    shapefile.i$Model_ID <- paste0(shapefile.i$Model_abbv, "_", shapefile.i$Subbasin)
  }
  
  #subset to only nodes with gages
  gages.sub <- shapefile.i[shapefile.i$Model_ID %in% lookup.gages$model_ID,]
  
  #combine with lookup columns
  gages.sub.2 <- gages.sub %>% 
    left_join(lookup.gages, by=c("Model_ID" = "model_ID"))
  
  #write.csv gage delineation polygon shapefile
  #shapefile name
  file.name.i <- paste0(delin_dir, "Gage_delineations/", unique(gages.sub.2$Model_abbrev), "_gages_all_delineations.shp")
  st_write(gages.sub.2, file.name.i)

}

#list all .shp in folder path (previously written)
shp_files <- list.files(paste0(delin_dir, "Gage_delineations/"), pattern = "\\.shp$", full.names = TRUE)

#read them in and combine all shapefiles
# Read each shapefile into an sf object, then combine
combined_sf <- shp_files %>%
  lapply(st_read) %>%        # Read all shapefiles
  bind_rows()                # Combine into one sf object

#write combined shapefile
st_write(combined_sf, paste0(delin_dir, "Gage_delineations/All_gages_delineations_combined.shp"))

######################################
###loop to read in all modeled unimpaired flow at gages

#read in all PRMS unimpaired flow model predictions at model nodes
eel <- read.csv("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/EEL_spring_summer_calibration_rev2/EelRiver_Modeled_Flow_rev2_combined.csv")
elk <- read.csv("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Elk_River_recalibration/ElkRiver_Modeled_Flow_rev2_combined.csv")
LR <- read.csv("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Little_River/LittleRiver_Modeled_Flow_rev2_combined.csv")
Mad <- read.csv("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Mad_River_rev2/MadRiver_Modeled_Flow_rev2_combined.csv")
RWC <- read.csv("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Redwood_Creek_rev2/RedwoodCreek_Modeled_Flow_rev2_combined.csv")

#combine into one df
flow.all <- eel %>% 
  bind_rows(elk, LR, Mad, RWC)

#write csv of combined flow dataset
write.csv(flow.all, file= "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/PRMS_unimpaired_flow_all_model_nodes.csv")

#filter to only gaged nodes
flow.gages.prms <- flow.all[flow.all$model_ID %in% lookup.gages$model_ID,]

#write csv
write.csv(flow.gages.prms, file="C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Gage_delineations/All_gages_unimpaired_flow.csv")
