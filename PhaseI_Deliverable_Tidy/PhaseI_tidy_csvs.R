#'  Cannabis SWB Phase I - Data Deliverable Tidy Script
#'  
#'  This script tidy's the original csv deliverables for SFE LOIs (from Paradigm) into combined longer table format
#'  Integrates file name info into data tables and combines all LOI data into one csv
#'  
#'  Subtask deliverables: 
#'    3.1.2 Natural FFMs for SFE LOIs
#'    3.1.3.A Flow ecology relationships for key eco management goals
#'      Flow reduction scenarios
#'      Type I curves S Coast data
#'      Type I ecorisk curves S Coast data
#'      Type II curves
#'      Type II ecorisk curves
#'      
#'@author Kris Taniguchi-Quan, SCCWRP
#'
############################################################################################################################

#load libraries
#install.packages("tidyverse")
#install.packages("dplyr")
library(tidyverse)
library(dplyr)


#data directories (location where csv deliverables are saved - change to your local directory for each folder)
  #ffm_dir <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Deliverables_FTP/Phase1_3.1/Phase1_3.1/3.1.2 Geodata LOI Natural FFMs/SFE_LOI_Natural_FFMs/"
  #curves_dir <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Deliverables_FTP/Phase1_3.1/Phase1_3.1/3.1.3.A Library of Flow-Ecology Curves/"
ffm_dir <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Deliverables_FTP/Phase1_3.1/Phase1_3.1/3.1.2 Geodata LOI Natural FFMs/SFE_LOI_Natural_FFMs/"
curves_dir <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Deliverables_FTP/Phase1_3.1/Phase1_3.1/3.1.3.A Library of Flow-Ecology Curves/"


############################################################################################################################
## Tidying 3.1.2 Natural FFMs for SFE LOIs (3.1.2 Geodata LOI Natural FFMs/SFE_LOI_Natural_FFMs)

#list all files in ffm_dir
ffm.files <- list.files(ffm_dir)
#get the long file name (with directory, will use this to read in csvs)
ffm.file.lng <- list.files(ffm_dir, full.names = TRUE)
#ignore the first csv file (we will not combine _functional_flow_metrics_lookup_table.csv) for both file name and long file name
ffm.files2 <- ffm.files[2:length(ffm.files)]
ffm.file.lng2 <- ffm.file.lng[2:length(ffm.file.lng)]

#first file is the FFM lookup table, will merge with output df to get FFM names and units
#read in first file lookup table (will left join to output df later)
ffm.lookup <- read.csv(ffm.file.lng[1])

### loop to read in the csv files, pivot longer, get siteID, and save in output df

#set output data frame with appropriate rows and columns in final output created (Year, FFM, Value, siteID)
output.df.ffm <- data.frame(matrix(NA, nrow=1, ncol=4))
#set names of columns in output df
names(output.df.ffm) <- c("Year", "FFM", "Value", "siteID")

#can set i to 1 and test the loop, just skip the for line and run lines inside of loop
i <- 1

#loop --> iterate from 1 to length of csv files reading in
for(i in 1:length(ffm.files2)) {
  #find siteID for csv i which is first part of file name, separate string file name by "__", take first element (if need second element use [[1]][2])
  siteID.i <- strsplit(ffm.files2[i], split="__")[[1]][1]
  
  #read in csv i, don't check column names (allows col names to be numbers)
  ffm.i <- read.csv(ffm.file.lng2[i], check.names = F)
  #replace all "None" to NA
  ffm.i <- data.frame(lapply(ffm.i, function(x) {gsub("None", NA, x)}), check.names = FALSE) 
  
  #pivot longer all columns except first column, should only be years listed
  cols.to.piv <- as.character(names(ffm.i)[2:length(names(ffm.i))])
  #set all value columns to numeric (exclude first col)
  ffm.i[,2:length(names(ffm.i))] <- sapply(ffm.i[,2:length(names(ffm.i))],as.numeric)
  #check class of each column (all cols need to be same class in order to pivot_longer/merge)
  sapply(ffm.i, class)
  
  #pivot_longer the cols.to.piv
  pivot.dat <- data.frame(pivot_longer(ffm.i, cols = cols.to.piv))
  #rename cols
  names(pivot.dat) <- c("FFM", "Year", "Value")
  #check col names are correct
  head(pivot.dat)
  #add siteID.i as column
  pivot.dat$siteID <- rep(siteID.i, length(pivot.dat$FFM))
  
  #save into output df
  output.df.ffm <- rbind(output.df.ffm, pivot.dat)
  
}

#remove the first NA row
output.df.ffm2 <- output.df.ffm[2:length(output.df.ffm$Year),]

#left join with ffm lookup table to get metric info and units
output.df.ffm.join <- output.df.ffm2 %>% 
  left_join(ffm.lookup, by=c("FFM"="Code"))

#write csv the final output.df.ffm.join, save into original directory where csvs were saved
out.csv.fname <- "SFE_LOI_Natural_FFM_annual_results.csv"
#write csv to original directory with output.csv.fname pasted to it
write.csv(output.df.ffm.join, paste0(ffm_dir, out.csv.fname), row.names = FALSE)


############################################################################################################################
## Tidying 3.1.3.A Flow ecology relationships for key eco management goals
## Flow reduction scenarios




#example code for getting list of water year date, assuming the csv i you read in is called flow.data.i and year column is called year (may need to change)
water.yr.i <- unique(flow.data.i$year)
water.yr.start <- as.numeric(water.yr.i) - 1 #previous year which is start of WY
#create start date as character, set as date
start.date <- as.POSIXct(paste0("10/01/", as.character(water.yr.start)))
#create end date as character, set as date
end.date <- as.POSIXct(paste0("09/30/", as.character(water.yr.i)))
#list of dates from start to end date
date.long <- seq(start.date,end.date, by="days",  format="%m-%d-%Y")
#list dates as short format (excluding time)
date <- format(date.long, "%m/%d/%Y")
#append date with water.year.day assuming the water year day column you make is called water.year.day
wyd.date.lookup <- data.frame(cbind(flow.data.i$water.year.day, date))
#after you pivot longer flow.data.i, you can left_join pivoted data with wyd.date.lookup by = "water.year.day" to get the date column attached to pivoted data
#end of chunk to copy into loop

###extra code just for you to run to see how above lines work, delete after you test
start.date <- as.POSIXct("10/01/1996", format="%m/%d/%Y")
#create end date as character, set as date
end.date <- as.POSIXct("09/30/1997", format="%m/%d/%Y")
#list of dates from start to end date
date.long <- seq(start.date,end.date, by="days",  format="%m-%d-%Y")
#list dates as short format (excluding time)
date <- format(date.long, "%m/%d/%Y")

############################################################################################################################
## Tidying 3.1.3.A Flow ecology relationships for key eco management goals
##  TypeII_Curves < Riffle_Depth


#update directory
typeII.dir <- paste0(curves_dir, "/TypeII_Curves/Riffle_Depth/")
#list all files in curves_dir
typeII.files <- list.files(typeII.dir)
#get the long file name (with directory, will use this to read in csvs)
typeII.file.lng <- list.files(typeII.dir, full.names = TRUE)

### loop to read in the csv files, pivot longer, get siteID, and save in output df

#set output data frame with appropriate rows and columns in final output created (Year, typeII, Value, siteID)
output.df.typeII <- data.frame(matrix(NA, nrow=1, ncol=5))
#set names of columns in output df
names(output.df.typeII) <- c("siteID", "Distance","Thalweg","Q_cfs","Depth")

#can set i to 1 and test the loop, just skip the for line and run lines inside of loop
i <- 1

#loop --> iterate from 1 to length of csv files reading in
for(i in 1:length(typeII.files)) {
  
  #get important info from file name i
  #find siteID for csv i which is first part of file name, separate string file name by "__", take first element (if need second element use [[1]][2])
  siteID.i <- strsplit(typeII.files[i], split="__")[[1]][1]

  #read in csv i, don't check column names (allows col names to be numbers)
  typeII.i <- read.csv(typeII.file.lng[i], check.names = F)
  
  #get Q columns (will replace col names)
  qcols.i <- as.numeric(as.character(names(typeII.i)[4:length(names(typeII.i))]))
  #create Q column names
  list.i <- 1:length(qcols.i)
  Qlist.i <- paste0("Q", list.i)
  #create lookup table to use later on
  qlookup.i <- data.frame(cbind(qcols.i, Qlist.i))
  #rename typeII.i
  names(typeII.i)[4:length(names(typeII.i))] <- Qlist.i
  names(typeII.i)
  
  #pivot longer all columns except first column, should only be years listed
  cols.to.piv <- names(typeII.i)[4:length(names(typeII.i))]
  #set all value columns to numeric (exclude first col)
  typeII.i[,2:length(names(typeII.i))] <- sapply(typeII.i[,2:length(names(typeII.i))],as.numeric)
  #check class of each column (all cols need to be same class in order to pivot_longer/merge)
  sapply(typeII.i, class)
  
  #pivot_longer the cols.to.piv but exclude the first column without header
  pivot.dat <- typeII.i[,2:length(names(typeII.i))] %>% 
    pivot_longer(cols = cols.to.piv) %>% 
    data.frame()
  #rename cols
  names(pivot.dat) <- c("Distance", "Thalweg", "Qind", "Depth")
  #check col names are correct
  head(pivot.dat)
  
  #find Q rate associated with Qind
  pivot.dat2 <- pivot.dat %>% 
    left_join(qlookup.i, by=c("Qind"="Qlist.i"), ) %>% 
    rename(Q_cfs=qcols.i)
 
  #add siteID.i as column
  pivot.dat2$siteID <- rep(siteID.i, length(pivot.dat2$Distance))
  
  #reorganize columns and remove Qind column just need Q_cfs
  pivot.dat2 <- pivot.dat2 %>% 
    select("siteID", "Distance","Thalweg","Q_cfs","Depth")
  
  names(pivot.dat2)
  
  #save into output df
  output.df.typeII <- rbind(output.df.typeII, pivot.dat2)
  
}

#remove the first NA row
output.df.typeII2 <- output.df.typeII[2:length(output.df.typeII$siteID),]


#write csv the final output.df.ffm.join, save into original directory where csvs were saved
out.csv.fname <- "thalweg_depths_SFE_LOIs.csv"
#write csv to original directory with output.csv.fname pasted to it
write.csv(output.df.typeII2, paste0(typeII.dir, out.csv.fname), row.names = FALSE)






############################################################################################################################
## Tidying 3.1.3.A Flow ecology relationships for key eco management goals
## Type II ecorisk curves
## workflow: read in all csv files, get important info from file name, replace blanks with NAs, pivot longer, add cols


#update directory
ecorisk.dir <- paste0(curves_dir, "/TypeII_EcoRisk_Curves/")
#list all files in curves_dir
ecorisk.files <- list.files(ecorisk.dir)
#get the long file name (with directory, will use this to read in csvs)
ecorisk.file.lng <- list.files(ecorisk.dir, full.names = TRUE)

### loop to read in the csv files, pivot longer, get siteID, and save in output df

#set output data frame with appropriate rows and columns in final output created (Year, ecorisk, Value, siteID)
output.df.ecorisk <- data.frame(matrix(NA, nrow=1, ncol=8))
#set names of columns in output df
names(output.df.ecorisk) <- c("Diversion","Year","Value","ValueType","Species","Lifestage","Need","siteID")

#can set i to 1 and test the loop, just skip the for line and run lines inside of loop
i <- 1

#loop --> iterate from 1 to length of csv files reading in
for(i in 1:length(ecorisk.files)) {
  
  #get important info from file name i
  #find siteID for csv i which is first part of file name, separate string file name by "__", take first element (if need second element use [[1]][2])
  siteID.i <- strsplit(ecorisk.files[i], split="__")[[1]][1]
  #get species
  species.i <- strsplit(ecorisk.files[i], split="__")[[1]][2]
  #get life stage
  lifestage.i <- strsplit(ecorisk.files[i], split="__")[[1]][3]
  #get need (habitat or passage)
  need.i <- strsplit(ecorisk.files[i], split="__")[[1]][4]
  #get need, remove .csv
  valuetype.i <- gsub(".csv",  "", strsplit(ecorisk.files[i], split="__")[[1]][5])
  
  #read in csv i, don't check column names (allows col names to be numbers)
  ecorisk.i <- read.csv(ecorisk.file.lng[i], check.names = F)

  #pivot longer all columns except first column, should only be years listed
  cols.to.piv <- as.character(names(ecorisk.i)[2:length(names(ecorisk.i))])
  #set all value columns to numeric (exclude first col)
  ecorisk.i[,2:length(names(ecorisk.i))] <- sapply(ecorisk.i[,2:length(names(ecorisk.i))],as.numeric)
  #check class of each column (all cols need to be same class in order to pivot_longer/merge)
  sapply(ecorisk.i, class)
  
  #pivot_longer the cols.to.piv
  pivot.dat <- data.frame(pivot_longer(ecorisk.i, cols = cols.to.piv))
  #rename cols
  names(pivot.dat) <- c("Diversion", "Year", "Value")
  #check col names are correct
  head(pivot.dat)
  
  
  #add siteID.i as column
  pivot.dat$ValueType <- rep(valuetype.i, length(pivot.dat$Diversion))
  #add siteID.i as column
  pivot.dat$Species <- rep(species.i, length(pivot.dat$Diversion))
  #add siteID.i as column
  pivot.dat$Lifestage <- rep(lifestage.i, length(pivot.dat$Diversion))
  #add siteID.i as column
  pivot.dat$Need <- rep(need.i, length(pivot.dat$Diversion))
  #add siteID.i as column
  pivot.dat$siteID <- rep(siteID.i, length(pivot.dat$Diversion))
  
  names(pivot.dat)
  
  
  #save into output df
  output.df.ecorisk <- rbind(output.df.ecorisk, pivot.dat)
  
}

#remove the first NA row
output.df.ecorisk2 <- output.df.ecorisk[2:length(output.df.ecorisk$Year),]


#write csv the final output.df.ffm.join, save into original directory where csvs were saved
out.csv.fname <- "spp_lifestage_hab_passage_rel_baseline_SFE_LOIs.csv"
#write csv to original directory with output.csv.fname pasted to it
write.csv(output.df.ecorisk2, paste0(ecorisk.dir, out.csv.fname), row.names = FALSE)







############################################################################################################################
## Tidying 3.1.3.A Flow ecology relationships for key eco management goals
## Type II ecorisk curves (last set of deliverables to tidy)


