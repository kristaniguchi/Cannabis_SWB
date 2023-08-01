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
