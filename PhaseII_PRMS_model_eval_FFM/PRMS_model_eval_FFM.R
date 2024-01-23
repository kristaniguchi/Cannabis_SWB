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

{
  #load libraries and install if necessary
  library("ggplot2")
  library("dplyr")
  library("tidyverse")
  # if (!require(devtools)) install.packages("devtools")
  # library(devtools)
  # install_github("hzambran/hydroGOF")
  library("hydroGOF")
  #install.packages("ztable")
  library("ztable")
  library("glue")
  library("scales")
}

#data directories (location where csv files are saved - change to your local directory for each folder)
FFM_dir <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/FFC_outputs/Eel_River/csv_results/"

############################################################################################################################
## Tidying Eel River modeled and gaged functional flow metric values, only keep gage and associated model node FFM

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
# #can delete line below later, but only using KTQ files since original ER gage data need to be reformatted
# ind.KTQ <- grep("_KTQ", list.files.ER)
# list.file.ER2 <- list.files.ER

#read in gaged data
ind.gage.ER <- grep("_gage_", list.files.ER)
gage.ffm.ER <- read.csv(list.files.ER[ind.gage.ER]) %>% 
  #remove rows where value is NA
  na.omit() %>% 
  #create new column of model_ID and FFM
  mutate(gage_ID_FFM = paste0(gage_ID, " ", FFM))

#count number of ffm values per gage, we will later filter list to sites with at least 10 years of FFM data
gage.ffm.years <- gage.ffm.ER %>% 
  group_by(gage_ID, FFM) %>% 
  count() %>% 
  #filter(n>10) %>% 
  #create new column of model_ID and FFM
  mutate(gage_ID_FFM = paste0(gage_ID, " ", FFM))

#read in model data
ind.model.ER <- grep("_model_", list.files.ER)
model.ffm.ER <- read.csv(list.files.ER[ind.model.ER]) %>% 
  #remove rows where value is NA
  na.omit() %>% 
  #create new column of model_ID and FFM
  mutate(model_ID_FFM = paste0(model_ID, " ", FFM)) 



#find unique gageIDs
unique.gages <- unique(gage.ffm.ER$gage_ID)
#find associated model_IDs for the unique.gages (these are model_IDs we want to keep in FFM data)
unique.model_ID <- lookup.ER$model_ID[lookup.ER$Gage.ID %in% unique.gages] 

#subset to only model_IDs that have gages
model.ffm.ER.sub <- model.ffm.ER[as.character(model.ffm.ER$model_ID) %in% unique.model_ID,]

#count number of ffm values per model node, we will later filter list to sites with at least 10 years of FFM data
model.ffm.years <- model.ffm.ER.sub %>% 
  group_by(model_ID, FFM) %>% 
  count() %>% 
  #filter(n>10) %>% 
  #create new column of model_ID and FFM
  mutate(model_ID_FFM = paste0(model_ID, " ", FFM))


#calculate predicted percentiles from annual FFM PRMS data --> used for step 1 below
pred.percentiles <- model.ffm.ER.sub %>%
  group_by(model_ID, FFM) %>% 
  mutate(p10 = quantile(Value, probs = 0.1),
         p25 = quantile(Value, probs = 0.25),
         p50 = quantile(Value, probs = 0.5),
         p75 = quantile(Value, probs = 0.75),
         p90 = quantile(Value, probs = 0.9),
         model_ID_FFM = paste(model_ID, FFM, sep=" ")) %>% 
  select(model_ID, COMID, FFM, p10, p25, p50, p75, p90, model_ID_FFM) %>% 
  #remove duplicate rows 
  distinct() %>% 
  #add n number of metric values for each model_ID_FFM
  left_join(model.ffm.years, by=c("model_ID", "FFM", "model_ID_FFM")) %>% 
  #add gage_ID and gage_ID_FFM from lookup table, rename to gage_ID, add gage_ID_FFM, and select only columns needed
  left_join(lookup.ER, by = "model_ID") %>% 
  rename("gage_ID" = "Gage.ID") %>% 
  mutate(gage_ID_FFM = paste0(gage_ID, " ",FFM)) %>% 
  select(model_ID, COMID, FFM, p10, p25, p50, p75, p90, model_ID_FFM, gage_ID, gage_ID_FFM)
  


############################################################################################################################
##### Model evaluation: 
## 1. evaluate dispersion - % of annual observed FFM values (from gage) that falls withing predicted IQR (p25-75) and and IQR (p10-90) for each FFM (from PRMS model node)
      ##for IQR (25th-75th percentile values) and I80R (10th-90th percentile values), expected that 50% and 80% of observed/gaged FFM values fall within these ranges
## 2. evaluate annual FFM values - compared annual FFM modeled value to annual FFM observed/gage value
    #2a. O/E: mean O/E was calculated for each gage and flow metric
    #2b. R2
    #2c. PBIAS   
    #2d. NSE  
## Note: could not evaluate performance for metrics that have <10 values calculated at a gage including certain peak flow metrics since only 1 value calculated across POR
#####

### 1. Evaluate dispersion
#loop to calculate % of observed values that fall within IQR (Q10-90 and Q25-50)


######### update code from here
#create output df of all evaluation criteria per FFM per gage
eval.criteria.all <- data.frame(matrix(NA, 1, 8))
names(eval.criteria.all) <- c("gage_ID", 'model_ID', 'comid', 'FFM', 'I80R_10_90', 'IQR_25_75', "n_values", "n_values_pred")

#unique FFMs to go through
unique.gageid.ffm <- unique(gage.ffm.ER$gage_ID_FFM)

#to test loop, run i=1 outside of loop and run everything inside
i=1

for(i in 1:length(unique.gageid.ffm)) {
  #subset to unique.gageid.ffm i, omit rows with NA
  gage.ffm.ER.i <- gage.ffm.ER %>% 
    filter(gage_ID_FFM == unique.gageid.ffm[i]) %>% 
    na.omit()
  
  #if all NA values or less than 10 values, skip
  if(length(gage.ffm.ER.i$value) < 10){
    #skip
  }else{
    #FFM 
    FFM <- unique(gage.ffm.ER.i$FFM)
    #gage id
    gage.id.i <- unique(gage.ffm.ER.i$gage_ID)
    
    #subset predicted IQR for FFM
    pred.percentiles.i <- pred.percentiles %>% 
      filter(gage_ID_FFM == unique.gageid.ffm[i])
    
    #1. evaluate dispersion (% of values within IQR and I80R) excluding calculations for peak mag metrics (1 value per gage)
    
    #if peak magnitude metrics, IQR and I80R = NA (since only 1 value). Else, calculate IQR and I80R
    if(identical(FFM,"Peak_2") | identical(FFM,"Peak_5") | identical(FFM,"Peak_5") ){
      I80R_10_90 <- NA
      IQR_25_75 <- NA
      length_n <- 1 #duplicate mag metrics with 1 value. 
      length_n_pred <- 1 #duplicate mag metrics with 1 value.
    }else{
      #calculate percent of obs values that fall within I80R (10-90) and IQR_25_75
      length_n <- length(gage.ffm.ER.i$Value)
      #check number of predicted ffm values
      length_n_pred <- model.ffm.years$n[model.ffm.years$model_ID_FFM == pred.percentiles.i$model_ID_FFM]
      #if < 10 FFM values for predicted data, set as NA
      if(length_n_pred<10){
        I80R_10_90 <- NA
        IQR_25_75 <- NA
      }else{
        sum.in.I80R <- nrow(gage.ffm.ER.i[gage.ffm.ER.i$Value >= pred.percentiles.i$p10 & gage.ffm.ER.i$Value <= pred.percentiles.i$p90,])
        sum.in.IQR_25_75 <- nrow(gage.ffm.ER.i[gage.ffm.ER.i$Value >= pred.percentiles.i$p25 & gage.ffm.ER.i$Value <= pred.percentiles.i$p75,])
        I80R_10_90 <- sum.in.I80R/length_n*100
        IQR_25_75 <- sum.in.IQR_25_75/length_n*100
      }
    }
    
    #create output row ###FIX THIS PART!
    out.row <- c(gage.ffm.ER.i$gage_ID[1], pred.percentiles.i$model_ID, gage.ffm.ER.i$COMID[1], FFM, I80R_10_90, IQR_25_75, length_n, length_n_pred)
    eval.criteria.all <- rbind(eval.criteria.all, out.row)
  }
}

#make a copy of eval.criteria.all
eval.criteria.all.orig <- eval.criteria.all

#if n_values < 10, put NA for percent IQR values and O_E values, not enough values to assess
#set n_values as numeric
eval.criteria.all$n_values <- as.numeric(eval.criteria.all$n_values)
#set I80R_10_90 as NA if < 10 values
eval.criteria.all$I80R_10_90[eval.criteria.all$n_values < 10] <- NA
#set IQR_25_75 as NA if < 10 values
eval.criteria.all$IQR_25_75[eval.criteria.all$n_values < 10] <- NA
#set O_E as NA if < 10 values
eval.criteria.all$O_E[eval.criteria.all$n_values < 10] <- NA

#for peak magnitude metrics, use O_E value for each gage (okay that n<10 because not using median O_E)
#find index of peak mag metrics
ind.peak <- grep("d_peak", eval.criteria.all$FFM)
eval.criteria.all$O_E[ind.peak] <- as.numeric(eval.criteria.all$median.obs[ind.peak])/as.numeric(eval.criteria.all$median.pred[ind.peak])

#filter evaluation criteria for csv to read
#keep evaluation criteria for peaks (okay that n<10 because not using median O_E)
eval.criteria.peak <- eval.criteria.all[ind.peak,]
#filter out rows that have n<10
eval.criteria.all.filter <- eval.criteria.all %>% 
  filter(n_values > 9) %>% 
  #combine with eval.criteria.peak
  bind_rows(eval.criteria.peak)

#summarize number of testing gages per metric
summary.testing <- eval.criteria.all.filter %>% 
  group_by(FFM) %>% 
  count() %>% 
  ungroup()

#write csv for eval.criteria.all.filter to know which gages are testing gages for each FFM
write.csv(eval.criteria.all.filter, file=glue("./data/output_05/final_testing_gages_used_all_FFM_test12_sametest11_allgages_{Sys.Date()}.csv"))


###reminder: need to add O/E in next chunk of code
