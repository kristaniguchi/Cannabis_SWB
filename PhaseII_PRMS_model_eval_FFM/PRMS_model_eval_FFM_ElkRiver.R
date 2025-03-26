#'  Cannabis SWB Phase II - PRMS watershed model evaluation at the functional flow metric level
#'  
#'  This script takes FFMs calculated from gaged and observed data and evaluates model performance for every gage
#'  Model performance by FFM following methods by Grantham et al. (2022) and Moriasi et al. (2007)
#'  
#'  PRMS watershed model flow timeseries data for:
#'    Elk River
#'    
#'    Notes for Elk River:
#'      	Calibration (reference) gage is 11479700. Used regression equation to predict unimpaired flows from historical data. Calibrated off of predicted flows.
#'       Calibration period WY 1/1/1990-12/31/2010, validation 1/1/2011-12/31/2020
#'      
#'      
#'@author Kris Taniguchi-Quan, SCCWRP
#'
############################################################################################################################

#install package to combine two geom_tile figures together (since only one gage)
#install.packages("patchwork")



{
  #load libraries and install if necessary
  library(patchwork)
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
#Elk River  recalibration
FFM_dir <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/FFC_outputs/Elk_River_rev2/csv_results/"

#set working directory to FFM_dir
setwd(FFM_dir)

#read in FFM lookup table for plots to get official names - saved in working directory 2 folders back
metric.names <- read.csv("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/FFC_outputs/all_metric_def_list_FFMs_v2.csv")


############################################################################################################################
## Tidying Elk River modeled and gaged functional flow metric values, only keep gage and associated model node FFM

#read in lookup table that has gage_ID and model_ID
lookup.ER <- read.csv(file="C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Eel_River/Lookup_Tables/Gage_PRMS_Subbasin_Lookup.csv") %>% 
  #filter only to Elk
  filter(Model_abbrev == "ELR") %>% 
  #create col with model_ID
  mutate(model_ID = paste0("ELR_", PRMS.Subbasin))


#list all files in FFM_dir
list.files.all <- list.files(FFM_dir, full.names=TRUE)

#read in gaged data
ind.gage.ER <- grep("_gage_", list.files.all)
gage.ffm.ER <- read.csv(list.files.all[ind.gage.ER]) %>% 
  #remove rows where value is NA
  na.omit() %>% 
  #create new column of model_ID and FFM
  mutate(gage_ID_FFM = paste0(gage_ID, " ", FFM))

#only evalute performance at the reference gage, validation years
#for 1 reference gage, remove calibration years <=2010 to only look at ref validation period
gage.ffm.ER <- gage.ffm.ER[! ((gage.ffm.ER$gage_ID == 11479700) & (gage.ffm.ER$Year<2011)),]

#count number of ffm values per gage, we will later filter list to sites with at least 10 years of FFM data
gage.ffm.years <- gage.ffm.ER %>% 
  group_by(gage_ID, FFM) %>% 
  count() %>% 
  #filter(n>10) %>% 
  #create new column of model_ID and FFM
  mutate(gage_ID_FFM = paste0(gage_ID, " ", FFM))

#read in model data
ind.model.ER <- grep("_model_", list.files.all)
model.ffm.ER <- read.csv(list.files.all[ind.model.ER]) %>% 
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

#only evalute performance at the reference gage, validation years
#for 1 reference gage, remove calibration years <=2010 to only look at ref validation period
model.ffm.ER.sub <- model.ffm.ER.sub[! ((model.ffm.ER.sub$Year<2011)),]


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
## Note: could not evaluate performance for metrics that have <10 values calculated at a gage or model node including certain peak flow metrics since only 1 value calculated across POR
#####

### 1. Evaluate dispersion
#loop to calculate % of observed values that fall within IQR (Q10-90 and Q25-50)


######### update code from here
#create output df of all evaluation criteria per FFM per gage
eval.criteria.all <- data.frame(matrix(NA, 1, 8))
names(eval.criteria.all) <- c("gage_ID", 'model_ID', 'COMID', 'FFM', 'I80R_10_90', 'IQR_25_75', "n_values", "n_values_pred")

#unique FFMs to go through
unique.gageid.ffm <- unique(gage.ffm.ER$gage_ID_FFM)

#to test loop, run i=1 outside of loop and run everything inside
i=2

for(i in 1:length(unique.gageid.ffm)) {
  #subset to unique.gageid.ffm i, omit rows with NA
  gage.ffm.ER.i <- gage.ffm.ER %>% 
    filter(gage_ID_FFM == unique.gageid.ffm[i]) %>% 
    na.omit()
  
  #if all NA values or less than 10 values, skip
  if(length(gage.ffm.ER.i$Value) < 10){
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
    if(identical(FFM,"Peak_2") | identical(FFM,"Peak_5") | identical(FFM,"Peak_10") ){
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

#remove first NA row
eval.criteria.all <- eval.criteria.all[2:length(eval.criteria.all$gage_ID),]

#make a copy of eval.criteria.all
eval.criteria.all.orig <- eval.criteria.all

#if n_values < 10 for gages or model, put NA for percent IQR values, not enough values to assess
#set n_values as numeric for gages
eval.criteria.all$n_values <- as.numeric(eval.criteria.all$n_values)
#set I80R_10_90 as NA if < 10 values
eval.criteria.all$I80R_10_90[eval.criteria.all$n_values < 10] <- NA
#set IQR_25_75 as NA if < 10 values
eval.criteria.all$IQR_25_75[eval.criteria.all$n_values < 10] <- NA
#set n_values as numeric for model
eval.criteria.all$n_values_pred <- as.numeric(eval.criteria.all$n_values_pred)
#set I80R_10_90 as NA if < 10 values
eval.criteria.all$I80R_10_90[eval.criteria.all$n_values_pred < 10] <- NA
#set IQR_25_75 as NA if < 10 values
eval.criteria.all$IQR_25_75[eval.criteria.all$n_values_pred < 10] <- NA

#filter evaluation criteria for csv to read
#filter out rows that have n<10 for gages and model nodes
eval.criteria.all.filter <- eval.criteria.all %>% 
  filter(n_values > 9) %>% 
  filter(n_values_pred > 9) %>% 
  #create gage_ID_FFM column
  mutate(gage_ID_FFM = paste0(gage_ID, " ", FFM))

#summarize number of gages per metric
summary.testing <- eval.criteria.all.filter %>% 
  group_by(FFM) %>% 
  count() %>% 
  ungroup()


##################################################################################
## 2. evaluate annual FFM values - compared annual FFM modeled value to annual FFM observed/gage value
#2a. O/E: mean O/E was calculated for each gage and flow metric
#2b. R2
#2c. PBIAS   
#2d. NSE  

#loop through each gage_ID_FFM and calculate O:E, R2, PBIAS, NSE for pred and obs annual values
unique.gage.ffm <- unique(eval.criteria.all.filter$gage_ID_FFM)

#create output df of all performance criteria per FFM 
perf.criteria.FFM <- data.frame(matrix(NA, 1, 18))
names(perf.criteria.FFM) <- c("gage_ID", "model_ID", "Type2", "FFM", "composite_index", "composite_index_disp", "IQR_25_75", "I80R_10_90", "PBIAS", "NSE", "mean_O_E", "R2", "PBIAS_scaled", "IQR_25_75_scaled", "I80R_10_90_scaled", "NSE_scaled", "mean_O_E_scaled", "n_annual_gage_model")


for(j in 1:(length(unique.gage.ffm))){
  #subset eval.criteria.all to unique.gage.ffm j
  eval.criteria.all.sub <- eval.criteria.all.filter %>% 
    filter(gage_ID_FFM == unique.gage.ffm[j])
  
  #subset gage data for unique.gage.ffm j
  gage.ffm.ER.sub <- gage.ffm.ER %>% 
    filter(gage_ID_FFM == unique.gage.ffm[j])
  
  #subset model data for unique.gage.ffm j
  model.ffm.ER.sub.j <- model.ffm.ER.sub %>% 
    #add gage_ID and gage_ID_FFM from lookup table, rename to gage_ID, add gage_ID_FFM, and select only columns needed
    left_join(lookup.ER, by = "model_ID") %>% 
    rename("gage_ID" = "Gage.ID") %>% 
    mutate(gage_ID_FFM = paste0(gage_ID, " ",FFM)) %>% 
    #filter by unique.gage.ffm j
    filter(gage_ID_FFM == unique.gage.ffm[j]) %>% 
    select(Year, FFM, Value, model_ID, scenario, COMID, model_ID_FFM, gage_ID, Type2, gage_ID_FFM) %>% 
    #rename Value to value_model
    rename("Value_model"= "Value")
  
  #combine gage and model data together into common df by year
  gage.model.FFM.join <- gage.ffm.ER.sub %>% 
    inner_join(model.ffm.ER.sub.j, by=c("Year", "FFM", "gage_ID", "COMID", "gage_ID_FFM"))
  
  #determine n years in common --> should have at least 10 to move forward
  n_annual_gage_model <- length(gage.model.FFM.join$Year)
  
  #if <10 years of annual data in common, can only look at dispersion, not enough data to look at annual performance
  if(n_annual_gage_model < 10){
    #only have enough data to look at dispersion
    #IQR scaled: take absolute value from diff between calculated value and 50 divided by 50 and subtracted by 1
    IQR_25_75_scaled <- 1-(abs(as.numeric(eval.criteria.all.sub$IQR_25_75)-50)/50)
    I80R_10_90_scaled <- 1-(abs(as.numeric(eval.criteria.all.sub$I80R_10_90)-80)/80)
    
    #composite performance index (mean of all 6 criteria)
    composite_index_all <- NA
    composite_index_disp <- mean(c(I80R_10_90_scaled, IQR_25_75_scaled), na.rm=TRUE)
    
    #save all values in output for evaluation (raw, scaled, composite) --> for annual metrics, set as NA
    perf.criteria.FFM[j,] <- c(eval.criteria.all.sub$gage_ID, eval.criteria.all.sub$model_ID, unique(model.ffm.ER.sub.j$Type), eval.criteria.all.sub$FFM, composite_index_all, composite_index_disp, eval.criteria.all.sub$IQR_25_75, eval.criteria.all.sub$I80R_10_90, NA, NA, NA, NA, NA, IQR_25_75_scaled, I80R_10_90_scaled, NA, NA, n_annual_gage_model)
    
  }else{
    
    ###Calculate suite of metrics for annual FFM evaluation
    #calculate O:E and mean O/E across all years
    gage.model.FFM.join <- gage.model.FFM.join %>% 
      mutate(O_E = Value_model/Value)
    
    #mean O/E, raw value
    mean_O_E <- mean(as.numeric(gage.model.FFM.join$O_E), na.rm = TRUE)
    
    #R2
    rsq <- function(x, y) summary(lm(y~x))$r.squared
    R2 <- rsq(as.numeric(gage.model.FFM.join$Value), as.numeric(gage.model.FFM.join$Value_model))
    
    #percent bias
    PBIAS <- pbias(as.numeric(gage.model.FFM.join$Value), as.numeric(gage.model.FFM.join$Value_model))
    
    #NSE: Nash-Sutcliffe Efficiency
    NSE <- NSE(as.numeric(gage.model.FFM.join$Value), as.numeric(gage.model.FFM.join$Value_model))
    
    ###scaled evaluation criteria all from 0 to 1
    
    #O/E scaled, for values >1, take the inverse (1/O_E)
    O_E_scaled <- na.omit(as.numeric(gage.model.FFM.join$O_E))
    #for all values >1, take inverse 1/O:E
    O_E_scaled[O_E_scaled > 1] <- 1/O_E_scaled[O_E_scaled > 1]
    #mean O/E scaled
    mean_O_E_scaled <- mean(O_E_scaled)
    
    #NSE scaled, if values < 0 set to 0
    if(NSE<0) {
      NSE_scaled <- 0
    }else{
      NSE_scaled <- NSE
    }
    
    #IQR scaled: take absolute value from diff between calculated value and 50 divided by 50 and subtracted by 1
    IQR_25_75_scaled <- 1-(abs(as.numeric(eval.criteria.all.sub$IQR_25_75)-50)/50)
    I80R_10_90_scaled <- 1-(abs(as.numeric(eval.criteria.all.sub$I80R_10_90)-80)/80)
    
    #percent bias scaled
    PBIAS_scaled <- (100-abs(PBIAS))/100
    #if scaled is negative, means that original was over 100
    if(PBIAS_scaled < 0){
      PBIAS_scaled <- 0
    }
    
    #composite performance index (mean of all 6 criteria)
    composite_index_all <- mean(c(PBIAS_scaled, I80R_10_90_scaled, IQR_25_75_scaled, NSE_scaled, O_E_scaled, R2), na.rm=TRUE)
    composite_index_disp <- mean(c(I80R_10_90_scaled, IQR_25_75_scaled), na.rm=TRUE)
    
    #save all values in output for evaluation (raw, scaled, composite)
    perf.criteria.FFM[j,] <- c(eval.criteria.all.sub$gage_ID, eval.criteria.all.sub$model_ID, unique(model.ffm.ER.sub.j$Type), eval.criteria.all.sub$FFM, composite_index_all, composite_index_disp, eval.criteria.all.sub$IQR_25_75, eval.criteria.all.sub$I80R_10_90, PBIAS, NSE, mean_O_E, R2, PBIAS_scaled, IQR_25_75_scaled, I80R_10_90_scaled, NSE_scaled, mean_O_E_scaled, n_annual_gage_model)
    
  }
}

#write the overall performance table so anyone can view the results, save in 1 directory back
write.csv(perf.criteria.FFM, file="../FFM_eval/Model_performance_FFM_summary_all_Elk_rev2.csv")

#tidy performance table for heatmap - composite using all criteria
perf.criteria.FFM.table <- perf.criteria.FFM %>% 
  select(gage_ID, FFM, composite_index, Type2) %>% 
  pivot_wider(names_from = FFM, values_from = composite_index)

#tidy performance table for heatmap - composite using dispersion only
perf.criteria.FFM.table.disp <- perf.criteria.FFM %>% 
  select(gage_ID, FFM, composite_index_disp, Type2) %>% 
  pivot_wider(names_from = FFM, values_from = composite_index_disp)

#round all columns except FFM by 2 digits
perf.criteria.FFM.table[,3:14] <- round(data.frame(lapply(perf.criteria.FFM.table[,3:14],as.numeric)), 2)
perf.criteria.FFM.table.disp[,3:14] <- round(data.frame(lapply(perf.criteria.FFM.table.disp[,3:14],as.numeric)), 2)


##########
#####create heatmap of this table based on the categories for composite index all
#pivot longer table
#find ffm col names to pivot longer
ffm.col.names <- names(perf.criteria.FFM.table)[3:14]
#remove peak timings as not standard FFM
ind.peak.tim <- grep("^Peak_Tim_*", ffm.col.names)
#if peak timing, then remove metric
if(length(ind.peak.tim > 0)){
  ffm.col.names <- ffm.col.names[-ind.peak.tim]
}

ffm_comp_ind_longer <- perf.criteria.FFM.table %>% 
  pivot_longer(cols = ffm.col.names,
               values_to = "Composite_Index",
               names_to = "FFM") %>% 
  left_join(metric.names, by = c("FFM" = "flow_metric"))

# #set levels for Performance Criteria
# ffm_comp_ind_longer$'Performance Criteria' <- factor(ffm_comp_ind_longer$'Performance Criteria', levels = c('IQR_25_75_mean_scaled', 'I80R_10_90_mean_scaled', "mean_O_E_scaled",'PBIAS_scaled', 'R2', 'NSE', "composite_index"))
#create rating for each based on criteria values
ffm_comp_ind_longer$rating <- NA
ffm_comp_ind_longer$rating[ffm_comp_ind_longer$Composite_Index < 0.5] <- "poor"
ffm_comp_ind_longer$rating[ffm_comp_ind_longer$Composite_Index >= 0.5 & ffm_comp_ind_longer$Composite_Index < 0.65] <- "satisfactory"
ffm_comp_ind_longer$rating[ffm_comp_ind_longer$Composite_Index >= 0.65 & ffm_comp_ind_longer$Composite_Index < 0.81] <- "good"
ffm_comp_ind_longer$rating[ffm_comp_ind_longer$Composite_Index >= 0.81 & ffm_comp_ind_longer$Composite_Index < 0.9] <- "very good"
ffm_comp_ind_longer$rating[ffm_comp_ind_longer$Composite_Index >= 0.81 & ffm_comp_ind_longer$Composite_Index < 0.9] <- "very good"
ffm_comp_ind_longer$rating[ffm_comp_ind_longer$Composite_Index >= 0.9] <- "excellent"
#save as factor
ffm_comp_ind_longer$rating <- factor(ffm_comp_ind_longer$rating, levels = c("poor", "satisfactory", "good", "very good", "excellent", NA))

#add an asterix to any validation gage_ID
#find index of validation gages
ind.val <- grep("Validation", ffm_comp_ind_longer$Type2)
#make a copy of Type2
ffm_comp_ind_longer$gage_ID2 <- as.character(ffm_comp_ind_longer$gage_ID)
#set validation gages with asterix
ffm_comp_ind_longer$gage_ID2[ind.val] <- paste0(ffm_comp_ind_longer$gage_ID[ind.val], "*")

#add in subbasins to ffm_comp_ind_longer
ffm_comp_ind_longer2 <- ffm_comp_ind_longer %>% 
  mutate(gage_ID = as.numeric(gage_ID)) %>% 
  left_join(lookup.ER, by = c("gage_ID"= "Gage.ID")) 

#sort gages by reference first, NA

#heatmap using all gages

#create color lookup table
colors <- c("#ff7f00", "#ffff33", "#4daf4a", "#377eb8", "#984ea3", "grey")
rating <- c("poor", "satisfactory", "good", "very good", "excellent", NA)
rating_values <- c("<0.5", "0.5-0.65", "0.65-0.8", "0.81-0.9", ">0.9", NA)
rating_labels <- paste0(rating, " (", rating_values, ")")
rating_labels[6] <- NA
#subset colors to what we have
lookup.cols <- data.frame(cbind(colors, rating, rating_values, rating_labels))
lookup.cols <- lookup.cols[lookup.cols$rating %in% unique(ffm_comp_ind_longer2$rating),]

heatmap <- ggplot(ffm_comp_ind_longer2, aes(y=title_name, x = gage_ID2)) +
  #make heatmap with geom_tile based on criteria values
  geom_tile(aes(fill = rating)) +
  
  #add the criteria values to each tile
  geom_text(aes(label = Composite_Index), color = "black") + 
  #some formatting to make things easier to read
  scale_x_discrete(position = "top") +
  scale_fill_manual(values = lookup.cols$colors, labels = rating_labels) +
  #scale_fill_gradient(high = “#132B43”, low = “#56B1F7”) +
  theme(legend.position = "bottom", 
        legend.box = "horizontal",
        legend.margin = margin(),
        #axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=1),
        axis.text.x = element_text(angle = 90),
        
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank()) +
  labs(y = "Functional Flow Metric ", x = "Gage ID", title = "Composite Index All") #+
  #facet_grid(~HUC8.Subarea, scales = "free_x", space = "free_x")

plot(heatmap)

#write heatmap as jpg
ggsave(heatmap, file="../FFM_eval/Model_performance_FFM_composite_all.jpg", width = 11, height = 5, dpi=300)


############
#####create heatmap of this table based on the categories for composite index for dispersion only
#pivot longer table
#find ffm col names to pivot longer
ffm.col.names <- names(perf.criteria.FFM.table.disp)[3:length(names(perf.criteria.FFM.table.disp))]
#remove peak timings as not standard FFM
ind.peak.tim <- grep("^Peak_Tim_*", ffm.col.names)
#if peak timing, then remove metric
if(length(ind.peak.tim > 0)){
  ffm.col.names <- ffm.col.names[-ind.peak.tim]
}

ffm_comp_ind_longer <- perf.criteria.FFM.table.disp %>% 
  pivot_longer(cols = ffm.col.names,
               values_to = "Composite_Index_Disp",
               names_to = "FFM") %>% 
  left_join(metric.names, by = c("FFM" = "flow_metric")) %>% 
  mutate(gage_ID = as.numeric(gage_ID)) %>%
  left_join(lookup.ER, by = c("gage_ID"= "Gage.ID")) 

# #set levels for Performance Criteria
# ffm_comp_ind_longer$'Performance Criteria' <- factor(ffm_comp_ind_longer$'Performance Criteria', levels = c('IQR_25_75_mean_scaled', 'I80R_10_90_mean_scaled', "mean_O_E_scaled",'PBIAS_scaled', 'R2', 'NSE', "Composite_Index_Disp"))
#create rating for each based on criteria values
ffm_comp_ind_longer$rating <- NA
ffm_comp_ind_longer$rating[ffm_comp_ind_longer$Composite_Index_Disp < 0.5] <- "poor"
ffm_comp_ind_longer$rating[ffm_comp_ind_longer$Composite_Index_Disp >= 0.5 & ffm_comp_ind_longer$Composite_Index_Disp < 0.65] <- "satisfactory"
ffm_comp_ind_longer$rating[ffm_comp_ind_longer$Composite_Index_Disp >= 0.65 & ffm_comp_ind_longer$Composite_Index_Disp < 0.81] <- "good"
ffm_comp_ind_longer$rating[ffm_comp_ind_longer$Composite_Index_Disp >= 0.81 & ffm_comp_ind_longer$Composite_Index_Disp < 0.9] <- "very good"
ffm_comp_ind_longer$rating[ffm_comp_ind_longer$Composite_Index_Disp >= 0.81 & ffm_comp_ind_longer$Composite_Index_Disp < 0.9] <- "very good"
ffm_comp_ind_longer$rating[ffm_comp_ind_longer$Composite_Index_Disp >= 0.9] <- "excellent"
#save as factor
ffm_comp_ind_longer$rating <- factor(ffm_comp_ind_longer$rating, levels = c("poor", "satisfactory", "good", "very good", "excellent", NA))

#add an asterix to any validation gage_ID
#find index of validation gages
ind.val <- grep("Validation", ffm_comp_ind_longer$Type2.x)
#make a copy of Type2
ffm_comp_ind_longer$gage_ID2 <- as.character(ffm_comp_ind_longer$gage_ID)
#set validation gages with asterix
ffm_comp_ind_longer$gage_ID2[ind.val] <- paste0(ffm_comp_ind_longer$gage_ID[ind.val], "*")

#heatmap using all gages

#create color lookup table
colors <- c("#ff7f00", "#ffff33", "#4daf4a", "#377eb8", "#984ea3", "grey")
rating <- c("poor", "satisfactory", "good", "very good", "excellent", NA)
rating_values <- c("<0.5", "0.5-0.65", "0.65-0.8", "0.81-0.9", ">0.9", NA)
rating_labels <- paste0(rating, " (", rating_values, ")")
rating_labels[6] <- NA
#subset colors to what we have
lookup.cols <- data.frame(cbind(colors, rating, rating_values, rating_labels))
lookup.cols <- lookup.cols[lookup.cols$rating %in% unique(ffm_comp_ind_longer$rating),]

heatmap_disp<- ggplot(ffm_comp_ind_longer, aes(y=title_name, x = gage_ID2)) +
  #make heatmap with geom_tile based on criteria values
  geom_tile(aes(fill = rating)) +
  
  #add the criteria values to each tile
  geom_text(aes(label = Composite_Index_Disp), color = "black") + 
  #some formatting to make things easier to read
  scale_x_discrete(position = "top") +
  scale_fill_manual(values = lookup.cols$colors, labels = lookup.cols$rating_labels) +
  #scale_fill_gradient(high = “#132B43”, low = “#56B1F7”) +
  theme(legend.position = "bottom", 
        legend.box = "horizontal",
        legend.margin = margin(),
        #axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=1),
        axis.text.x = element_text(angle = 90),
        
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank()) +
  labs(y = "Functional Flow Metric ", x = "Gage ID", title = "Composite Index Dispersion") #+
  #facet_grid(~HUC8.Subarea, scales = "free_x", space = "free_x")

plot(heatmap_disp)

#write heatmap as jpg
ggsave(heatmap_disp, file="../FFM_eval/Model_performance_FFM_composite_dispersion_all.jpg", width = 11, height = 5, dpi=300)

#combine two geom_tile figures together 

#remove legend on one plot
heatmap_v2 <- heatmap + theme(legend.position = "none")

heatmap_disp2 <- heatmap_disp + theme(legend.position = "right", axis.text.y = element_blank(), axis.title.y = element_blank())

#combine plots
combined_plot <- heatmap_v2 + heatmap_disp2

plot(combined_plot)

#write heatmap as jpg
ggsave(combined_plot, file="../FFM_eval/Model_performance_FFM_composite_all_dispersion_combine.jpg", width = 7.5, height = 5, dpi=300)

