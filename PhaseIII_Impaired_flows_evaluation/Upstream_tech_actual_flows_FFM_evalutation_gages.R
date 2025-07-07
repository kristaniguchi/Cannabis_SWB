#'  Cannabis SWB Phase III - Upstream Tech's actual flow evaluation at the functional flow metric level
#'  
#'  This script takes FFMs calculated from gages (observed data) and evaluates model performance for every gage
#'  Upstream Tech's actual flows were estimated by taking modeled unimpaired flows from PRMS model and subtracting monthly demand data (cumulative point of diversions) for each watershed draining to gages
#'  Model performance by FFM following methods by Grantham et al. (2022) and Moriasi et al. (2007)
#'  Only performance at validation gages or timeperiods that match impaired flows period of record (WY 2016-2021) are evaluated
#'  
#'  Excluding historical gages that don't match impaired flow POR: c(11472800, 11472900, 11472200, 11472150, 11479700)
#'  
#'  For all gages, functional flow metrics were recalculated using the revised functional flow calculators to ensure consistency in the way the metrics were calculated
#'      
#'      
#'@author Kris Taniguchi-Quan, SCCWRP

############################################################################################################################

#install package to combine two geom_tile figures together (since only one gage)
#install.packages("patchwork")
install.packages("fmsb")

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
  library("fmsb")
}

#data directories (location where csv files are saved - change to your local directory for each folder)
#FFM directory where all gage FFMs and upstream tech ffms are saved
FFM_dir <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/FFM/"

#set working directory to FFM_dir
setwd(FFM_dir)

#read in FFM lookup table for plots to get official names - saved in working directory 2 folders back
metric.names <- read.csv("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/FFC_outputs/all_metric_def_list_FFMs_v2.csv")


############################################################################################################################
## Tidying actual flows (Upstream Tech) and gaged functional flow metric values, only keep gage and associated model node FFM

#read in lookup table that has Gage.ID and model_ID
#gage LU, linking gageID to COMID and other info,
gage_lu <- read.csv("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Eel_River/Lookup_Tables/Gage_PRMS_Subbasin_Lookup_comid.csv") %>% 
  mutate(dataset = "gages")

#list all files in FFM_dir
list.files.all <- list.files(FFM_dir, full.names=TRUE)

#read in gaged data
#find gage data
ind.gage <- grep("Obs_FFM", list.files.all)
#FFM columns
gage.ffm1 <- read.csv(list.files.all[ind.gage])

#find FFM columns to pivot longer on
ind.start.col <- grep("FA_Mag", names(gage.ffm1))
ind.end.col <- grep("DS_No_Flow_Dur", names(gage.ffm1))
cols.ffm <- names(gage.ffm1)[ind.start.col:ind.end.col]

#pivot longer the FFM columns 
gage.ffm <- gage.ffm1 %>% 
  #pivot longer to get an FFM column
  pivot_longer(cols = cols.ffm, 
               names_to = "FFM",
               values_to = "Value") %>% 
  #create new column of model_ID and FFM
  mutate(gage_ID_FFM = paste0(Gage.ID, " ", FFM)) %>% 
  #remove rows with NA values
  na.omit()

#only evaluate performance at gages that match POR of actual flows. all have <=4 years of data
gage.ffm <- gage.ffm[! (gage.ffm$Gage.ID %in% c(11472800, 11472900, 11472200, 11472150, 11479700)),]


#count number of ffm values per gage, we will later filter list to sites with at least 6 years of FFM data
gage.ffm.years <- gage.ffm %>% 
  group_by(Gage.ID, FFM) %>% 
  count() %>% 
  #filter(n>10) %>% 
  #create new column of model_ID and FFM
  mutate(gage_ID_FFM = paste0(Gage.ID, " ", FFM))

#read in model data
ind.model <- grep("Impaired_FFM_", list.files.all)
model.ffm <- read.csv(list.files.all[ind.model]) %>% 
  #filter to predictions at gages data only
  filter(dataset == "gage") %>% 
  #join gage ID
  left_join(gage_lu %>% select(model_ID, Gage.ID), by = c("siteID"="Gage.ID")) %>% 
  #pivot longer by FFM columns
  pivot_longer(cols = cols.ffm, 
               names_to = "FFM",
               values_to = "Value") %>% 
  #rename column
  rename(Gage.ID=siteID) %>% 
  #create new column of model_ID and FFM
  mutate(gage_ID_FFM = paste0(Gage.ID, " ", FFM)) %>% 
  #remove rows with NA values
  na.omit()

#find unique gageIDs
unique.gages <- unique(gage.ffm$Gage.ID)

#subset to only model_IDs that have gages
model.ffm.sub <- model.ffm[model.ffm$Gage.ID %in% unique.gages,]


#count number of ffm values per model node, we will later filter list to sites with at least 6 years of FFM data
model.ffm.years <- model.ffm.sub %>% 
  group_by(Gage.ID, FFM) %>% 
  count() %>% 
  #filter(n>10) %>% 
  #create new column of model_ID and FFM
  mutate(gage_ID_FFM = paste0(Gage.ID, " ", FFM))


#calculate predicted percentiles from annual FFM PRMS data --> used for step 1 below
pred.percentiles <- model.ffm.sub %>%
  group_by(Gage.ID, FFM) %>% 
  mutate(p10 = quantile(Value, probs = 0.1),
         p25 = quantile(Value, probs = 0.25),
         p50 = quantile(Value, probs = 0.5),
         p75 = quantile(Value, probs = 0.75),
         p90 = quantile(Value, probs = 0.9),
         gage_ID_FFM = paste(Gage.ID, FFM, sep=" ")) %>% 
  select(Gage.ID, COMID, FFM, p10, p25, p50, p75, p90, gage_ID_FFM) %>% 
  #remove duplicate rows 
  distinct() %>% 
  #add n number of metric values for each model_ID_FFM
  left_join(model.ffm.years, by=c("Gage.ID", "FFM", "gage_ID_FFM")) %>% 
  select(Gage.ID, COMID, FFM, p10, p25, p50, p75, p90, gage_ID_FFM)



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
eval.criteria.all <- data.frame(matrix(NA, 1, 7))
names(eval.criteria.all) <- c("Gage.ID", 'COMID', 'FFM', 'I80R_10_90', 'IQR_25_75', "n_values", "n_values_pred")

#unique FFMs to go through
unique.gageid.ffm <- unique(model.ffm.sub$gage_ID_FFM)

#to test loop, run i=1 outside of loop and run everything inside
i=2

for(i in 1:length(unique.gageid.ffm)) {
  #subset to unique.gageid.ffm i, omit rows with NA
  gage.ffm.i <- gage.ffm %>% 
    filter(gage_ID_FFM == unique.gageid.ffm[i]) %>% 
    na.omit()
  
  #if all NA values or less than 5 values, skip
  if(length(gage.ffm.i$Value) < 5){
    #skip
  }else{
    #FFM 
    FFM <- unique(gage.ffm.i$FFM)
    #gage id
    gage.id.i <- unique(gage.ffm.i$Gage.ID)
    
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
      length_n <- length(gage.ffm.i$Value)
      #check number of predicted ffm values
      length_n_pred <- model.ffm.years$n[model.ffm.years$gage_ID_FFM == unique.gageid.ffm[i]]
      #if < 5 FFM values for predicted data, set as NA
      if(length_n_pred<5){
        I80R_10_90 <- NA
        IQR_25_75 <- NA
      }else{
        sum.in.I80R <- nrow(gage.ffm.i[gage.ffm.i$Value >= pred.percentiles.i$p10 & gage.ffm.i$Value <= pred.percentiles.i$p90,])
        sum.in.IQR_25_75 <- nrow(gage.ffm.i[gage.ffm.i$Value >= pred.percentiles.i$p25 & gage.ffm.i$Value <= pred.percentiles.i$p75,])
        I80R_10_90 <- sum.in.I80R/length_n*100
        IQR_25_75 <- sum.in.IQR_25_75/length_n*100
      }
    }
    
    #create output row 
    out.row <- c(gage.ffm.i$Gage.ID[1], gage.ffm.i$COMID[1], FFM, I80R_10_90, IQR_25_75, length_n, length_n_pred)
    eval.criteria.all <- rbind(eval.criteria.all, out.row)
  }
}

#remove first NA row
eval.criteria.all <- eval.criteria.all[2:length(eval.criteria.all$Gage.ID),]

#make a copy of eval.criteria.all
eval.criteria.all.orig <- eval.criteria.all

#if n_values < 5 for gages or model, put NA for percent IQR values, not enough values to assess
#set n_values as numeric for gages
eval.criteria.all$n_values <- as.numeric(eval.criteria.all$n_values)
#set I80R_10_90 as NA if < 5 values
eval.criteria.all$I80R_10_90[eval.criteria.all$n_values < 5] <- NA
#set IQR_25_75 as NA if < 5 values
eval.criteria.all$IQR_25_75[eval.criteria.all$n_values < 5] <- NA
#set n_values as numeric for model
eval.criteria.all$n_values_pred <- as.numeric(eval.criteria.all$n_values_pred)
#set I80R_10_90 as NA if < 5 values
eval.criteria.all$I80R_10_90[eval.criteria.all$n_values_pred < 5] <- NA
#set IQR_25_75 as NA if < 5 values
eval.criteria.all$IQR_25_75[eval.criteria.all$n_values_pred < 5] <- NA

#filter evaluation criteria for csv to read
#filter out rows that have n<5 for gages and model nodes
eval.criteria.all.filter <- eval.criteria.all %>% 
  filter(n_values > 4) %>% 
  filter(n_values_pred > 4) %>% 
  #create gage_ID_FFM column
  mutate(gage_ID_FFM = paste0(Gage.ID, " ", FFM))

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
perf.criteria.FFM <- data.frame(matrix(NA, 1, 17))
names(perf.criteria.FFM) <- c("Gage.ID", "Type2", "FFM", "composite_index", "composite_index_disp", "IQR_25_75", "I80R_10_90", "PBIAS", "NSE", "mean_O_E", "R2", "PBIAS_scaled", "IQR_25_75_scaled", "I80R_10_90_scaled", "NSE_scaled", "mean_O_E_scaled", "n_annual_gage_model")


for(j in 1:(length(unique.gage.ffm))){
  #subset eval.criteria.all to unique.gage.ffm j
  eval.criteria.all.sub <- eval.criteria.all.filter %>% 
    filter(gage_ID_FFM == unique.gage.ffm[j])
  
  #subset gage data for unique.gage.ffm j
  gage.ffm.sub <- gage.ffm %>% 
    filter(gage_ID_FFM == unique.gage.ffm[j])
  
  #subset model data for unique.gage.ffm j
  model.ffm.sub.j <- model.ffm.sub %>% 
    #add Gage.ID and gage_ID_FFM from gage_lu table, rename to Gage.ID, add gage_ID_FFM, and select only columns needed
    left_join(gage_lu, by = c("Gage.ID", "COMID")) %>% 
    mutate(gage_ID_FFM = paste0(Gage.ID, " ",FFM),
           scenario = "impaired") %>% 
    #filter by unique.gage.ffm j
    filter(gage_ID_FFM == unique.gage.ffm[j]) %>% 
    select(Year, FFM, Value, scenario, COMID, Gage.ID, Type2, gage_ID_FFM) %>% 
    #rename Value to value_model
    rename("Value_model"= "Value")
  
  #combine gage and model data together into common df by year
  gage.model.FFM.join <- gage.ffm.sub %>% 
    inner_join(model.ffm.sub.j, by=c("Year", "FFM", "Gage.ID", "COMID", "gage_ID_FFM"))
  
  #determine n years in common --> should have at least 10 to move forward
  n_annual_gage_model <- length(gage.model.FFM.join$Year)
  
  #if <5 years of annual data in common, can only look at dispersion, not enough data to look at annual performance
  if(n_annual_gage_model < 5){
    #only have enough data to look at dispersion
    #IQR scaled: take absolute value from diff between calculated value and 50 divided by 50 and subtracted by 1
    IQR_25_75_scaled <- 1-(abs(as.numeric(eval.criteria.all.sub$IQR_25_75)-50)/50)
    I80R_10_90_scaled <- 1-(abs(as.numeric(eval.criteria.all.sub$I80R_10_90)-80)/80)
    
    #composite performance index (mean of all 6 criteria)
    composite_index_all <- NA
    composite_index_disp <- mean(c(I80R_10_90_scaled, IQR_25_75_scaled), na.rm=TRUE)
    
    #save all values in output for evaluation (raw, scaled, composite) --> for annual metrics, set as NA
    perf.criteria.FFM[j,] <- c(eval.criteria.all.sub$Gage.ID,  unique(model.ffm.sub.j$Type2), eval.criteria.all.sub$FFM, composite_index_all, composite_index_disp, eval.criteria.all.sub$IQR_25_75, eval.criteria.all.sub$I80R_10_90, NA, NA, NA, NA, NA, IQR_25_75_scaled, I80R_10_90_scaled, NA, NA, n_annual_gage_model)
    
  }else{
    
    ###Calculate suite of metrics for annual FFM evaluation
    #calculate O:E and mean O/E across all years
    gage.model.FFM.join <- gage.model.FFM.join %>% 
      mutate(O_E = Value/Value_model)
    
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
    perf.criteria.FFM[j,] <- c(eval.criteria.all.sub$Gage.ID, unique(model.ffm.sub.j$Type2), eval.criteria.all.sub$FFM, composite_index_all, composite_index_disp, eval.criteria.all.sub$IQR_25_75, eval.criteria.all.sub$I80R_10_90, PBIAS, NSE, mean_O_E, R2, PBIAS_scaled, IQR_25_75_scaled, I80R_10_90_scaled, NSE_scaled, mean_O_E_scaled, n_annual_gage_model)
    
  }
}

#write the overall performance table so anyone can view the results, save in 1 directory back
#dir.create("../FFM_eval/")
write.csv(perf.criteria.FFM, file="../FFM_eval/Model_performance_FFM_summary_Upstream_Actual_flows.csv")

#tidy performance table for heatmap - composite using all criteria
perf.criteria.FFM.table <- perf.criteria.FFM %>% 
  select(Gage.ID, FFM, composite_index, Type2) %>% 
  pivot_wider(names_from = FFM, values_from = composite_index)

#tidy performance table for heatmap - composite using dispersion only
perf.criteria.FFM.table.disp <- perf.criteria.FFM %>% 
  select(Gage.ID, FFM, composite_index_disp, Type2) %>% 
  pivot_wider(names_from = FFM, values_from = composite_index_disp)

#round all columns except FFM by 2 digits
perf.criteria.FFM.table[,3:21] <- round(data.frame(lapply(perf.criteria.FFM.table[,3:21],as.numeric)), 2)
perf.criteria.FFM.table.disp[,3:21] <- round(data.frame(lapply(perf.criteria.FFM.table.disp[,3:21],as.numeric)), 2)


##########
#####create heatmap of this table based on the categories for composite index all
#pivot longer table
#find ffm col names to pivot longer
ffm.col.names <- names(perf.criteria.FFM.table)[3:21]
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

#add an asterix to any validation Gage.ID
#find index of validation gages
ind.val <- grep("Validation", ffm_comp_ind_longer$Type2)
#make a copy of Type2
ffm_comp_ind_longer$Gage.ID2 <- as.character(ffm_comp_ind_longer$Gage.ID)
#set validation gages with asterix
ffm_comp_ind_longer$Gage.ID2[ind.val] <- paste0(ffm_comp_ind_longer$Gage.ID[ind.val], "*")

#add in subbasins to ffm_comp_ind_longer
ffm_comp_ind_longer2 <- ffm_comp_ind_longer %>% 
  mutate(Gage.ID = as.numeric(Gage.ID)) %>% 
  left_join(gage_lu, by = c("Gage.ID"= "Gage.ID")) %>% 
  filter(FFM %in% unique(metric.names$flow_metric))

#sort gages by reference first, NA

#heatmap using all gages

#create color gage_lu table
colors <- c("#ff7f00", "#ffff33", "#4daf4a", "#377eb8", "#984ea3", "grey")
rating <- c("poor", "satisfactory", "good", "very good", "excellent", NA)
rating_values <- c("<0.5", "0.5-0.65", "0.65-0.8", "0.81-0.9", ">0.9", NA)
rating_labels <- paste0(rating, " (", rating_values, ")")
rating_labels[6] <- NA
#subset colors to what we have
gage_lu.cols <- data.frame(cbind(colors, rating, rating_values, rating_labels))
gage_lu.cols <- gage_lu.cols[gage_lu.cols$rating %in% unique(ffm_comp_ind_longer2$rating),]

heatmap <- ggplot(ffm_comp_ind_longer2, aes(y=title_name, x = Gage.ID2)) +
  #make heatmap with geom_tile based on criteria values
  geom_tile(aes(fill = rating)) +
  
  #add the criteria values to each tile
  geom_text(aes(label = Composite_Index), color = "black") + 
  #some formatting to make things easier to read
  scale_x_discrete(position = "top") +
  scale_fill_manual(values = gage_lu.cols$colors, labels = rating_labels) +
  #scale_fill_gradient(high = “#132B43”, low = “#56B1F7”) +
  theme(legend.position = "bottom", 
        legend.box = "horizontal",
        legend.margin = margin(),
        #axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=1),
        axis.text.x = element_text(angle = 90),
        
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank()) +
  labs(y = "Functional Flow Metric ", x = "Gage ID", title = "Composite Index All") +
  facet_grid(~HUC8.Subarea, scales = "free_x", space = "free_x")

plot(heatmap)

#write heatmap as jpg
ggsave(heatmap, file="../FFM_eval/Model_performance_FFM_composite_Upstream_Actual_flows.jpg", width = 11, height = 5, dpi=300)


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
  #set all metric value columns to numeric
  mutate(across(3:23, as.numeric)) %>% 
  pivot_longer(cols = ffm.col.names,
               values_to = "Composite_Index_Disp",
               names_to = "FFM") %>% 
  left_join(metric.names, by = c("FFM" = "flow_metric")) %>% 
  mutate(Gage.ID = as.numeric(Gage.ID)) %>%
  left_join(gage_lu, by = c("Gage.ID"= "Gage.ID")) 

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

#add an asterix to any validation Gage.ID
#find index of validation gages
ind.val <- grep("Validation", ffm_comp_ind_longer$Type2.x)
#make a copy of Type2
ffm_comp_ind_longer$Gage.ID2 <- as.character(ffm_comp_ind_longer$Gage.ID)
#set validation gages with asterix
ffm_comp_ind_longer$Gage.ID2[ind.val] <- paste0(ffm_comp_ind_longer$Gage.ID[ind.val], "*")

ffm_comp_ind_longer <- ffm_comp_ind_longer %>% 
  filter(FFM %in% unique(metric.names$flow_metric))

#heatmap using all gages

#create color gage_lu table
colors <- c("#ff7f00", "#ffff33", "#4daf4a", "#377eb8", "#984ea3", "grey")
rating <- c("poor", "satisfactory", "good", "very good", "excellent", NA)
rating_values <- c("<0.5", "0.5-0.65", "0.65-0.8", "0.81-0.9", ">0.9", NA)
rating_labels <- paste0(rating, " (", rating_values, ")")
rating_labels[6] <- NA
#subset colors to what we have
gage_lu.cols <- data.frame(cbind(colors, rating, rating_values, rating_labels))
gage_lu.cols <- gage_lu.cols[gage_lu.cols$rating %in% unique(ffm_comp_ind_longer$rating),]

heatmap_disp<- ggplot(ffm_comp_ind_longer, aes(y=title_name, x = Gage.ID2)) +
  #make heatmap with geom_tile based on criteria values
  geom_tile(aes(fill = rating)) +
  
  #add the criteria values to each tile
  geom_text(aes(label = Composite_Index_Disp), color = "black") + 
  #some formatting to make things easier to read
  scale_x_discrete(position = "top") +
  scale_fill_manual(values = gage_lu.cols$colors, labels = gage_lu.cols$rating_labels) +
  #scale_fill_gradient(high = “#132B43”, low = “#56B1F7”) +
  theme(legend.position = "bottom", 
        legend.box = "horizontal",
        legend.margin = margin(),
        #axis.text.x = element_text(angle = 75, vjust = 0.5, hjust=1),
        axis.text.x = element_text(angle = 90),
        
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank()) +
  labs(y = "Functional Flow Metric ", x = "Gage ID", title = "Composite Index Dispersion") +
  facet_grid(~HUC8.Subarea, scales = "free_x", space = "free_x")

plot(heatmap_disp)

#write heatmap as jpg
ggsave(heatmap_disp, file="../FFM_eval/Model_performance_FFM_composite_dispersion_Upstream_Actual_flows.jpg", width = 11, height = 5, dpi=300)

#combine two geom_tile figures together 

#remove legend on one plot
heatmap_v2 <- heatmap + theme(legend.position = "none")

heatmap_disp2 <- heatmap_disp + theme(legend.position = "right", axis.text.y = element_blank(), axis.title.y = element_blank())

#combine plots
combined_plot <- heatmap_v2 + heatmap_disp2

plot(combined_plot)

#write heatmap as jpg
ggsave(combined_plot, file="../FFM_eval/Model_performance_FFM_composite_all_dispersion_combine_Upstream_Actual_flows.jpg", width = 7.5, height = 5, dpi=300)


##########################################################################
### Create boxplots and scatterplots for observed vs. expected by FFM (across all gages)


#add gage info to model.ffm and select certain columns
model.ffm.2 <- model.ffm %>% 
  #add Gage.ID and gage_ID_FFM from gage_lu table, rename to Gage.ID, add gage_ID_FFM, and select only columns needed
  left_join(gage_lu, by = c("Gage.ID", "COMID")) %>% 
  mutate(gage_ID_FFM = paste0(Gage.ID, " ",FFM),
         scenario = "impaired") %>% 
  select(Year, FFM, Value, scenario, COMID, Gage.ID, Type2, gage_ID_FFM) %>% 
  #rename Value to value_model
  rename("Value_model"= "Value")

#combine gage and model data together into common df by year FFM
gage.model.FFM.join <- gage.ffm %>% 
  inner_join(model.ffm.2, by=c("Year", "FFM", "Gage.ID", "COMID", "gage_ID_FFM")) %>% 
  #join to get huc9 subarea
  left_join(gage_lu %>% select(Gage.ID, HUC8.Subarea, Gage.Name), by = "Gage.ID")


#unique FFMs to loop through
unique.ffm <- unique(gage.model.FFM.join$FFM)

#Loop through each FFM and create violin plots and boxplots for each, explore faceting by gageID

for(k in 1:length(unique.ffm)){
  #subset to ffm k
  gage.model.FFM.join.sub <- gage.model.FFM.join %>% 
    filter(FFM == unique.ffm[k])
  
  #subset FFM names
  metric.names.k <- metric.names %>% 
    filter(flow_metric == unique.ffm[k])
  
  #if not a FFM (some of the new dry season metrics, skip)
  if(length(metric.names.k$flow_metric) < 1) {
    #skip
  }else{
    #plot observed vs. modeled FFMs for all sites in one plot
    #scatter plot
    scatter.all <- ggplot(gage.model.FFM.join.sub, aes(x = Value, y = Value_model, color = HUC8.Subarea)) +
      geom_point() +
      labs(
        title = paste0(metric.names.k$title_component2, metric.names.k$title_ffm),
        subtitle = "Upstream Actual",
        x = paste0("Observed ", metric.names.k$title_component2, metric.names.k$title_ffm),
        y = paste0("Predicted ", metric.names.k$title_component2, metric.names.k$title_ffm)) +
      #add 1:1 line
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
      coord_fixed(ratio = 1) +
      theme_minimal()
    
    scatter.all
    
    #save plot
    ggsave(paste0("../FFM_eval/scatter_UpstreamActual_gages_all_", unique.ffm[k], ".jpg"),        # file name
           plot = scatter.all,       # plot object (optional if it's the last plot)
           width = 6, height = 4, dpi = 300, units = "in")
    
    #scatter plot by gageID
    scatter.gage <- ggplot(gage.model.FFM.join.sub, aes(x = Value, y = Value_model, color = as.character(Gage.ID))) +
      geom_point() +
      labs(
        title = paste0(metric.names.k$title_component2, metric.names.k$title_ffm),
        subtitle = "Upstream Actual",
        x = paste0("Observed ", metric.names.k$title_component2, metric.names.k$title_ffm),
        y = paste0("Predicted ", metric.names.k$title_component2, metric.names.k$title_ffm)) +
      labs(color = "Gage ID")  + # sets the legend title for color 
      #facet_wrap(~ Gage.Name, ncol = 3, scales = "free") +
      facet_wrap(~ HUC8.Subarea, ncol = 3, scales = "free") +
      #add 1:1 line
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
      #coord_fixed(ratio = 1) +
      theme_minimal()
    
    scatter.gage
    #save plot
    ggsave(paste0("../FFM_eval/scatter_UpstreamActual_gages_facet_", unique.ffm[k], ".jpg"),        # file name
           plot = scatter.gage,       # plot object (optional if it's the last plot)
           width = 10, height = 7, dpi = 300, units = "in")
    
    
    ###violin plot (boxplot)
    #pivot data longer
    gage.model.FFM.join.sub.long <- gage.model.FFM.join.sub %>% 
      pivot_longer(cols = c(Value, Value_model),
                   names_to = "Data",
                   values_to = "value")
    #change Data names to be displayed
    gage.model.FFM.join.sub.long$Data <- gsub("Value_model", "Predicted SWB", gage.model.FFM.join.sub.long$Data)
    gage.model.FFM.join.sub.long$Data <- gsub("Value", "Gage", gage.model.FFM.join.sub.long$Data)
    
    violin.all <- ggplot(gage.model.FFM.join.sub.long, aes(x = Data, y = value, fill = Data)) +
      geom_violin(trim = TRUE, color = "black") +
      geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
      labs(title = paste0(metric.names.k$title_component2, metric.names.k$title_ffm),
           subtitle = "Upstream Actual",
           x = "Data Type",
           y = paste0(metric.names.k$title_component2, metric.names.k$title_ffm)) +
      theme_minimal()
    
    violin.all
    #save plot
    ggsave(paste0("../FFM_eval/violin_UpstreamActual_gages_all_", unique.ffm[k], ".jpg"),        # file name
           plot = violin.all,       # plot object (optional if it's the last plot)
           width = 6, height = 4, dpi = 300, units = "in")
    
    
    #facet violin plots by site
    violin.facet <- ggplot(gage.model.FFM.join.sub.long, aes(x = Data, y = value, fill = Data)) +
      geom_violin(trim = TRUE, color = "black") +
      geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
      #facet_grid(rows = vars(HUC8.Subarea), cols = vars(Gage.ID)) +
      #facet_grid(~HUC8.Subarea, scales = "free_y", space = "free_y") +
      facet_wrap(~ Gage.Name, ncol = 3, scales = "free_y") +
      labs(title = paste0(metric.names.k$title_component2, metric.names.k$title_ffm),
           subtitle = "Upstream Actual",
           x = "Data Type",
           y = paste0(metric.names.k$title_component2, metric.names.k$title_ffm)) +
      theme_minimal()
    
    violin.facet
    #save plot
    ggsave(paste0("../FFM_eval/violin_UpstreamActual_gages_facet_", unique.ffm[k], ".jpg"),        # file name
           plot = violin.facet,       # plot object (optional if it's the last plot)
           width = 10, height = 7, dpi = 300, units = "in")
    
    
  }
}


##################################################
#make comparison performance plots for Upstream Actual vs. SWB Impaired

#read in performance summary from SWB impaired
perf.criteria.FFM.SWB <- read.csv("C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/NC_impaired_flows_20250515/python_edit_SCCWRP/outputs/FFM_eval/Model_performance_FFM_summary_SWB_impaired_flow.csv")

#SWB tidy performance table for heatmap - composite using all criteria
perf.criteria.FFM.table.SWB <- perf.criteria.FFM.SWB %>% 
  select(Gage.ID, FFM, composite_index, Type2) %>% 
  pivot_wider(names_from = FFM, values_from = composite_index) %>% 
  mutate(Gage.ID = as.character(Gage.ID)) %>% 
  #only keep rows with same matching gageIDs
  semi_join(perf.criteria.FFM.table, by="Gage.ID")

#SWB tidy performance table for heatmap - composite using dispersion only
perf.criteria.FFM.table.disp.SWB <- perf.criteria.FFM.SWB %>% 
  select(Gage.ID, FFM, composite_index_disp, Type2) %>% 
  pivot_wider(names_from = FFM, values_from = composite_index_disp) %>% 
  mutate(Gage.ID = as.character(Gage.ID)) %>% 
  #only keep rows with same matching gageIDs
  semi_join(perf.criteria.FFM.table.disp, by="Gage.ID")

#summarize and use the mean score for every metric
perf.criteria.FFM.table.SWB.mean <- perf.criteria.FFM.table.SWB[,3:14] %>% 
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

perf.criteria.FFM.table.disp.SWB.mean <- perf.criteria.FFM.table.disp.SWB[,3:14] %>% 
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))


#summarize and use mean score for every metric for Upstream
perf.criteria.FFM.table.upstream.mean <- perf.criteria.FFM.table[,3:16] %>% 
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

perf.criteria.FFM.table.disp.upstream.mean <- perf.criteria.FFM.table.disp[,3:16] %>% 
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

#only keep common column metrics
common_cols <- intersect(names(perf.criteria.FFM.table.upstream.mean), names(perf.criteria.FFM.table.SWB.mean))
#overall perf
mean.perf.Upstream.common <- perf.criteria.FFM.table.upstream.mean[, common_cols, drop = FALSE]
mean.perf.SWB.common <- perf.criteria.FFM.table.SWB.mean[, common_cols, drop = FALSE]
#join together
mean.perf.overall <- data.frame(bind_rows(mean.perf.Upstream.common, mean.perf.SWB.common))
rownames(mean.perf.overall) <- c("Upstream", "SWB")

#dispersion perf
mean.perf.Upstream.common <- perf.criteria.FFM.table.disp.upstream.mean[, common_cols, drop = FALSE]
mean.perf.SWB.common <- perf.criteria.FFM.table.disp.SWB.mean[, common_cols, drop = FALSE]
#join together
mean.perf.disp <- data.frame(bind_rows(mean.perf.Upstream.common, mean.perf.SWB.common))
rownames(mean.perf.disp) <- c("Upstream", "SWB")

# fmsb needs first two rows to be the max and min
data_for_plot.overall <- rbind(
  Max = rep(1, ncol(mean.perf.overall)),  # set max for axes
  Min = rep(0, ncol(mean.perf.overall)),  # set min for axes
  mean.perf.overall
)

# Create the radar plot for dispersion
radarchart(data_for_plot.overall,
           axistype = 1,
           pcol = c("blue", "red"),
           pfcol = c(rgb(0,0,1,0.3), rgb(1,0,0,0.3)),
           plwd = 2,
           cglcol = "grey", cglty = 1,
           axislabcol = "grey", caxislabels = seq(0, 1, 0.2), cglwd = 0.8,
           vlcex = 0.8)

legend("bottom", inset = c(0, -.5), legend = c("Upstream", "SWB"),
       pch=20, col=c("blue","red"), xpd = TRUE)

# Create the radar plot for overall
radarchart(data_for_plot.overall,
           axistype = 1,
           pcol = c("blue", "red"),
           pfcol = c(rgb(0,0,1,0.3), rgb(1,0,0,0.3)),
           plwd = 2,
           cglcol = "grey", cglty = 1,
           axislabcol = "grey", caxislabels = seq(0, 1, 0.2), cglwd = 0.8,
           vlcex = 0.8)

legend("bottom", inset = c(0, -.5), legend = c("Upstream", "SWB"),
       pch=20, col=c("blue","red"), xpd = TRUE)

