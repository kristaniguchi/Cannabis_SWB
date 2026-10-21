### Q99 (annual 99th percentile of daily flow): validation at gages and delta at bioassessment sites
### Purpose: Q99 is a high-flow magnitude metric calculated every water year, so (unlike 2/5/10-yr peak magnitudes)
### it can be evaluated with the same annual performance criteria used for the FFMs (Grantham et al. 2022).
### Workflow (mirrors Upstream_tech_*_FFM_evalutation_gages.R):
###   1. Pull observed daily flow (USGS 00060, daily mean) at validation gages for WY 2002-2022  [run in RStudio]
###   2. Calculate annual Q99 for observed and modeled (Unimpaired and Actual discharge_mean) flows
###   3. Evaluate model performance: % in IQR / I80R, mean O/E, R2, PBIAS, NSE -> scaled -> composite index
###      (composite uses mean scaled O/E, i.e., equal weighting of the six criteria)
###   4. If mean composite >= 0.5 in both models: calculate delta Q99 at bioassessment COMIDs
###      (annual relative change, actual vs unimpaired, median across water years)

{
  library("dplyr")
  library("tidyr")
  library("ggplot2")
  library("dataRetrieval")
}

#############################
#### Directories (edit for your machine)

#repo folder (this script's folder)
#PC desktop
#repo_dir <- "C:/Users/kristinet/Documents/Cannabis_SWB/PhaseIII_Impaired_flows_evaluation/"
#MacBook
repo_dir <- "/Users/kristq/Cannabis_SWB/PhaseIII_Impaired_flows_evaluation/"
#Upstream Tech daily flow csvs (Unimpaired_Actual_gage_*.csv and Unimpaired_Actual_comid_*.csv)
#PC desktop
#flow_dir <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/"
#MacBook
flow_dir <- "/Users/kristq/Library/CloudStorage/OneDrive-SharedLibraries-OneDrive-SharedLibraries-SCCWRP2 2/Cannabis E-Flows - General/Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/"
eval_dir <- file.path(flow_dir, "FFM_eval")
out_dir  <- file.path(repo_dir, "FFM_alteration_range")
obs_file <- file.path(repo_dir, "Gage_Evaluation/inputs/obs_daily_validation_gages_WY2002_2022.csv")

#water years matching Upstream Tech model period of record
WY <- c(2002, 2022)
#minimum days of observed data in a water year to calculate Q99
min_days <- 330

#validation gages and which model each validates (withheld from training)
val.gages <- read.csv(file.path(eval_dir, "Appendix_Table_Validation_Gages.csv")) %>%
  mutate(Gage.ID = as.character(Gage.ID))

#############################
#### 1. Observed daily flow at validation gages (requires internet access to USGS; run once)

if (!file.exists(obs_file)) {
  #USGS Water Data API (dataRetrieval >= 2.7.17; readNWISdv is deprecated)
  obs.raw <- read_waterdata_daily(monitoring_location_id = paste0("USGS-", val.gages$Gage.ID),
                                  parameter_code = "00060", statistic_id = "00003",
                                  time = c(paste0(WY[1] - 1, "-10-01"), paste0(WY[2], "-09-30")))
  obs.raw <- data.frame(site_no = sub("USGS-", "", obs.raw$monitoring_location_id),
                        Date = as.Date(obs.raw$time), Flow = obs.raw$value)
  write.csv(obs.raw, obs_file, row.names = FALSE)
}

obs <- read.csv(obs_file, colClasses = c(site_no = "character")) %>%
  transmute(Gage.ID = site_no, Date = as.Date(Date), flow = Flow) %>%
  filter(!is.na(flow))

#############################
#### 2. Annual Q99

water_year <- function(d) as.integer(format(d, "%Y")) + as.integer(format(d, "%m") >= "10")

#observed: only water years with sufficient daily records
q99.obs <- obs %>%
  mutate(Year = water_year(Date)) %>%
  filter(Year >= WY[1], Year <= WY[2]) %>%
  group_by(Gage.ID, Year) %>%
  summarise(n_days = n(), Value = quantile(flow, 0.99), .groups = "drop") %>%
  filter(n_days >= min_days) %>%
  select(-n_days)

#modeled: discharge_mean from Upstream Tech files (both models in each file)
read_upstream <- function(file) {
  read.csv(file) %>%
    transmute(Date = as.Date(datetime),
              Unimpaired = unimpaired_discharge_mean,
              Actual = actual_discharge_mean)
}

annual_q99_model <- function(file) {
  read_upstream(file) %>%
    mutate(Year = water_year(Date)) %>%
    filter(Year >= WY[1], Year <= WY[2]) %>%
    group_by(Year) %>%
    summarise(Unimpaired = quantile(Unimpaired, 0.99, na.rm = TRUE),
              Actual = quantile(Actual, 0.99, na.rm = TRUE), .groups = "drop")
}

q99.model.gage <- bind_rows(lapply(val.gages$Gage.ID, function(g) {
  f <- file.path(flow_dir, paste0("Unimpaired_Actual_gage_", g, ".csv"))
  if (!file.exists(f) || file.size(f) < 1000) return(NULL)
  annual_q99_model(f) %>% mutate(Gage.ID = g)
})) %>%
  pivot_longer(c(Unimpaired, Actual), names_to = "Model", values_to = "Value_model")

#############################
#### 3. Performance evaluation (same criteria and scaling as FFM evaluation scripts)

evaluate_q99 <- function(obs.g, mod.g) {

  #dispersion: % of observed annual values within predicted IQR (p25-p75) and I80R (p10-p90)
  p <- quantile(mod.g$Value_model, c(0.10, 0.25, 0.75, 0.90))
  IQR_25_75 <- 100 * mean(obs.g$Value >= p[2] & obs.g$Value <= p[3])
  I80R_10_90 <- 100 * mean(obs.g$Value >= p[1] & obs.g$Value <= p[4])
  IQR_25_75_scaled <- 1 - abs(IQR_25_75 - 50) / 50
  I80R_10_90_scaled <- 1 - abs(I80R_10_90 - 80) / 80

  #annual criteria on concurrent years
  j <- inner_join(obs.g, mod.g, by = "Year")
  n_annual <- nrow(j)

  if (n_annual < 5) {
    O_E <- R2 <- PBIAS <- NSE <- NA
    mean_O_E_scaled <- R2 <- PBIAS_scaled <- NSE_scaled <- NA
  } else {
    O_E <- j$Value_model / j$Value
    O_E_scaled <- ifelse(O_E > 1, 1 / O_E, O_E)
    mean_O_E_scaled <- mean(O_E_scaled)
    R2 <- summary(lm(Value_model ~ Value, data = j))$r.squared
    #PBIAS and NSE as in hydroGOF::pbias(sim, obs) and hydroGOF::NSE(sim, obs)
    PBIAS <- 100 * sum(j$Value_model - j$Value) / sum(j$Value)
    NSE <- 1 - sum((j$Value_model - j$Value)^2) / sum((j$Value - mean(j$Value))^2)
    PBIAS_scaled <- max(0, (100 - abs(PBIAS)) / 100)
    NSE_scaled <- max(0, NSE)
    O_E <- mean(O_E)
  }

  data.frame(n_annual = n_annual,
             IQR_25_75, I80R_10_90, mean_O_E = O_E, R2, PBIAS, NSE,
             IQR_25_75_scaled, I80R_10_90_scaled, mean_O_E_scaled, PBIAS_scaled, NSE_scaled,
             composite_index = mean(c(PBIAS_scaled, I80R_10_90_scaled, IQR_25_75_scaled, NSE_scaled, mean_O_E_scaled, R2), na.rm = TRUE),
             composite_index_disp = mean(c(I80R_10_90_scaled, IQR_25_75_scaled)))
}

gage.model <- q99.model.gage %>% distinct(Gage.ID, Model) %>% filter(Gage.ID %in% q99.obs$Gage.ID)

perf.q99 <- bind_rows(lapply(seq_len(nrow(gage.model)), function(i) {
  g <- gage.model$Gage.ID[i]
  m <- gage.model$Model[i]
  obs.g <- q99.obs %>% filter(Gage.ID == g) %>% select(Year, Value)
  mod.g <- q99.model.gage %>% filter(Gage.ID == g, Model == m) %>% select(Year, Value_model)
  cbind(Gage.ID = g, Model = m, evaluate_q99(obs.g, mod.g))
})) %>%
  left_join(val.gages %>% select(Gage.ID, Gage.Name, Validation_Model), by = "Gage.ID") %>%
  #only score each model at gages withheld from its training
  mutate(Validation_gage = mapply(grepl, Model, Validation_Model))

perf.q99.val <- perf.q99 %>% filter(Validation_gage)

#overall mean performance by model (validation gages only), rating per Grantham et al. (2022)
perf.q99.summary <- perf.q99.val %>%
  group_by(Model) %>%
  summarise(n_gages = n(),
            mean_composite_index = mean(composite_index, na.rm = TRUE),
            mean_composite_index_disp = mean(composite_index_disp, na.rm = TRUE),
            median_PBIAS = median(PBIAS, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(Rating = cut(mean_composite_index, c(-Inf, 0.5, 0.65, 0.81, 0.9, Inf), right = FALSE,
                      labels = c("Poor", "Satisfactory", "Good", "Very good", "Excellent")))
perf.q99.summary

write.csv(perf.q99 %>% mutate(across(where(is.double), ~ round(.x, 3))),
          file.path(out_dir, "Q99_performance_validation_gages.csv"), row.names = FALSE)

#scatter of annual observed vs modeled Q99 at validation gages
p.q99 <- q99.obs %>%
  inner_join(q99.model.gage, by = c("Gage.ID", "Year")) %>%
  left_join(val.gages %>% select(Gage.ID, Gage.Name, Validation_Model), by = "Gage.ID") %>%
  filter(mapply(grepl, Model, Validation_Model)) %>%
  ggplot(aes(x = Value, y = Value_model, color = Gage.Name)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(size = 1.6, alpha = 0.8) +
  scale_x_log10(labels = scales::comma) + scale_y_log10(labels = scales::comma) +
  facet_wrap(~ Model) +
  labs(x = "Observed annual Q99 (cfs)", y = "Modeled annual Q99 (cfs)", color = NULL) +
  theme_classic(base_size = 9) +
  theme(legend.position = "bottom", strip.background = element_blank(), strip.text = element_text(face = "bold"))
p.q99
ggsave(p.q99, filename = file.path(out_dir, "Q99_obs_vs_model_validation_gages.png"), width = 7.5, height = 4.5, dpi = 300)

#############################
#### 4. Delta Q99 at bioassessment COMIDs (only meaningful if Q99 passes the performance screen)

pass <- all(perf.q99.summary$mean_composite_index >= 0.5)
if (!pass) message("Q99 did not meet the performance threshold (mean composite >= 0.5) in both models; delta Q99 calculated for reference only.")

biosite.files <- list.files(flow_dir, pattern = "^Unimpaired_Actual_comid_.*\\.csv$", full.names = TRUE)
biosite.files <- biosite.files[file.size(biosite.files) > 1000]

delta.q99 <- bind_rows(lapply(biosite.files, function(f) {
  annual_q99_model(f) %>%
    mutate(COMID = as.numeric(gsub("\\D", "", basename(f))))
})) %>%
  mutate(Delta = (Actual - Unimpaired) / Unimpaired) %>%
  group_by(COMID) %>%
  summarise(Median_Delta = median(Delta, na.rm = TRUE), n_years = sum(!is.na(Delta)), .groups = "drop") %>%
  mutate(FFM = "Q99")

summary(delta.q99$Median_Delta)
write.csv(delta.q99, file.path(out_dir, "NC_Biosites_median_delta_Q99.csv"), row.names = FALSE)
