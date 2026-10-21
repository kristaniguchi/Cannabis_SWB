### Descriptive comparison of observed vs modeled peak flow magnitudes (2-, 5-, 10-yr) at validation gages
### Peak magnitudes are calculated once per period of record (2002-2022), so formal performance
### criteria (NSE, R2, PBIAS, etc.) cannot be computed. Instead we report percent error at each gage
### for the Upstream Tech Unimpaired and Actual Flows models, and the model-to-model difference
### (Actual vs Unimpaired), which is the same quantity used as delta FFM at bioassessment sites.

{
  library("dplyr")
  library("tidyr")
  library("ggplot2")
}

#data directories (change to your local directory)
#PC desktop
#FFM_dir <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/FFM/"
#MacBook
FFM_dir <- "/Users/kristq/Library/CloudStorage/OneDrive-SharedLibraries-OneDrive-SharedLibraries-SCCWRP2 2/Cannabis E-Flows - General/Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/FFM/"
#PC desktop
#eval_dir <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/FFM_eval/"
#MacBook
eval_dir <- "/Users/kristq/Library/CloudStorage/OneDrive-SharedLibraries-OneDrive-SharedLibraries-SCCWRP2 2/Cannabis E-Flows - General/Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/FFM_eval/"
#PC desktop
#out_dir <- "C:/Users/kristinet/Documents/Cannabis_SWB/PhaseIII_Impaired_flows_evaluation/FFM_alteration_range/"
#MacBook
out_dir <- "/Users/kristq/Cannabis_SWB/PhaseIII_Impaired_flows_evaluation/FFM_alteration_range/"
dir.create(out_dir, showWarnings = FALSE)

peak.ffm <- c("Peak_2", "Peak_5", "Peak_10")

#validation gage lookup (gage ID, name, COMID, which model it validates)
val.gages <- read.csv(file.path(eval_dir, "Appendix_Table_Validation_Gages.csv"))

#observed FFMs at gages (POR 2002-2022); SF Eel at Leggett is in a separate file
obs <- bind_rows(read.csv(file.path(FFM_dir, "Obs_FFM_validation_gages_upstreamtech_POR.csv")),
                 read.csv(file.path(FFM_dir, "Obs_FFM_validation_gages_upstreamtech_POR_unimpaired_2.csv"))) %>%
  filter(Gage.ID %in% val.gages$Gage.ID) %>%
  #peak magnitudes are one value per POR (repeated each year), take the unique value per gage
  group_by(Gage.ID) %>%
  summarise(across(all_of(peak.ffm), ~ first(na.omit(.x))), .groups = "drop") %>%
  pivot_longer(all_of(peak.ffm), names_to = "FFM", values_to = "Observed")

#modeled FFMs at gage COMIDs
read_model <- function(file, model.name) {
  read.csv(file.path(FFM_dir, file)) %>%
    #join on gage ID (siteID) rather than COMID: Mad R nr Arcata is COMID 8316811 in the model output
    #but 8317377 in the appendix validation table
    filter(dataset == "gage", siteID %in% val.gages$Gage.ID) %>%
    rename(Gage.ID = siteID) %>%
    group_by(Gage.ID) %>%
    summarise(across(all_of(peak.ffm), ~ first(na.omit(.x))), .groups = "drop") %>%
    pivot_longer(all_of(peak.ffm), names_to = "FFM", values_to = model.name)
}
unimp <- read_model("Unimpaired_FFM_upstream_tech_comid_gages.csv", "Unimpaired")
actual <- read_model("Impaired_FFM_upstream_tech_comid_gages.csv", "Actual")

#combine and calculate percent error and model-to-model relative difference
peak.comp <- val.gages %>%
  select(Gage.ID, Gage.Name, COMID, HUC8.Subarea, Validation_Model) %>%
  left_join(obs, by = "Gage.ID") %>%
  left_join(unimp, by = c("Gage.ID", "FFM")) %>%
  left_join(actual, by = c("Gage.ID", "FFM")) %>%
  mutate(Pct_Error_Unimpaired = 100 * (Unimpaired - Observed) / Observed,
         Pct_Error_Actual     = 100 * (Actual - Observed) / Observed,
         #same quantity as delta FFM (relative change) used for the flow-ecology curves
         Rel_Diff_Actual_vs_Unimpaired = (Actual - Unimpaired) / Unimpaired,
         FFM = factor(FFM, levels = peak.ffm)) %>%
  arrange(FFM, desc(Observed))

#note: gages with NA modeled values do not have model output at that COMID
peak.comp %>% filter(is.na(Actual) | is.na(Unimpaired)) %>% distinct(Gage.ID, Gage.Name)

write.csv(peak.comp %>% mutate(across(where(is.double), ~ round(.x, 2))),
          file = file.path(out_dir, "Peak_magnitude_obs_vs_model_validation_gages.csv"), row.names = FALSE)

#summary across gages
peak.comp %>%
  group_by(FFM) %>%
  summarise(n_gages = sum(!is.na(Actual) | !is.na(Unimpaired)),
            median_pct_error_unimpaired = median(Pct_Error_Unimpaired, na.rm = TRUE),
            median_pct_error_actual = median(Pct_Error_Actual, na.rm = TRUE),
            range_rel_diff_actual_unimp = paste(round(range(Rel_Diff_Actual_vs_Unimpaired, na.rm = TRUE), 2), collapse = " to "))

#############################
#### Figure: observed vs modeled peak magnitude by gage

plot.dat <- peak.comp %>%
  mutate(Gage_label = paste0(Gage.Name, " (", Gage.ID, ")"),
         Gage_label = factor(Gage_label, levels = unique(Gage_label[order(Observed)]))) %>%
  pivot_longer(c(Observed, Unimpaired, Actual), names_to = "Source", values_to = "Flow_cfs") %>%
  filter(!is.na(Flow_cfs)) %>%
  mutate(Source = factor(Source, levels = c("Observed", "Unimpaired", "Actual")),
         FFM = recode(FFM, Peak_2 = "2-yr flood magnitude", Peak_5 = "5-yr flood magnitude", Peak_10 = "10-yr flood magnitude"))

p <- ggplot(plot.dat, aes(x = Flow_cfs, y = Gage_label, color = Source, shape = Source)) +
  geom_line(aes(group = Gage_label), color = "grey80", linewidth = 0.5) +
  geom_point(size = 2.4, stroke = 0.9) +
  scale_x_log10(labels = scales::comma) +
  scale_color_manual(values = c(Observed = "black", Unimpaired = "#2a6fb0", Actual = "#d1495b")) +
  scale_shape_manual(values = c(Observed = 16, Unimpaired = 1, Actual = 2)) +
  facet_wrap(~ FFM, ncol = 1) +
  labs(x = "Peak flow magnitude (cfs, daily mean, log scale)", y = NULL, color = NULL, shape = NULL) +
  theme_classic(base_size = 9) +
  theme(legend.position = "top",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0, size = 9.5),
        panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
        axis.line = element_line(color = "grey60", linewidth = 0.3))
p

ggsave(p, filename = file.path(out_dir, "FigA6_Peak_magnitude_obs_vs_model_validation_gages.png"),
       width = 7, height = 8, dpi = 300)
