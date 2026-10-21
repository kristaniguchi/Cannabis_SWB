#### Overall mean model performance (Upstream Tech Unimpaired and Actual models) at North Coast testing gages
#Standalone version of the final section of Upstream_tech_actual_flows_FFM_evalutation_gages - Copy.R
#Run this after both evaluation scripts have written their per-gage summary files
#(Model_performance_FFM_summary_Upstream_Unimpaired_flows.csv / _Actual_flows.csv); avoids rerunning the slow plotting loops.
#Outputs: Model_performance_FFM_overall_actual_unimpaired_validation_Gages_NC_mean.csv, spider plot and dot plot (png) in FFM_eval/

{
  library(tidyverse)

}

#Cannabis E-Flows SharePoint root
#PC desktop
#sp_dir <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/"
#MacBook
sp_dir <- "/Users/kristq/Library/CloudStorage/OneDrive-SharedLibraries-OneDrive-SharedLibraries-SCCWRP2 2/Cannabis E-Flows - General/"

eval_dir <- paste0(sp_dir, "Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/FFM_eval/")

#repo folder (Q99 performance is written here by Q99_gage_validation_and_biosite_delta.R)
#PC desktop
#repo_dir <- "C:/Users/kristinet/Documents/Cannabis_SWB/PhaseIII_Impaired_flows_evaluation/"
#MacBook
repo_dir <- "/Users/kristq/Cannabis_SWB/PhaseIII_Impaired_flows_evaluation/"

#North Coast gage lookup (used to keep NC gages only)
gage_lu <- read.csv(paste0(sp_dir, "Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Eel_River/Lookup_Tables/Gage_PRMS_Subbasin_Lookup_comid.csv")) %>% 
  mutate(dataset = "gages")

#FFM names lookup (metric name for output table)
metric.names <- read.csv(paste0(sp_dir, "Data/Working/Watershed_Delineation_Tool/Modeled_Flow/FFC_outputs/all_metric_def_list_FFMs_v2.csv"))

#########summary of overall performance for all metrics upstream tech models of unimpaired and actual flows at validation/testing gages only

#read in unimpaired flows performance all gages 
unimpaired.perf.all <- read.csv(paste0(sp_dir, "Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/FFM_eval/Model_performance_FFM_summary_Upstream_Unimpaired_flows.csv")) %>% 
  filter(Upstream_Type == "Validation") %>% 
  #exclude gage metrics < 5 years of metrics calculated
  filter(n_annual_gage_model >= 5,
         #keep only north coast gages used in PRMS model
         Gage.ID %in% gage_lu$Gage.ID) %>% 
  #note: no PRMS impaired-gage screen here; pure model performance uses all Upstream Tech testing (eval) gages
  #(the PRMS Validation_Notes screen is only for apples-to-apples PRMS vs Upstream comparisons above)
  #group by FFM and find mean for composite_index and composite_index_disp
  group_by(FFM) %>%
  summarise(
    mean_composite_index = mean(composite_index, na.rm = TRUE),
    mean_composite_index_disp = mean(composite_index_disp, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(model = "Upstream Tech Unimpaired Flows")

#read in actual flows performance all gages
actual.perf.all <- read.csv(paste0(sp_dir, "Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/FFM_eval/Model_performance_FFM_summary_Upstream_Actual_flows.csv")) %>% 
  filter(Upstream_Type == "Validation") %>% 
  #exclude gage metrics < 5 years of metrics calculated
  filter(n_annual_gage_model >= 5,
         #keep only north coast gages used in PRMS model
         Gage.ID %in% gage_lu$Gage.ID) %>% 
  #group by FFM and find mean for composite_index and composite_index_disp
  group_by(FFM) %>%
  summarise(
    mean_composite_index = mean(composite_index, na.rm = TRUE),
    mean_composite_index_disp = mean(composite_index_disp, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(model = "Upstream Tech Actual Flows")

#Q99 (annual 99th percentile of daily flow): evaluated separately (Q99_gage_validation_and_biosite_delta.R)
#with the same criteria at the same North Coast testing gages
q99.perf.all <- read.csv(paste0(repo_dir, "FFM_alteration_range/Q99_performance_validation_gages.csv")) %>% 
  filter(Validation_gage) %>% 
  group_by(Model) %>% 
  summarise(mean_composite_index = mean(composite_index, na.rm = TRUE),
            mean_composite_index_disp = mean(composite_index_disp, na.rm = TRUE),
            .groups = "drop") %>% 
  mutate(FFM = "Q99",
         model = paste0("Upstream Tech ", Model, " Flows")) %>% 
  select(FFM, mean_composite_index, mean_composite_index_disp, model)

#combine the overall performance summary tables (FFMs + Q99)
summary.performance.all <- unimpaired.perf.all %>% 
  bind_rows(actual.perf.all) %>% 
  bind_rows(q99.perf.all)


write.csv(summary.performance.all, file=paste0(sp_dir, "Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/FFM_eval/Model_performance_FFM_overall_actual_unimpaired_validation_Gages_NC_mean.csv"))

#############################
#### Overall performance figures (FFMs + Q99): spider plot and dot plot
#retained FFMs (mean composite >= 0.5 in both models) are shown in black; excluded FFMs in grey
#FFMs that could not be evaluated (no composite score) are not shown

#short labels follow the FFM codes defined in Table 1 of the memo
perf.plot <- summary.performance.all %>% 
  mutate(Model = ifelse(grepl("Unimpaired", model), "Unimpaired", "Actual"),
         Model = factor(Model, levels = c("Unimpaired", "Actual"))) %>% 
  filter(!is.na(mean_composite_index), is.finite(mean_composite_index)) %>% 
  group_by(FFM) %>% 
  filter(n() == 2) %>%                                   #evaluated in both models
  mutate(min_composite = min(mean_composite_index),
         Status = ifelse(min_composite >= 0.5, "Retained", "Excluded (< 0.5)")) %>% 
  ungroup()

#order spokes by functional flow component so related metrics sit together
ffm.order <- c("FA_Mag", "FA_Tim", "FA_Dur",
               "Wet_BFL_Mag_10", "Wet_BFL_Mag_50", "Wet_Tim", "Wet_BFL_Dur",
               "Peak_Dur_2", "Peak_Fre_2", "Q99",
               "SP_Mag", "SP_Tim", "SP_Dur", "SP_ROC",
               "DS_Mag_50", "DS_Mag_90", "DS_7d_Low_Mag", "DS_Tim", "DS_7d_Low_Tim", "DS_Dur_WS")
ffm.order <- c(intersect(ffm.order, unique(perf.plot$FFM)), setdiff(unique(perf.plot$FFM), ffm.order))

model.cols <- c(Unimpaired = "#2a6fb0", Actual = "#d1495b")

## ---- spider plot (built in ggplot so labels and the 0.5 threshold can be styled) ----
n.ffm <- length(ffm.order)
spokes <- tibble(FFM = ffm.order,
                 angle = pi/2 - 2*pi*(seq_len(n.ffm) - 1)/n.ffm) %>%   #clockwise from top
  left_join(perf.plot %>% distinct(FFM, Status), by = "FFM")

to_xy <- function(r, angle) tibble(x = r*cos(angle), y = r*sin(angle))

#grid rings (0.2 ... 1) and the 0.5 threshold ring
ring <- function(r) { a <- seq(0, 2*pi, length.out = 200); tibble(x = r*cos(a), y = r*sin(a), r = r) }
grid.rings <- bind_rows(lapply(c(0.2, 0.4, 0.6, 0.8, 1), ring))
poor.zone  <- ring(0.5)

poly <- perf.plot %>% 
  left_join(spokes %>% select(FFM, angle), by = "FFM") %>% 
  mutate(x = mean_composite_index*cos(angle), y = mean_composite_index*sin(angle)) %>% 
  arrange(Model, match(FFM, ffm.order))
poly.closed <- poly %>% group_by(Model) %>% group_modify(~ bind_rows(.x, .x[1, ])) %>% ungroup()

lab <- spokes %>% 
  mutate(x = 1.13*cos(angle), y = 1.13*sin(angle),
         hjust = ifelse(abs(cos(angle)) < 0.15, 0.5, ifelse(cos(angle) > 0, 0, 1)))

p.spider <- ggplot() +
  #shaded "poor" zone inside the 0.5 threshold
  geom_polygon(data = poor.zone, aes(x, y), fill = "grey92", color = NA) +
  geom_path(data = grid.rings, aes(x, y, group = r), color = "grey80", linewidth = 0.3) +
  geom_segment(data = spokes, aes(x = 0, y = 0, xend = cos(angle), yend = sin(angle)), color = "grey80", linewidth = 0.3) +
  geom_path(data = poor.zone, aes(x, y), color = "grey25", linetype = "dashed", linewidth = 0.6) +
  geom_polygon(data = poly.closed, aes(x, y, group = Model, fill = Model), alpha = 0.12, color = NA) +
  geom_path(data = poly.closed, aes(x, y, group = Model, color = Model), linewidth = 0.9) +
  geom_point(data = poly, aes(x, y, color = Model), size = 1.8) +
  #ring values
  annotate("text", x = 0.02, y = c(0.2, 0.4, 0.6, 0.8, 1), label = c("0.2", "0.4", "0.6", "0.8", "1.0"),
           size = 2.6, color = "grey45", hjust = 0, vjust = -0.3) +
  #spoke labels: retained in black bold, excluded in grey italic
  geom_text(data = lab, aes(x, y, label = FFM, hjust = hjust),
            color = ifelse(lab$Status == "Retained", "black", "grey55"),
            fontface = ifelse(lab$Status == "Retained", "bold", "italic"), size = 3.1) +
  scale_color_manual(values = model.cols, name = NULL) +
  scale_fill_manual(values = model.cols, name = NULL) +
  coord_equal(xlim = c(-1.55, 1.55), ylim = c(-1.3, 1.3), clip = "off") +
  labs(caption = "Mean composite index at North Coast testing gages. Bold labels: retained (>= 0.5 in both models); grey italic: excluded.\nShaded area and dashed ring: below the 0.5 (satisfactory) threshold. FFM codes are defined in Table 1.") +
  theme_void(base_size = 10) +
  theme(legend.position = "top", plot.caption = element_text(hjust = 0, size = 8, color = "grey30"),
        plot.margin = margin(10, 10, 10, 10))
p.spider

ggsave(p.spider, filename = paste0(eval_dir, "spider_plot_FFM_perf_Upstream_mean_overall.png"),
       width = 8, height = 8, dpi = 300, bg = "white")

## ---- dot plot alternative (easier to read than the spider plot) ----
dot.dat <- perf.plot %>% 
  left_join(metric.names %>% select(FFM = flow_metric, Metric = title_name), by = "FFM") %>% 
  mutate(Metric = coalesce(Metric, recode(FFM,
                                          DS_7d_Low_Mag = "Dry-Season Baseflow: 7-Day Low Flow Magnitude",
                                          DS_7d_Low_Tim = "Dry-Season Baseflow: 7-Day Low Flow Timing",
                                          .default = FFM)),
         Label = ifelse(grepl(paste0("(", FFM, ")"), Metric, fixed = TRUE), Metric, paste0(Metric, " (", FFM, ")")),
         Label = reorder(Label, min_composite))

p.dot <- ggplot(dot.dat, aes(x = mean_composite_index, y = Label)) +
  annotate("rect", xmin = -Inf, xmax = 0.5, ymin = -Inf, ymax = Inf, fill = "grey93") +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey25") +
  geom_line(aes(group = FFM), color = "grey70", linewidth = 0.5) +
  geom_point(aes(color = Model), size = 2.6) +
  scale_color_manual(values = model.cols, name = NULL) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(x = "Mean composite performance index (testing gages)", y = NULL,
       caption = "Shaded area: below the 0.5 (satisfactory) threshold.\nFFMs below 0.5 in either model were excluded (grey labels).") +
  theme_classic(base_size = 10) +
  theme(legend.position = "top",
        axis.text.y = element_text(color = ifelse(levels(dot.dat$Label) %in% dot.dat$Label[dot.dat$Status == "Retained"], "black", "grey55")),
        panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
        plot.caption = element_text(hjust = 0, size = 8, color = "grey30"),
        plot.caption.position = "plot")
p.dot

ggsave(p.dot, filename = paste0(eval_dir, "dotplot_FFM_perf_Upstream_mean_overall.png"),
       width = 8, height = 6, dpi = 300, bg = "white")




#############################
#FFMs retained for flow-ecology analysis: mean composite >= 0.5 in BOTH models at testing gages
#(FFMs that could not be evaluated have no composite score and are not retained)
ffm.retained <- summary.performance.all %>% 
  mutate(model = ifelse(grepl("Unimpaired", model), "Unimpaired", "Actual")) %>% 
  select(FFM, model, mean_composite_index) %>% 
  pivot_wider(names_from = model, values_from = mean_composite_index) %>% 
  filter(!is.na(Unimpaired), !is.na(Actual),
         Unimpaired >= 0.5, Actual >= 0.5) %>% 
  mutate(min_composite = pmin(Unimpaired, Actual)) %>% 
  left_join(metric.names %>% select(FFM = flow_metric, Metric = title_name), by = "FFM") %>% 
  #names for Ayers et al. (2024) dry-season metrics not in the lookup table
  mutate(Metric = coalesce(Metric, recode(FFM,
                                          DS_7d_Low_Mag = "Dry-Season Baseflow: 7-Day Low Flow Magnitude",
                                          DS_7d_Low_Tim = "Dry-Season Baseflow: 7-Day Low Flow Timing",
                                          DS_No_Flow_Tim = "Dry-Season Baseflow: First Zero-Flow Day",
                                          DS_No_Flow_Dur = "Dry-Season Baseflow: No-Flow Duration",
                                          Q99 = "Peak Flow: Q99 (99th percentile of daily flow)",
                                          .default = NA_character_))) %>% 
  select(Metric, FFM, Unimpaired, Actual, min_composite) %>% 
  arrange(min_composite) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 2)))

print(ffm.retained, n = Inf)

write.csv(ffm.retained, file = paste0(eval_dir, "FFMs_retained_performance_screen.csv"), row.names = FALSE)

