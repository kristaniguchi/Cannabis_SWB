### Range of modeled flow alteration (actual vs unimpaired) across NC bioassessment sites
### Responds to Ted Grantham comment on Type 1 CSCI/ASCI tech memo:
###   "range of deviation between modeled actual and unimpaired flow across sites and across FFMs,
###    at least for the FFMs that were predicted with reasonable accuracy"
### Figure shows FFMs retained by the performance screen (mean composite >= 0.5 in both models at testing gages);
### summary table includes all FFMs with their screening status
### Inputs: Upstream Tech biosite FFMs + Q99 (from Upstream_tech_Q99_biosites.R)
### Outputs: site-level median delta FFM, summary table, appendix figure

{
  library("dplyr")
  library("tidyr")
  library("ggplot2")
  library("patchwork")
}

#data directories (change to your local directory)
#PC desktop
#FFM_dir <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/FFM/"
#MacBook
FFM_dir <- "/Users/kristq/Library/CloudStorage/OneDrive-SharedLibraries-OneDrive-SharedLibraries-SCCWRP2 2/Cannabis E-Flows - General/Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/FFM/"
#PC desktop
#out_dir <- "C:/Users/kristinet/Documents/Cannabis_SWB/PhaseIII_Impaired_flows_evaluation/FFM_alteration_range/"
#MacBook
out_dir <- "/Users/kristq/Cannabis_SWB/PhaseIII_Impaired_flows_evaluation/FFM_alteration_range/"
dir.create(out_dir, showWarnings = FALSE)

#read in biosite FFMs (unimpaired and actual), including Q99
biosite_FFMs <- read.csv(file.path(FFM_dir, "NC_Biosites_Impaired_Unimpaired_FFM_Q99_Upstream_Tech_Models.csv"))

#############################
#### FFM lookup: names, flow characteristic, screening status

#magnitude metrics are expressed as relative change from unimpaired (proportion, consistent with Table 1); all others as absolute difference
mag.ffm <- c("FA_Mag", "Wet_BFL_Mag_10", "Wet_BFL_Mag_50", "Peak_2", "Peak_5", "Peak_10", "Q99",
             "SP_Mag", "DS_Mag_50", "DS_Mag_90", "DS_7d_Low_Mag")

#FFMs retained by the performance screen (FFMs_retained_performance_screen.csv)
retained.ffm <- c("SP_Mag", "Peak_Fre_2", "DS_Mag_90", "Q99", "DS_7d_Low_Tim", "DS_Tim", "Wet_BFL_Mag_10",
                  "FA_Tim", "Wet_BFL_Mag_50", "DS_Dur_WS", "SP_Tim", "Wet_BFL_Dur", "Wet_Tim")

#FFMs evaluated but excluded due to poor hydrologic model performance (mean composite < 0.5 in either model)
excl.ffm <- c("DS_7d_Low_Mag", "DS_Mag_50", "FA_Mag", "Peak_Dur_2", "SP_ROC", "SP_Dur", "FA_Dur")

ffm.lu <- tribble(
  ~FFM,             ~FFM_name,                          ~Characteristic,
  #magnitude
  "FA_Mag",         "Fall pulse magnitude",             "Magnitude",
  "Wet_BFL_Mag_10", "Wet-season low baseflow",          "Magnitude",
  "Wet_BFL_Mag_50", "Wet-season median baseflow",       "Magnitude",
  "Peak_2",         "2-yr flood magnitude",             "Magnitude",
  "Peak_5",         "5-yr flood magnitude",             "Magnitude",
  "Peak_10",        "10-yr flood magnitude",            "Magnitude",
  "Q99",            "Annual high flow",                 "Magnitude",
  "SP_Mag",         "Spring recession magnitude",       "Magnitude",
  "DS_Mag_90",      "Dry-season high baseflow",         "Magnitude",
  "DS_Mag_50",      "Dry-season median baseflow",       "Magnitude",
  "DS_7d_Low_Mag",  "Dry-season 7-day low magnitude",   "Magnitude",
  #timing
  "FA_Tim",         "Fall pulse timing",                "Timing",
  "Wet_Tim",        "Wet-season timing",                "Timing",
  "SP_Tim",         "Spring timing",                    "Timing",
  "DS_Tim",         "Dry-season timing",                "Timing",
  "DS_7d_Low_Tim",  "Dry-season 7-day low timing",      "Timing",
  "DS_No_Flow_Tim", "First zero-flow day",              "Timing",
  #duration
  "FA_Dur",         "Fall pulse duration",              "Duration",
  "Wet_BFL_Dur",    "Wet-season duration",              "Duration",
  "Peak_Dur_2",     "2-yr flood duration",              "Duration",
  "Peak_Dur_5",     "5-yr flood duration",              "Duration",
  "Peak_Dur_10",    "10-yr flood duration",             "Duration",
  "SP_Dur",         "Spring duration",                  "Duration",
  "DS_Dur_WS",      "Dry-season duration",              "Duration",
  "DS_No_Flow_Dur", "No-flow duration",                 "Duration",
  #frequency
  "Peak_Fre_2",     "2-yr flood frequency",             "Frequency",
  "Peak_Fre_5",     "5-yr flood frequency",             "Frequency",
  "Peak_Fre_10",    "10-yr flood frequency",            "Frequency",
  #rate of change
  "SP_ROC",         "Spring rate of change",            "Rate of change"
) %>%
  mutate(Status = case_when(FFM %in% retained.ffm ~ "Retained",
                            FFM %in% excl.ffm ~ "Excluded (poor model performance)",
                            TRUE ~ "Not evaluated"),
         Units = case_when(FFM %in% mag.ffm ~ "relative change",
                           Characteristic == "Frequency" ~ "events",
                           Characteristic == "Rate of change" ~ "difference",
                           TRUE ~ "days"),
         Status = factor(Status, levels = c("Retained", "Excluded (poor model performance)", "Not evaluated")))

#############################
#### Calculate annual delta FFM, then median delta per site (COMID)

#annual delta: relative change (proportion) for magnitude metrics, absolute difference for all others
delta.annual <- biosite_FFMs %>%
  filter(!is.na(Value_Unimpaired), !is.na(Value_Actual)) %>%
  mutate(Delta = case_when(FFM %in% mag.ffm & Value_Unimpaired > 0 ~
                             (Value_Actual - Value_Unimpaired) / Value_Unimpaired,
                           FFM %in% mag.ffm ~ NA_real_,
                           TRUE ~ Value_Actual - Value_Unimpaired))

#median delta per site across water years
delta.site <- delta.annual %>%
  group_by(FFM, COMID) %>%
  summarise(Median_Delta = median(Delta, na.rm = TRUE),
            n_years = sum(!is.na(Delta)),
            .groups = "drop") %>%
  filter(!is.na(Median_Delta)) %>%
  inner_join(ffm.lu, by = "FFM")

#note: water years in the FFM record
range(delta.annual$Year)

write.csv(delta.site, file = file.path(out_dir, "NC_Biosites_median_delta_FFM.csv"), row.names = FALSE)

#############################
#### Summary table (appendix table; all FFMs with screening status)

#threshold for "meaningful" alteration: >0.10 relative change for magnitude, >7 days for timing
summary.table <- delta.site %>%
  group_by(Characteristic, FFM, FFM_name, Units, Status) %>%
  summarise(n_sites = n(),
            Median = median(Median_Delta),
            Q25 = quantile(Median_Delta, 0.25),
            Q75 = quantile(Median_Delta, 0.75),
            P05 = quantile(Median_Delta, 0.05),
            P95 = quantile(Median_Delta, 0.95),
            Min = min(Median_Delta),
            Max = max(Median_Delta),
            Pct_sites_depleted = 100 * mean(Median_Delta < 0),
            Pct_sites_augmented = 100 * mean(Median_Delta > 0),
            Pct_sites_exceed_thresh = case_when(
              first(Units) == "relative change" ~ 100 * mean(abs(Median_Delta) > 0.10),
              first(Characteristic) == "Timing" ~ 100 * mean(abs(Median_Delta) > 7),
              TRUE ~ NA_real_),
            .groups = "drop") %>%
  mutate(Characteristic = factor(Characteristic, levels = c("Magnitude", "Timing", "Duration", "Frequency", "Rate of change"))) %>%
  arrange(Status, Characteristic, match(FFM, ffm.lu$FFM)) %>%
  mutate(across(c(Median, Q25, Q75, P05, P95, Min, Max), ~ ifelse(Units == "relative change", round(.x, 2), round(.x, 1))),
         across(starts_with("Pct_"), ~ round(.x, 0)))

write.csv(summary.table, file = file.path(out_dir, "TableA_FFM_alteration_summary_biosites.csv"), row.names = FALSE)

#retained FFMs only (for memo text)
summary.table %>% filter(Status == "Retained") %>% print(n = Inf, width = Inf)

#############################
#### Figure: distribution of site-level delta FFM for retained FFMs, by flow characteristic

pt.col <- "#2a6fb0"

#x-axis limits per panel; points beyond limits are clipped to the edge and counted
panel.lims <- tribble(
  ~Characteristic, ~xmin, ~xmax, ~xlab,
  "Magnitude",     -1,    2.5,   "Relative change from unimpaired",
  "Timing",        -40,   70,    "Change from unimpaired (days; + = later)",
  "Duration",      -35,   50,    "Change from unimpaired (days)",
  "Frequency",     -1.5,  1.5,   "Change from unimpaired (events per year)"
)

plot_panel <- function(char, title) {

  lims <- panel.lims %>% filter(Characteristic == char)

  dat <- delta.site %>%
    filter(Characteristic == char, Status == "Retained") %>%
    #label with name and FFM code (codes defined in Table 1); order top to bottom as in lookup table
    mutate(FFM_label = paste0(FFM_name, " (", FFM, ")"),
           FFM_label = factor(FFM_label, levels = rev(unique(FFM_label[order(match(FFM, ffm.lu$FFM))]))),
           Delta_plot = pmin(pmax(Median_Delta, lims$xmin), lims$xmax))

  #sample size and count of points beyond x-axis limit
  lab.dat <- dat %>%
    group_by(FFM_label) %>%
    summarise(n = n(), n_over = sum(Median_Delta > lims$xmax), .groups = "drop") %>%
    mutate(n_lab = paste0("n=", n),
           over_lab = ifelse(n_over > 0, paste0(n_over, " > ", lims$xmax), NA))

  ggplot(dat, aes(x = Median_Delta, y = FFM_label)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey30", linewidth = 0.4) +
    geom_jitter(aes(x = Delta_plot), height = 0.18, width = 0, size = 1.1, alpha = 0.45, stroke = 0, color = pt.col) +
    geom_boxplot(outlier.shape = NA, fill = NA, width = 0.55, linewidth = 0.5, color = "grey20") +
    geom_text(data = lab.dat, aes(x = Inf, y = FFM_label, label = n_lab),
              inherit.aes = FALSE, hjust = -0.15, size = 2.4, color = "grey35") +
    geom_text(data = lab.dat %>% filter(!is.na(over_lab)), aes(x = lims$xmax, y = FFM_label, label = over_lab),
              inherit.aes = FALSE, hjust = 1, vjust = -1.1, size = 2.2, color = "grey35") +
    coord_cartesian(xlim = c(lims$xmin, lims$xmax), clip = "off") +
    labs(title = title, x = lims$xlab, y = NULL) +
    theme_classic(base_size = 9) +
    theme(plot.title = element_text(face = "bold", size = 10),
          panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
          axis.line = element_line(color = "grey60", linewidth = 0.3),
          axis.ticks = element_line(color = "grey60", linewidth = 0.3),
          plot.margin = margin(5, 30, 5, 5))
}

p.mag <- plot_panel("Magnitude", "A. Magnitude")
p.tim <- plot_panel("Timing", "B. Timing")
p.dur <- plot_panel("Duration", "C. Duration")
p.fre <- plot_panel("Frequency", "D. Frequency")

#panel heights proportional to number of retained FFMs in each
p.all <- (p.mag / p.tim / p.dur / p.fre) +
  plot_layout(heights = c(5, 4, 3, 1))

p.all

ggsave(p.all, filename = file.path(out_dir, "FigA5_FFM_alteration_range_biosites.png"),
       width = 7.5, height = 9, dpi = 300)
