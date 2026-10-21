#' Appendix: Validation Gage Table + Model Performance Heatmaps
#' 
#' Builds, for both the Unimpaired and Actual Flows models:
#'   1. A site table of validation gages (Gage.ID, Gage.Name, COMID, HUC8.Subarea,
#'      Validation_Model, n_years)
#'   2. A "Composite Index All" heatmap (validation gages only, faceted by subarea)
#'   3. A "Composite Index Dispersion" heatmap (validation gages only, faceted by subarea)
#' 
#' All three land in a single Word document: Appendix_A_Validation_Gages_and_Heatmaps.docx
#' 
#' Inputs: the summary CSVs already produced by the model evaluation scripts
#' (Model_performance_FFM_summary_Upstream_Unimpaired_flows.csv / _Actual_flows.csv),
#' plus the gage lookup table and FFM metric name lookup table.

library(dplyr)
library(tidyr)
library(ggplot2)
library(flextable)
library(officer)

## ================================================================
## 0. SET PATHS - update these to match your local setup
## ================================================================

#Cannabis E-Flows SharePoint root (switch between computers here; all paths below are built from sp_dir)
#PC desktop
#sp_dir <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/"
#MacBook
sp_dir <- "/Users/kristq/Library/CloudStorage/OneDrive-SharedLibraries-OneDrive-SharedLibraries-SCCWRP2 2/Cannabis E-Flows - General/"

ffm_eval_dir <- paste0(sp_dir, "Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/FFM_eval")

unimpaired_summary_path <- file.path(ffm_eval_dir, "Model_performance_FFM_summary_Upstream_Unimpaired_flows.csv")
actual_summary_path     <- file.path(ffm_eval_dir, "Model_performance_FFM_summary_Upstream_Actual_flows.csv")

gage_lu_path <- paste0(sp_dir, "Data/Working/Watershed_Delineation_Tool/Modeled_Flow/Eel_River/Lookup_Tables/Gage_PRMS_Subbasin_Lookup_comid.csv")

metric_names_path <- paste0(sp_dir, "Data/Working/Watershed_Delineation_Tool/Modeled_Flow/FFC_outputs/all_metric_def_list_FFMs_v2.csv")

#repo folder (Q99 performance written by Q99_gage_validation_and_biosite_delta.R)
#PC desktop
#repo_dir <- "C:/Users/kristinet/Documents/Cannabis_SWB/PhaseIII_Impaired_flows_evaluation/"
#MacBook
repo_dir <- "/Users/kristq/Cannabis_SWB/PhaseIII_Impaired_flows_evaluation/"
q99_perf_path <- file.path(repo_dir, "FFM_alteration_range/Q99_performance_validation_gages.csv")


## ================================================================
## 1. READ IN DATA
## ================================================================

unimpaired.summary <- read.csv(unimpaired_summary_path) %>% mutate(model_label = "Unimpaired")
actual.summary     <- read.csv(actual_summary_path)     %>% mutate(model_label = "Actual")

gage_lu      <- read.csv(gage_lu_path)
metric.names <- read.csv(metric_names_path)

#add names for Ayers et al. (2024) dry-season metrics that are not in the lookup table (otherwise dropped from heatmaps)
metric.names <- bind_rows(metric.names,
  data.frame(flow_component = "Dry-season baseflow",
             flow_metric = c("DS_7d_Low_Mag", "DS_7d_Low_Tim", "DS_No_Flow_Tim", "DS_No_Flow_Dur"),
             title_name = c("Dry-Season Baseflow: 7-Day Low Flow Magnitude", "Dry-Season Baseflow: 7-Day Low Flow Timing",
                            "Dry-Season Baseflow: First Zero-Flow Day", "Dry-Season Baseflow: No-Flow Duration")) %>% 
    filter(!flow_metric %in% metric.names$flow_metric))

#add Q99 (evaluated separately with the same criteria) to each model's per-gage summary,
#at that model's testing gages only, so it appears in the heatmaps
q99.perf <- read.csv(q99_perf_path) %>% filter(Validation_gage)

add_q99 <- function(summary.df, model_name) {
  q99.rows <- q99.perf %>% 
    filter(Model == model_name) %>% 
    transmute(Gage.ID, FFM = "Q99", composite_index, composite_index_disp,
              n_annual_gage_model = n_annual, Upstream_Type = "Validation") %>% 
    left_join(summary.df %>% distinct(Gage.ID, Type2), by = "Gage.ID") %>% 
    mutate(model_label = unique(summary.df$model_label))
  bind_rows(summary.df, q99.rows)
}
unimpaired.summary <- add_q99(unimpaired.summary, "Unimpaired")
actual.summary     <- add_q99(actual.summary, "Actual")


## ================================================================
## 2. SITE / VALIDATION GAGE TABLE - updated formatting
## ================================================================

# validation (testing) gages for each model, same filters as the overall mean performance summary
# (Upstream_Type == "Validation" per Upstream Tech eval designation, >= 5 annual values, North Coast gages)
# no PRMS impaired-gage screen: pure model performance uses all Upstream Tech testing gages
unimpaired.gages <- unimpaired.summary %>%
  filter(Upstream_Type == "Validation",
         n_annual_gage_model >= 5,
         Gage.ID %in% gage_lu$Gage.ID) %>%
  group_by(Gage.ID) %>%
  summarise(n_years = max(n_annual_gage_model, na.rm = TRUE), .groups = "drop") %>%
  mutate(model = "Unimpaired")

actual.gages <- actual.summary %>%
  filter(Upstream_Type == "Validation",
         n_annual_gage_model >= 5,
         Gage.ID %in% gage_lu$Gage.ID) %>%
  group_by(Gage.ID) %>%
  summarise(n_years = max(n_annual_gage_model, na.rm = TRUE), .groups = "drop") %>%
  mutate(model = "Actual")

# keep Gage.ID as character so flextable doesn't add thousands separators
site.table.appendix <- bind_rows(unimpaired.gages, actual.gages) %>%
  group_by(Gage.ID) %>%
  summarise(
    n_years = max(n_years, na.rm = TRUE),
    Validation_Model = paste(sort(unique(model)), collapse = " & "),
    .groups = "drop"
  ) %>%
  left_join(
    gage_lu %>% select(Gage.ID, COMID, Gage.Name, HUC8.Subarea),
    by = "Gage.ID"
  ) %>%
  mutate(
    Gage.ID = as.character(Gage.ID),   # <<< prevent comma formatting
    COMID = as.character(COMID)        # <<< same fix, in case COMID is also numeric
  ) %>%
  select(Gage.ID, Gage.Name, COMID, HUC8.Subarea, Validation_Model, n_years) %>%
  arrange(HUC8.Subarea, Gage.ID)

write.csv(site.table.appendix,
          file.path(ffm_eval_dir, "Appendix_Table_Validation_Gages.csv"),
          row.names = FALSE)

# formatted Word table with human-readable column headers
ft.gages <- flextable(site.table.appendix) %>%
  set_caption(caption = "Appendix Table A1. Validation gages used to evaluate Unimpaired and Actual Flows model performance.") %>%
  set_header_labels(
    Gage.ID = "Gage ID",
    Gage.Name = "Gage Name",
    COMID = "COMID",
    HUC8.Subarea = "HUC8 Subarea",
    Validation_Model = "Validation Model",
    n_years = "N Years"
  ) %>%
  theme_booktabs() %>%
  fontsize(size = 9, part = "all") %>%
  font(fontname = "Calibri", part = "all") %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  align(j = 1:2, align = "left", part = "all") %>%
  fit_to_width(max_width = 10)

## ================================================================
## 3. COMPOSITE INDEX HEATMAPS (validation gages only)
##    one function builds both "all" and "dispersion" heatmaps for a given model
## ================================================================

build_composite_heatmap <- function(perf.criteria.FFM, value_col, title_text, file_suffix) {
  
  # pivot wide just to leverage existing column-selection logic, then back to long
  perf.wide <- perf.criteria.FFM %>%
    select(Gage.ID, FFM, all_of(value_col), Type2, Upstream_Type) %>%
    pivot_wider(names_from = FFM, values_from = all_of(value_col))
  
  ffm.col.names <- setdiff(names(perf.wide), c("Gage.ID", "Type2", "Upstream_Type"))
  ind.peak.tim <- grep("^Peak_Tim_*", ffm.col.names)
  if (length(ind.peak.tim) > 0) {
    ffm.col.names <- ffm.col.names[-ind.peak.tim]
  }
  
  ffm_long <- perf.wide %>%
    filter(Upstream_Type == "Validation") %>%          # validation gages only
    pivot_longer(cols = all_of(ffm.col.names),
                 values_to = "Composite_Index",
                 names_to = "FFM") %>%
    mutate(Composite_Index = round(as.numeric(Composite_Index), 2)) %>%   # <<< round to 0.01
    left_join(metric.names, by = c("FFM" = "flow_metric"))
  
  ffm_long$rating <- NA
  ffm_long$rating[ffm_long$Composite_Index < 0.5] <- "poor"
  ffm_long$rating[ffm_long$Composite_Index >= 0.5 & ffm_long$Composite_Index < 0.65] <- "satisfactory"
  ffm_long$rating[ffm_long$Composite_Index >= 0.65 & ffm_long$Composite_Index < 0.81] <- "good"
  ffm_long$rating[ffm_long$Composite_Index >= 0.81 & ffm_long$Composite_Index < 0.9] <- "very good"
  ffm_long$rating[ffm_long$Composite_Index >= 0.9] <- "excellent"
  ffm_long$rating <- factor(ffm_long$rating, levels = c("poor", "satisfactory", "good", "very good", "excellent", NA))
  
  ffm_long$Gage.ID2 <- as.character(ffm_long$Gage.ID)  # no asterisk needed - validation only
  
  ffm_long2 <- ffm_long %>%
    mutate(Gage.ID = as.numeric(Gage.ID)) %>%
    left_join(gage_lu, by = "Gage.ID") %>%
    filter(FFM %in% unique(metric.names$flow_metric))
  
  colors <- c("#ff7f00", "#ffff33", "#4daf4a", "#377eb8", "#984ea3", "grey")
  rating <- c("poor", "satisfactory", "good", "very good", "excellent", NA)
  rating_values <- c("<0.5", "0.5-0.65", "0.65-0.8", "0.81-0.9", ">0.9", NA)
  rating_labels <- paste0(rating, " (", rating_values, ")")
  rating_labels[6] <- NA
  gage_lu.cols <- data.frame(cbind(colors, rating, rating_values, rating_labels))
  gage_lu.cols <- gage_lu.cols[gage_lu.cols$rating %in% unique(ffm_long2$rating), ]
  
  heatmap <- ggplot(ffm_long2, aes(y = title_name, x = Gage.ID2)) +
    geom_tile(aes(fill = rating)) +
    geom_text(aes(label = Composite_Index), color = "black") +
    scale_x_discrete(position = "top") +
    scale_fill_manual(values = gage_lu.cols$colors, labels = rating_labels) +
    theme(legend.position = "bottom",
          legend.box = "horizontal",
          legend.margin = margin(),
          axis.text.x = element_text(angle = 90),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white"),
          axis.ticks = element_blank()) +
    labs(y = "Functional Flow Metric ", x = "Gage ID", title = title_text) +
    facet_grid(~HUC8.Subarea, scales = "free_x", space = "free_x")
  
  ggsave(heatmap,
         file = file.path(ffm_eval_dir, paste0("Model_performance_FFM_", file_suffix, "_validation.jpg")),
         width = 11, height = 5, dpi = 300)
  
  heatmap
}

heatmap.unimpaired.all  <- build_composite_heatmap(unimpaired.summary, "composite_index",
                                                     "Composite Index All - Unimpaired (Validation Gages)",
                                                     "composite_Upstream_Unimpaired_flows")

heatmap.unimpaired.disp <- build_composite_heatmap(unimpaired.summary, "composite_index_disp",
                                                     "Composite Index Dispersion - Unimpaired (Validation Gages)",
                                                     "composite_dispersion_Upstream_Unimpaired_flows")

heatmap.actual.all      <- build_composite_heatmap(actual.summary, "composite_index",
                                                     "Composite Index All - Actual (Validation Gages)",
                                                     "composite_Upstream_Actual_flows")

heatmap.actual.disp     <- build_composite_heatmap(actual.summary, "composite_index_disp",
                                                     "Composite Index Dispersion - Actual (Validation Gages)",
                                                     "composite_dispersion_Upstream_Actual_flows")


## ================================================================
## 4. COMBINE TABLE + HEATMAPS INTO ONE WORD APPENDIX
##    (landscape, editable table, caption format per report template)
## ================================================================

add_caption <- function(doc, text) {
  doc %>% body_add_par(text, style = "Normal")
}

doc <- read_docx() %>%
  body_set_default_section(
    prop_section(
      page_size = page_size(orient = "landscape"),
      page_margins = page_mar(top = 0.5, bottom = 0.5, left = 0.5, right = 0.5)
    )
  ) %>%
  body_add_par("Appendix A. Hydrologic Model Performance", style = "heading 1") %>%
  body_add_par("", style = "Normal") %>%
  
  add_caption("Table A1. Validation gages used to evaluate Unimpaired and Actual Flows model performance.") %>%
  body_add_flextable(ft.gages) %>%   # editable Word table, not an image
  body_add_break() %>%
  
  add_caption(paste0("Figure A1. Composite index of model performance by functional flow ",
                     "metric and validation gage for the Unimpaired Flows model, faceted by HUC8 subarea. ",
                     "Note: 10-year flood duration is not shown due to insufficient non-missing annual ",
                     "values for evaluation (see Section X).")) %>%
  body_add_gg(heatmap.unimpaired.all, width = 9, height = 4.5) %>%
  body_add_par("", style = "Normal") %>%
  
  add_caption(paste0("Figure A2. Dispersion index of model performance by functional flow ",
                     "metric and validation gage for the Unimpaired Flows model, faceted by HUC8 subarea. ",
                     "Note: 10-year flood duration is not shown due to insufficient non-missing annual ",
                     "values for evaluation (see Section X).")) %>%
  body_add_gg(heatmap.unimpaired.disp, width = 9, height = 4.5) %>%
  body_add_break() %>%
  
  add_caption(paste0("Figure A3. Composite index of model performance by functional flow ",
                     "metric and validation gage for the Actual Flows model, faceted by HUC8 subarea. ",
                     "Note: 10-year flood duration is not shown due to insufficient non-missing annual ",
                     "values for evaluation (see Section X).")) %>%
  body_add_gg(heatmap.actual.all, width = 9, height = 4.5) %>%
  body_add_par("", style = "Normal") %>%
  
  add_caption(paste0("Figure A4. Dispersion index of model performance by functional flow ",
                     "metric and validation gage for the Actual Flows model, faceted by HUC8 subarea. ",
                     "Note: 10-year flood duration is not shown due to insufficient non-missing annual ",
                     "values for evaluation (see Section X).")) %>%
  body_add_gg(heatmap.actual.disp, width = 9, height = 4.5)

print(doc, target = file.path(ffm_eval_dir, "Appendix_A_Validation_Gages_and_Heatmaps.docx"))

