### Sensitivity of Type 1 GLM curves to sub-basin clustering of delta FFM
### Upstream Tech flows are downscaled from sub-basins by drainage area, so COMIDs in the same sub-basin share
### (nearly) identical delta FFM. Sub-basin groups are approximated by identical delta values (rounded to 4 decimals).
### Input: Lara's joined bio-flow dataset (00_bugs_algae_flow_joined_by_comid.RData, object AllData)

{
  library("dplyr")
}

#Lara's joined bio-flow dataset (copy received Oct 2026; contains duplicated rows, deduplicated below)
#PC desktop
#bio_flow_file <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/Bio/TypeI_curve_inputs_Lara/00_bugs_algae_flow_joined_by_comid.RData"
#MacBook
bio_flow_file <- "/Users/kristq/Library/CloudStorage/OneDrive-SharedLibraries-OneDrive-SharedLibraries-SCCWRP2 2/Cannabis E-Flows - General/Data/RawData/Bio/TypeI_curve_inputs_Lara/00_bugs_algae_flow_joined_by_comid.RData"
load(bio_flow_file)

#NOTE: every masterid x Type x FFM appears twice in AllData (duplicate rows in Bio_Stream_Type_Land_Use_summary.csv)
dat <- AllData %>% distinct(masterid, Type, FFM, .keep_all = TRUE)

mcfadden <- function(m) 1 - m$deviance / m$null.deviance

fit_curve <- function(df) {
  m <- glm(good ~ delta_md, family = binomial, data = df)
  data.frame(n = nrow(df), n_subbasins = n_distinct(df$grp), pseudoR2 = round(mcfadden(m), 3),
             p = signif(coef(summary(m))[2, 4], 2), delta_at_50pct = round(-coef(m)[1] / coef(m)[2], 3))
}

#curves to test: index, FFM, index threshold, side of curve (-1 = depleted/earlier, 1 = augmented/later)
curves <- tribble(
  ~Index, ~FFM,            ~thresh, ~side,
  "ASCI", "Peak_10",       0.86,    -1,
  "ASCI", "DS_Mag_90",     0.86,    -1,
  "ASCI", "DS_7d_Low_Tim", 0.86,    -1,
  "CSCI", "DS_7d_Low_Tim", 0.79,    -1
)

results <- list()
for (i in 1:nrow(curves)) {
  cv <- curves[i, ]
  df <- dat %>%
    filter(Type == cv$Index, FFM == cv$FFM, delta_md * cv$side > 0) %>%
    mutate(good = as.integer(BioScore >= cv$thresh), grp = round(delta_md, 4))

  #two sub-basin groups with the most sites
  big <- df %>% count(grp, sort = TRUE) %>% slice(1:2) %>% pull(grp)

  #one observation per sub-basin (majority condition)
  df.sb <- df %>% group_by(grp) %>% summarise(good = as.integer(mean(good) >= 0.5), delta_md = first(delta_md)) %>%
    mutate(grp = delta_md)

  results[[i]] <- bind_rows(
    cbind(cv, version = "All sites", fit_curve(df)),
    cbind(cv, version = "Drop 2 largest sub-basin groups", fit_curve(df %>% filter(!grp %in% big))),
    cbind(cv, version = "One obs per sub-basin", fit_curve(df.sb)))
}
results <- bind_rows(results)
results
