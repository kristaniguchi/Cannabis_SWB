### Identify bioassessment COMIDs located downstream of USGS gages
### Rationale: the Upstream Tech Actual Flows model replaces routed predictions with USGS observations
### at gaged confluences (Read et al. 2025), whereas the Unimpaired model does not. Biosites downstream
### of a gage may therefore show large positive delta in peak magnitude because the Actual model carries
### observed flood peaks while the Unimpaired model underpredicts them.
### Inputs: NHDPlusV2 flowlines with VAAs (North Coast), gage COMIDs from Upstream Tech gage FFM file,
###         site-level median delta FFM (from FFM_alteration_range_biosites.R)

{
  library("dplyr")
  library("tidyr")
  library("ggplot2")
  library("foreign")
}

#data directories (change to your local directory)
NHD_dbf <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/NHD/NC_NHDPlusV2_CAComplete_W_VAAs.dbf"
FFM_dir <- "C:/Users/kristinet/SCCWRP/Cannabis E-Flows - General/Data/RawData/Upstream_Impaired_Flows_NC/Actual Flows/FFM/"
out_dir <- "C:/Users/kristinet/Documents/Cannabis_SWB/PhaseIII_Impaired_flows_evaluation/FFM_alteration_range/"

#NHD flowline attributes (network topology via Hydroseq / DnHydroseq)
nhd <- read.dbf(NHD_dbf, as.is = TRUE) %>%
  select(ComID, GNIS_Name, Hydroseq, DnHydroseq, TotDASqKM) %>%
  distinct(ComID, .keep_all = TRUE)

#gage COMIDs used in the Upstream Tech gage runs (siteID = USGS gage ID)
gages <- read.csv(file.path(FFM_dir, "Impaired_FFM_upstream_tech_comid_gages.csv")) %>%
  filter(dataset == "gage") %>%
  distinct(Gage.ID = siteID, COMID) %>%
  left_join(nhd %>% select(ComID, Gage_DA_km2 = TotDASqKM, Gage_River = GNIS_Name), by = c("COMID" = "ComID"))

#validation (test) gages - these may have been withheld from routing in the Actual model
val.gages <- c(11481200, 11475000, 11477000, 11478500, 11480390, 11481000, 11482500, 11475800)
gages <- gages %>% mutate(Validation_gage = Gage.ID %in% val.gages)

#site-level median delta FFM for biosites
delta.site <- read.csv(file.path(out_dir, "NC_Biosites_median_delta_FFM.csv"))

#############################
#### Trace downstream from each gage

hs.to.dn <- setNames(nhd$DnHydroseq, nhd$Hydroseq)
hs.to.comid <- setNames(nhd$ComID, nhd$Hydroseq)
comid.to.hs <- setNames(nhd$Hydroseq, nhd$ComID)

trace_downstream <- function(comid) {
  hs <- comid.to.hs[as.character(comid)]
  out <- c()
  while (!is.na(hs) && hs != 0 && as.character(hs) %in% names(hs.to.comid)) {
    out <- c(out, hs.to.comid[as.character(hs)])
    hs <- hs.to.dn[as.character(hs)]
  }
  unname(out)
}

#all COMIDs downstream of (and including) each gage
gage.dn <- gages %>%
  rowwise() %>%
  mutate(dn_comids = list(trace_downstream(COMID))) %>%
  ungroup() %>%
  unnest(dn_comids) %>%
  rename(Biosite_COMID = dn_comids)

#for each biosite: nearest upstream gage = gage with largest drainage area upstream of site
biosites <- delta.site %>% distinct(COMID) %>%
  left_join(nhd %>% select(ComID, Site_DA_km2 = TotDASqKM, Site_River = GNIS_Name), by = c("COMID" = "ComID"))

site.gage <- biosites %>%
  left_join(gage.dn, by = c("COMID" = "Biosite_COMID")) %>%
  group_by(COMID) %>%
  #all upstream gages, and the largest one (covers the most of the site's drainage)
  summarise(Site_River = first(Site_River),
            Site_DA_km2 = first(Site_DA_km2),
            n_upstream_gages = sum(!is.na(Gage.ID)),
            n_upstream_gages_nonval = sum(!is.na(Gage.ID) & !Validation_gage),
            Upstream_gage = ifelse(n_upstream_gages > 0, Gage.ID[which.max(Gage_DA_km2)], NA),
            Gage_DA_frac = ifelse(n_upstream_gages > 0, max(Gage_DA_km2, na.rm = TRUE) / first(Site_DA_km2), 0),
            .groups = "drop") %>%
  mutate(Gaged_upstream = case_when(n_upstream_gages == 0 ~ "No gage upstream",
                                    Gage_DA_frac >= 0.5 ~ "Gage upstream (>=50% of drainage)",
                                    TRUE ~ "Gage upstream (<50% of drainage)"),
         Gaged_upstream = factor(Gaged_upstream, levels = c("No gage upstream", "Gage upstream (<50% of drainage)",
                                                            "Gage upstream (>=50% of drainage)")))

write.csv(site.gage, file = file.path(out_dir, "Biosite_COMIDs_upstream_gage_lookup.csv"), row.names = FALSE)

#############################
#### Compare delta in magnitude metrics by gage position

mag.ffm <- c("Peak_2", "Peak_5", "Peak_10", "Wet_BFL_Mag_50", "SP_Mag", "DS_Mag_90")

comp <- delta.site %>%
  filter(FFM %in% mag.ffm) %>%
  left_join(site.gage, by = "COMID")

#summary: median delta and share of sites augmented by gage position
comp.summary <- comp %>%
  group_by(FFM, Gaged_upstream) %>%
  summarise(n = n(),
            median_delta = round(median(Median_Delta), 2),
            pct_augmented = round(100 * mean(Median_Delta > 0)),
            pct_gt_0.5 = round(100 * mean(Median_Delta > 0.5)),
            .groups = "drop") %>%
  arrange(match(FFM, mag.ffm), Gaged_upstream)
comp.summary

write.csv(comp.summary, file = file.path(out_dir, "Delta_magnitude_by_upstream_gage.csv"), row.names = FALSE)

#Wilcoxon test: delta Peak_10 at sites with vs without a gage upstream
with(comp %>% filter(FFM == "Peak_10"), wilcox.test(Median_Delta ~ (n_upstream_gages > 0)))

#figure
p <- comp %>%
  mutate(FFM = factor(FFM, levels = mag.ffm)) %>%
  ggplot(aes(x = Gaged_upstream, y = Median_Delta, color = Gaged_upstream)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_boxplot(outlier.shape = NA, fill = NA, width = 0.55) +
  geom_jitter(width = 0.15, height = 0, size = 1.2, alpha = 0.5) +
  facet_wrap(~ FFM, nrow = 2) +
  scale_color_manual(values = c("grey45", "#e0a030", "#d1495b")) +
  scale_x_discrete(labels = c("None", "<50%", ">=50%")) +
  labs(x = "USGS gage upstream (share of site drainage area)", y = "Relative change from unimpaired") +
  theme_classic(base_size = 9) +
  theme(legend.position = "none", strip.background = element_blank(), strip.text = element_text(face = "bold"))
p

ggsave(p, filename = file.path(out_dir, "Delta_magnitude_by_upstream_gage.png"), width = 7.5, height = 5, dpi = 300)
