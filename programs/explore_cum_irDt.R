library(tidyverse)
source("source_import_modeldata.r")

ctyDat <- read_csv("../R_export/fullIndicAll_periodicReg_irDt_Octfit_span0.4_degree2_analyzeDB_cty.csv")
stDat <- read_csv("../R_export/fullIndicAll_periodicReg_irDt_Octfit_span0.4_degree2_analyzeDB_st.csv")
regDat <- read_csv("../R_export/fullIndicAll_periodicReg_irDt_Octfit_span0.4_degree2_analyzeDB_reg.csv")
refs <- read_csv("../reference_data/state_abbreviations_FIPS_region.csv") %>%
  dplyr::mutate(region = paste0("R", Region)) %>%
  dplyr::rename(fips_st = fips) %>%
  dplyr::select(fips_st, region)

ctyDat2 <- ctyDat %>%
  dplyr::filter(season > 2 & has.epi & incl.analysis & in.season) %>%
  group_by(fips, season) %>%
  dplyr::mutate(ir.dt.cum = cumsum(ir.dt)) %>%
  dplyr::mutate(ir.dt.cumtot = max(ir.dt.cum)) %>% 
  dplyr::mutate(ir.dt.cumnorm = ir.dt.cum/ir.dt.cumtot) %>%
  ungroup %>%
  dplyr::mutate(scale.lvl = "county", loc = fips) %>%
  dplyr::mutate(fips_st = substring(fips, 1, 2)) %>%
  left_join(refs, by = c("fips_st")) %>%
  dplyr::select(season, loc, scale.lvl, fips, fips_st, region, Thu.week, ir.dt, ir.dt.cum, ir.dt.cumtot, ir.dt.cumnorm)

stDat2 <- stDat %>%
  dplyr::filter(season > 2 & has.epi & incl.analysis & in.season) %>%
  group_by(fips_st, season) %>%
  dplyr::mutate(ir.dt.cum = cumsum(ir.dt)) %>%
  dplyr::mutate(ir.dt.cumtot = max(ir.dt.cum)) %>% 
  dplyr::mutate(ir.dt.cumnorm = ir.dt.cum/ir.dt.cumtot) %>%
  ungroup %>%
  dplyr::mutate(scale.lvl = "state", loc = fips_st) %>%
  left_join(refs, by = c("fips_st")) %>%
  dplyr::select(season, loc, scale.lvl, fips_st, region, Thu.week, ir.dt, ir.dt.cum, ir.dt.cumtot, ir.dt.cumnorm)

regDat2 <- regDat %>%
  dplyr::filter(season > 2 & has.epi & incl.analysis & in.season) %>%
  group_by(region, season) %>%
  dplyr::mutate(ir.dt.cum = cumsum(ir.dt)) %>%
  dplyr::mutate(ir.dt.cumtot = max(ir.dt.cum)) %>% 
  dplyr::mutate(ir.dt.cumnorm = ir.dt.cum/ir.dt.cumtot) %>%
  ungroup %>%
  dplyr::mutate(scale.lvl = "region", loc = region) %>%
  dplyr::select(season, loc, scale.lvl, region, Thu.week, ir.dt, ir.dt.cum, ir.dt.cumtot, ir.dt.cumnorm)

fullDat <- bind_rows(ctyDat2, stDat2, regDat2) %>%
  ungroup %>%
  dplyr::mutate(scale.lvl = factor(scale.lvl, levels = c("county", "state", "region"))) %>%
  dplyr::mutate(plt.date = ifelse(lubridate::month(Thu.week)>=11, paste0("2005", substring(Thu.week, 5, 10)), paste0("2006", substring(Thu.week, 5, 10)))) %>%
  dplyr::mutate(plt.date = lubridate::as_date(plt.date)) %>%
  dplyr::mutate(plt.season = factor(season, levels = 3:9, labels = c("2002-2003", "2003-2004", "2004-2005", "2005-2006", "2006-2007", "2007-2008", "2008-2009"))) %>%
  dplyr::mutate(plt.region = factor(region, levels = paste0("R", 1:10), labels = c("Region 1:\nCT,ME,MA,\nNH,RI,VT", "Region 2:\nNJ,NY", "Region 3:\nDE,MD,PA,\nVA,WV", "Region 4:\nAL,FL,GA,\nKY,MS,NC,\nSC,TN", "Region 5:\nIL,IN,MI,\nMN,OH,WI", "Region 6:\nAR,LA,NM,\nOK,TX", "Region 7:\nIA,KS,MO,NE", "Region 8:\nCO,MT,ND,\nSD,UT,WY", "Region 9:\nAZ,CA,NV", "Region 10:\nAK,ID,OR,WA")))

colvec <- c("county" = "grey70", "state" = "black", "region" = "red")
szvec <- c("county" = .9, "state" = 1.5, "region" = 1.5)
alphvec <- c("county" = 0.05, "state" = 1, "region" = .5)

####################################
## NON-CUMULATIVE ##

plt <- ggplot(fullDat, aes(x = plt.date, y = ir.dt, group = loc)) +
  geom_line(aes(colour = scale.lvl, size = scale.lvl, alpha = scale.lvl)) +
  scale_colour_manual("Spatial Scale", values = colvec) +
  scale_size_manual("Spatial Scale", values = szvec) +
  scale_alpha_manual(values = alphvec) +
  scale_x_date(date_labels = "%b", date_breaks = "2 months") +
  scale_y_continuous("Intensity") +
  theme_bw() +
  theme(legend.position = "bottom", axis.title.x = element_blank(), text = element_text(size=10), strip.text.y = element_text(size=8, angle=360)) +
  guides(alpha = "none") +
  facet_grid(plt.region~plt.season, scales = "free_y")

ggsave("../graph_outputs/explore_cum_irDt/noncum_irDt_region_season_grid.png", plt, width = 7, height = 9)

####################################
## CUMULATIVE ##
## cumulative distribution by region
# testDat <- fullDat %>% dplyr::filter(season %in% 5:6 & region %in% c("R4", "R3"))

plt <- ggplot(fullDat, aes(x = plt.date, y = ir.dt.cumnorm, group = loc)) +
  geom_line(aes(colour = scale.lvl, size = scale.lvl, alpha = scale.lvl)) +
  scale_colour_manual("Spatial Scale", values = colvec) +
  scale_size_manual("Spatial Scale", values = szvec) +
  scale_alpha_manual(values = alphvec) +
  scale_x_date(date_labels = "%b", date_breaks = "2 months") +
  scale_y_continuous("Cumulative Intensity", breaks = c(0, 0.5, 1)) +
  theme_bw() +
  theme(legend.position = "bottom", axis.title.x = element_blank(), text = element_text(size=10), strip.text.y = element_text(size=8, angle=360)) +
  guides(alpha = "none") +
  facet_grid(plt.region~plt.season)

ggsave("../graph_outputs/explore_cum_irDt/cum_irDt_region_season_grid.png", plt, width = 6, height = 9)

############################
## choropleth with colors representing cumulative intensity (county and state version) by week ##
alldates <- unique(as.character(fullDat$Thu.week))
ctymap <- import_county_geomMap()
stmap <- import_state_geomMap()

## county-level scale
for(wk in alldates){
  wkCtyDat <- fullDat %>%
    dplyr::filter(Thu.week == as.Date(wk) & scale.lvl == "county") %>%
    dplyr::mutate(ir.dt.cumperc = ir.dt.cumnorm*100)
  season <- wkCtyDat[1,]$season

  map_cty <- ggplot() +
    geom_map(data = wkCtyDat, map = ctymap, aes(fill = ir.dt.cumperc, map_id = fips), colour = "white", size = .02) +
    geom_map(data = stmap, map = stmap, aes(x = long, y = lat, map_id = region), colour = "grey50", fill = "transparent", size = .1) +
    scale_fill_viridis_c("Cumulative Intensity (%)", limits = c(0,100), direction = -1, option = "A", na.value = "grey25") +
    expand_limits(x = ctymap$long, y = ctymap $lat) +
    theme_minimal() +
    theme(text = element_text(size = 9), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom", legend.key.size = unit(.35, "cm"), legend.margin = margin(t = 0, r = 0, b = 2, l = 0, unit = "pt"), plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.title = element_text(size = 8)) +
    ggtitle(wk)
  wd_cty <- paste0("../graph_outputs/explore_cum_irDt/S", season, "/cty/")
  dir.create(wd_cty, showWarnings = FALSE)
  fname_cty <- paste0(wd_cty, "cumPerc_irDt_cty_", wk, ".png")
  ggsave(fname_cty, map_cty, width = 4, height = 3, units = "in")
}

## state scale
for(wk in alldates){
  wkStDat <- fullDat %>%
    dplyr::filter(Thu.week == as.Date(wk) & scale.lvl == "state") %>%
    dplyr::mutate(ir.dt.cumperc = ir.dt.cumnorm*100)
  season <- wkStDat[1,]$season

  map_st <- ggplot() +
    geom_map(data = wkStDat, map = stmap, aes(fill = ir.dt.cumperc, map_id = fips_st), colour = "white", size = .02) +
    geom_map(data = stmap, map = stmap, aes(x = long, y = lat, map_id = region), colour = "grey50", fill = "transparent", size = .1) +
    scale_fill_viridis_c("Cumulative Intensity (%)", limits = c(0,100), direction = -1, option = "A", na.value = "grey25") +
    expand_limits(x = stmap$long, y = stmap$lat) +
    theme_minimal() +
    theme(text = element_text(size = 9), axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(), legend.position = "bottom", legend.key.size = unit(.35, "cm"), legend.margin = margin(t = 0, r = 0, b = 2, l = 0, unit = "pt"), plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), legend.title = element_text(size = 8)) +
    ggtitle(wk)
  wd_st <- paste0("../graph_outputs/explore_cum_irDt/S", season, "/st/")
  dir.create(wd_st, showWarnings = FALSE)
  fname_st <- paste0(wd_st, "cumPerc_irDt_st_", wk, ".png")
  ggsave(fname_st, map_st, width = 4, height = 3, units = "in")
}

############################
## speed of spatial spread (e.g., distribution of number of weeks between 0-50% cumulative intensity of the outbreak at each scale)
p1 <- 0
p2 <- 1
speedSubset <- fullDat %>% 
  dplyr::mutate(round.ir.dt.cumnorm = round(ir.dt.cumnorm, digits=1)) %>%
  dplyr::filter(round.ir.dt.cumnorm >= p1 & round.ir.dt.cumnorm <= p2)
speedSumm <- speedSubset %>%
  group_by(season, loc) %>%
  summarise(scale.lvl = first(scale.lvl), fips = first(fips), fips_st = first(fips_st), region = first(region), nweeks = n(), plt.season = first(plt.season), plt.region = first(plt.region))

plt2 <- ggplot(speedSumm, aes(x = nweeks)) +
  geom_density(aes(fill = scale.lvl), alpha = 0.4) +
  scale_fill_manual("Spatial Scale", values = colvec) +
  scale_x_continuous(paste0("Duration from ", p1*100, "-", p2*100, " Cumulative Intensity (Weeks)")) +
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size=10)) +
  facet_wrap(~plt.season, ncol=2, as.table=TRUE)

ggsave(paste0("../graph_outputs/explore_cum_irDt/duration_density_irDt_", p1*100, "-", p2*100, "_season.png"), plt2, width = 4, height = 5)

############################
## one boxplot per spatial scale showing duration of 20-80% cum intensity for each region-season pair
dur_viol_s <- ggplot(speedSumm, aes(x = plt.season, y = nweeks, group = interaction(plt.season, scale.lvl))) +
  geom_violin(aes(fill = scale.lvl), scale = "width", colour = "grey50") +
  geom_boxplot(width = 1, fill = "transparent") +
  scale_y_continuous(paste0("Duration from ", p1*100, "-", p2*100, " Cumulative Intensity (Weeks)")) +
  theme_bw() +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1), legend.position = "bottom")
ggsave(paste0("../graph_outputs/explore_cum_irDt/duration_violin_irDt_", p1*100, "-", p2*100, "_season.png"), dur_viol_s, height = 6, width = 6)

means_reg <- speedSumm %>%
  group_by(plt.season, scale.lvl, plt.region) %>%
  summarise(nweeks = mean(nweeks))
dur_viol_sr <- ggplot(speedSumm, aes(x = scale.lvl, y = nweeks, group = scale.lvl)) +
  geom_violin(aes(fill = scale.lvl), colour = "grey50", scale = "width") +
  geom_boxplot(width = .5, fill = "transparent") +
  # geom_point(data = means_reg, aes(x = plt.season, y = nweeks, group = scale.lvl, position = "dodge")) +
  scale_y_continuous(paste0("Duration from ", p1*100, "-", p2*100, " Cumulative Intensity (Weeks)")) +
  theme_bw() +
  theme(legend.title = element_blank(), axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "bottom") +
  facet_wrap(~plt.region)
ggsave(paste0("../graph_outputs/explore_cum_irDt/duration_violin_irDt_", p1*100, "-", p2*100, "_region.png"), dur_viol_sr, height = 6, width = 6)

# spread for onset and peak weeks at county and state scales